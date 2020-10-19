{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import App (App (unApp))
import BusinessLogic (convertActionedItem, getItems)
import Capability
  ( CanAddBudgetDefinition,
    CanInitialize,
    CanOutput,
    CanQueryData,
  )
import qualified Capability as C
import Control.Exception (SomeException, catch)
import Control.Monad (join)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Foldable (Foldable (fold))
import Data.Text (Text, pack)
import Data.Time.Calendar (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Database
  ( BudgetDefinition
      ( BudgetDefinition,
        budgetDefinitionAmount,
        budgetDefinitionDescription,
        budgetDefinitionFrequency,
        budgetDefinitionIsDeleted,
        budgetDefinitionStartDate
      ),
  )
import Database.Persist (Entity (entityVal), Key, ToBackendKey)
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Sqlite
  ( Entity (Entity),
    createSqlitePool,
    fromSqlKey,
  )
import Database.Persist.Types ()
import DatabaseTypes
  ( Frequency (..),
    biWeeklyText,
    monthlyText,
    oneTimeText,
  )
import Options.Applicative
  ( Parser,
    ReadM,
    command,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    maybeReader,
    option,
    optional,
    progDesc,
    short,
    strOption,
    subparser,
    (<**>),
  )
import Text.Printf (printf)
import Types
  ( BudgetItem (..),
    Config (Config),
  )

data CliCommand
  = Init
  | ListDefinitions
  | ListUpcoming Day (Maybe Day)
  | AddBudgetDefinition Text Double Frequency Day

cliParser :: Parser CliCommand
cliParser =
  subparser
    ( command
        "init"
        ( info
            (pure Init <**> helper)
            (progDesc "Initialize a new budget database here")
        )
        <> command
          "list"
          ( info
              (pure ListDefinitions <**> helper)
              (progDesc "List all budget definitions")
          )
        <> command
          "upcoming"
          ( info
              (listUpcomingParser <**> helper)
              (progDesc "List upcoming budget items")
          )
        <> command
          "add"
          (info (addParser <**> helper) (progDesc "Add a new budget item"))
    )

dateReader :: ReadM Day
dateReader = maybeReader iso8601ParseM

listUpcomingParser :: Parser CliCommand
listUpcomingParser = ListUpcoming <$> startDate <*> optional endDate
  where
    startDate :: Parser Day
    startDate =
      option dateReader (long "start-date" <> short 's' <> help "The start date")

    endDate :: Parser Day
    endDate =
      option dateReader (long "end-date" <> short 'e' <> help "The end date")

addParser :: Parser CliCommand
addParser =
  AddBudgetDefinition
    <$> strOption
      ( long "description" <> short 'd' <> help "The description of the item"
      )
    <*> option
      (maybeReader parseAmount)
      (long "amount" <> short 'a' <> help "The amount for the item")
    <*> option
      (maybeReader parseFrequency)
      ( long "frequency" <> short 'f'
          <> help
            "The frequency. Valid values are: one-time, monthly, bi-weekly"
      )
    <*> option
      dateReader
      (long "start-date" <> short 's' <> help "The start date")
  where
    parseFrequency :: String -> Maybe Frequency
    parseFrequency t
      | pack t == oneTimeText = pure OneTime
      | pack t == monthlyText = pure Monthly
      | pack t == biWeeklyText = pure BiWeekly
      | otherwise = Nothing
    parseAmount :: String -> Maybe Double
    parseAmount s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

convertKey :: ToBackendKey SqlBackend a => Key a -> Text
convertKey = pack . show . fromSqlKey

run ::
  (CanInitialize m, CanQueryData m, CanOutput m, CanAddBudgetDefinition m) =>
  CliCommand ->
  m ()
run Init = C.init >> C.output "Database initialization/migration completed"
run ListDefinitions = do
  defs <- C.getBudgetDefinitions
  mapM_ (C.output . budgetDefinitionLine) defs
  where
    budgetDefinitionLine (Entity k BudgetDefinition {..}) =
      fold
        [ convertKey k,
          ". ",
          budgetDefinitionDescription,
          " ",
          pack
            (printf "$%.2f" budgetDefinitionAmount)
        ]
run (ListUpcoming s e) = do
  defs <- join . map getItems <$> C.getBudgetDefinitions
  actioned <- map (convertActionedItem . entityVal) <$> C.getActionedBudgetItems (Just s) e
  let complete =
        filter
          (\d -> not (any (\a -> (budgetItemDate d == budgetItemDate a) && (budgetItemDefinitionId d == budgetItemDefinitionId a)) actioned))
          defs
  undefined
run (AddBudgetDefinition description amt freq sd) = do
  k <- C.addBudgetDefinition BudgetDefinition {..}
  C.output $ "Item added successfully, ID: " <> convertKey k
  where
    budgetDefinitionAmount = amt
    budgetDefinitionDescription = description
    budgetDefinitionFrequency = freq
    budgetDefinitionIsDeleted = False
    budgetDefinitionStartDate = sd

databaseName :: Text
databaseName = "budget.sqlite3"

main :: IO ()
main = do
  catch process (\(e :: SomeException) -> putStrLn $ "An error occurred: " <> show e)
  where
    opts =
      info
        (cliParser <**> helper)
        ( fullDesc <> progDesc "Budget, in CLI form"
            <> header
              "budget - cli for budgets"
        )
    process = do
      cmd <- execParser opts
      pool <- runStdoutLoggingT (createSqlitePool databaseName 10)
      runReaderT (unApp (run cmd)) $ Config databaseName pool
