{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Options.Applicative
import           Data.Time.Calendar
import           Data.Time.Format.ISO8601
import           Capability                     ( CanInitialize
                                                , CanQueryData
                                                , CanOutput
                                                , CanAddBudgetDefinition
                                                )
import qualified Capability                    as C
import           Database.Persist.Sqlite
import           Data.Text
import           Control.Monad.Logger
import           Types
import           App
import           Control.Monad.Trans.Reader
import           Control.Exception
import qualified Data.Text.IO                  as TIO
import           Database
import           Database.Persist.Types
import           Text.Printf
data CliCommand =
  Init
  | ListDefinitions
  | ListUpcoming Day (Maybe Day)
  | AddBudgetDefinition Text Double Frequency Day

cliParser :: Parser CliCommand
cliParser = subparser
  (  command
      "init"
      (info (pure Init <**> helper)
            (progDesc "Initialize a new budget database here")
      )
  <> command
       "list"
       (info (pure ListDefinitions <**> helper)
             (progDesc "List all budget definitions")
       )
  <> command
       "upcoming"
       (info (listUpcomingParser <**> helper)
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
          (long "description" <> short 'd' <> help "The description of the item"
          )
    <*> option (maybeReader parseAmount)
               (long "amount" <> short 'a' <> help "The amount for the item")
    <*> option
          (maybeReader parseFrequency)
          (long "frequency" <> short 'f' <> help
            "The frequency. Valid values are: one-time, monthly, bi-weekly"
          )
    <*> option dateReader
               (long "start-date" <> short 's' <> help "The start date")
 where
  parseFrequency :: String -> Maybe Frequency
  parseFrequency t | pack t == oneTimeText  = pure OneTime
                   | pack t == monthlyText  = pure Monthly
                   | pack t == biWeeklyText = pure BiWeekly
                   | otherwise              = Nothing
  parseAmount :: String -> Maybe Double
  parseAmount = undefined

run
  :: (CanInitialize m, CanQueryData m, CanOutput m, CanAddBudgetDefinition m)
  => CliCommand
  -> m ()
run Init = C.init >> C.output "Database initialization/migration completed"
run ListDefinitions = do
  defs <- C.getBudgetDefinitions
  mapM_ (C.output . budgetDefinitionLine) defs
 where
  budgetDefinitionLine (Entity k BudgetDefinition {..}) =
    let e = pack $ show (fromSqlKey k)
    in  e <> ".\t" <> budgetDefinitionDescription <> "\t" <> pack
          (printf "$%d.2" budgetDefinitionAmount)

run (ListUpcoming s e                           ) = pure ()
run (AddBudgetDefinition description amt freq sd) = do
  k <- C.addBudgetDefinition BudgetDefinition { .. }
  C.output "Item added successfully"
 where
  budgetDefinitionAmount      = amt
  budgetDefinitionDescription = description
  budgetDefinitionFrequency   = freq
  budgetDefinitionIsDeleted   = False
  budgetDefinitionStartDate   = sd

databaseName :: Text
databaseName = "budget.sqlite3"

main :: IO ()
main = do
  catch process (\(e :: SomeException) -> putStrLn "An error occurred")
 where
  opts = info
    (cliParser <**> helper)
    (fullDesc <> progDesc "Budget, in CLI form" <> header
      "budget - cli for budgets"
    )
  process = do
    cmd  <- execParser opts
    pool <- runStdoutLoggingT (createSqlitePool databaseName 10)
    runReaderT (unApp (run cmd)) $ Config databaseName pool
