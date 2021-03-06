{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import App (App (unApp), run)
import Control.Exception (SomeException, catch)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Text (Text, pack)
import Data.Time.Calendar (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Database.Persist.Sqlite (createSqlitePool)
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
    flag',
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
    (<|>),
  )
import Tui (runTuiApp)
import Types (CliCommand (..), Config (..))

cliParser :: Parser CliCommand
cliParser =
  interactiveParser
    <|> subparser
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

interactiveParser :: Parser CliCommand
interactiveParser =
  Interactive
    <$ flag'
      False
      ( long "interactive" <> short 'i' <> help "Start in interactive mode"
      )

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
      config <- Config databaseName <$> runStdoutLoggingT (createSqlitePool databaseName 10)
      case cmd of
        Interactive -> runTuiApp
        _ -> runReaderT (unApp (run cmd)) config
