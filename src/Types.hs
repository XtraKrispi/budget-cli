{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Database (BudgetDefinition)
import Database.Persist (Key)
import Database.Persist.Class ()
import Database.Persist.Sql (SqlBackend)
import DatabaseTypes (Frequency)

data Config = Config
  { _configDatabaseName :: Text,
    _configSqlPool :: Pool SqlBackend
  }

data BudgetItem = BudgetItem
  { budgetItemDefinitionId :: !(Key BudgetDefinition),
    budgetItemDescription :: !Text,
    budgetItemDate :: !Day,
    budgetItemAmount :: !Double
  }

data CliCommand
  = Init
  | ListDefinitions
  | ListUpcoming Day (Maybe Day)
  | AddBudgetDefinition Text Double Frequency Day
  | Interactive
