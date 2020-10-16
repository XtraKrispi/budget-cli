{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Database where

import           Database.Persist
import           Database.Persist.TH
import           Data.Text
import           Data.Time.Calendar
import           Database.Persist.Class
import           Database.Persist.Sql
import           Types


share [ mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  BudgetDefinition
    description Text
    startDate Day
    amount Double
    frequency Frequency
    isDeleted Bool
    deriving Show
  ActionedBudgetItem
    definitionId BudgetDefinitionId
    description Text
    date Day
    amount Double
    actionedOn Day
    status ItemStatus
|]
