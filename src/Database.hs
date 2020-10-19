{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import Database.Persist ()
import Database.Persist.Sql (BackendKey (SqlBackendKey))
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import DatabaseTypes (Frequency, ItemStatus)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
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
