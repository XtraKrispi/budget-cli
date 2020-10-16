{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
module App where

import           Control.Monad.Trans.Reader
                                         hiding ( asks )

import           Types

import           Capability
import           Control.Monad.Reader
import           Database.Persist.Sqlite
import           Database
import           Data.Time.Calendar
import           Database.Persist
import           Control.Monad.IO.Unlift
import           Data.Pool
import qualified Data.Text.IO                  as TIO
import           Data.Text

newtype App a = App { unApp :: ReaderT Config IO a }
    deriving (Functor, Monad, Applicative, MonadReader Config, MonadIO, MonadUnliftIO)

runDb :: ReaderT SqlBackend App a -> App a
runDb action = asks _configSqlPool >>= runSqlPool action

instance CanInitialize App where
  init = runDb (runMigration migrateAll)

instance CanQueryData App where
  getBudgetDefinitions :: App [Entity BudgetDefinition]
  getBudgetDefinitions =
    runDb $ selectList [BudgetDefinitionIsDeleted ==. False] []
  getActionedBudgetItems :: App [Entity ActionedBudgetItem]
  getActionedBudgetItems = runDb $ selectList [] []

instance CanOutput App where
  output :: Text -> App ()
  output = liftIO . TIO.putStrLn

instance CanAddBudgetDefinition App where
  addBudgetDefinition :: BudgetDefinition -> App (Key BudgetDefinition)
  addBudgetDefinition = runDb . insert
