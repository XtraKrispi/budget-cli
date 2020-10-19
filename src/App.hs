{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module App where

import Capability
  ( CanAddBudgetDefinition (..),
    CanInitialize (..),
    CanOutput (..),
    CanQueryData (..),
  )
import Control.Monad.IO.Unlift
  ( MonadUnliftIO,
    UnliftIO (UnliftIO),
  )
import Control.Monad.Reader (MonadIO (..), MonadReader, asks)
import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Time (Day)
import Database
  ( ActionedBudgetItem,
    BudgetDefinition,
    EntityField (ActionedBudgetItemDate, BudgetDefinitionIsDeleted),
    migrateAll,
  )
import Database.Persist (Entity, PersistEntity (Key), PersistStoreWrite (insert), SelectOpt (Asc), selectList, (<=.), (==.), (>=.))
import Database.Persist.Sql
  ( SqlBackend,
    runMigration,
    runSqlPool,
  )
import Types (Config (_configSqlPool))

newtype App a = App {unApp :: ReaderT Config IO a}
  deriving (Functor, Monad, Applicative, MonadReader Config, MonadIO, MonadUnliftIO)

runDb :: ReaderT SqlBackend App a -> App a
runDb action = asks _configSqlPool >>= runSqlPool action

instance CanInitialize App where
  init = runDb (runMigration migrateAll)

instance CanQueryData App where
  getBudgetDefinitions :: App [Entity BudgetDefinition]
  getBudgetDefinitions =
    runDb $ selectList [BudgetDefinitionIsDeleted ==. False] []
  getActionedBudgetItems :: Maybe Day -> Maybe Day -> App [Entity ActionedBudgetItem]
  getActionedBudgetItems mS mE =
    let f op = ((ActionedBudgetItemDate `op`) <$>) . maybeToList
     in runDb $ selectList (f (>=.) mS <> f (<=.) mE) [Asc ActionedBudgetItemDate]

instance CanOutput App where
  output :: Text -> App ()
  output = liftIO . TIO.putStrLn

instance CanAddBudgetDefinition App where
  addBudgetDefinition :: BudgetDefinition -> App (Key BudgetDefinition)
  addBudgetDefinition = runDb . insert
