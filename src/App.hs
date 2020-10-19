{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App where

import BusinessLogic (convertActionedItem, getItems)
import Capability
  ( CanAddBudgetDefinition (..),
    CanInitialize (..),
    CanOutput (..),
    CanQueryData (..),
  )
import qualified Capability as C
import Control.Monad (join)
import Control.Monad.IO.Unlift
  ( MonadUnliftIO,
    UnliftIO (UnliftIO),
  )
import Control.Monad.Reader (MonadIO (..), MonadReader, asks)
import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import Data.Foldable (Foldable (fold))
import Data.Maybe (maybeToList)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Data.Time (Day)
import Data.Time.Format.ISO8601 (iso8601Show)
import Database
  ( ActionedBudgetItem,
    BudgetDefinition (..),
    EntityField (ActionedBudgetItemDate, BudgetDefinitionIsDeleted),
    migrateAll,
  )
import Database.Persist (Entity (Entity, entityVal), PersistEntity (Key), PersistStoreWrite (insert), SelectOpt (Asc), ToBackendKey, selectList, (<=.), (==.), (>=.))
import Database.Persist.Sql
  ( SqlBackend,
    fromSqlKey,
    runMigration,
    runSqlPool,
  )
import Text.Printf (printf)
import Types (BudgetItem (..), CliCommand (..), Config (_configSqlPool))

formatDate :: Day -> Text
formatDate =
  pack . iso8601Show

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
  let fn d a = budgetItemDate d == budgetItemDate a && budgetItemDefinitionId d == budgetItemDefinitionId a
      outstandingItems =
        filter
          (\d -> not (any (fn d) actioned))
          defs
  mapM_ (C.output . budgetItemLine) outstandingItems
  where
    budgetItemLine BudgetItem {..} =
      fold
        [ formatDate budgetItemDate,
          ": ",
          budgetItemDescription,
          " ",
          ( pack
              (printf "$%.2f" budgetItemAmount)
          )
        ]
run (AddBudgetDefinition description amt freq sd) = do
  k <- C.addBudgetDefinition BudgetDefinition {..}
  C.output $ "Item added successfully, ID: " <> convertKey k
  where
    budgetDefinitionAmount = amt
    budgetDefinitionDescription = description
    budgetDefinitionFrequency = freq
    budgetDefinitionIsDeleted = False
    budgetDefinitionStartDate = sd
