module Capability where

import Data.Text (Text)
import Data.Time (Day)
import Database (ActionedBudgetItem, BudgetDefinition, Key)
import Database.Persist (Entity)

class Monad m => CanInitialize m where
  init :: m ()

class Monad m => CanQueryData m where
  getBudgetDefinitions :: m [Entity BudgetDefinition]
  getActionedBudgetItems :: Maybe Day -> Maybe Day -> m [Entity ActionedBudgetItem]

class Monad m => CanOutput m where
  output :: Text -> m ()

class Monad m => CanAddBudgetDefinition m where
  addBudgetDefinition :: BudgetDefinition -> m (Key BudgetDefinition)