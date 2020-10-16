module Capability where

import           Types
import           Database
import           Data.Time.Calendar
import           Data.Text
import           Database.Persist

class Monad m => CanInitialize m where
    init :: m ()

class Monad m => CanQueryData m where
    getBudgetDefinitions :: m [Entity BudgetDefinition]
    getActionedBudgetItems :: m [Entity ActionedBudgetItem]

class Monad m => CanOutput m where
    output :: Text -> m ()

class Monad m => CanAddBudgetDefinition m where
    addBudgetDefinition :: BudgetDefinition -> m (Key BudgetDefinition)
