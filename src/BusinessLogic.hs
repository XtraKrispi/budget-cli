{-# LANGUAGE RecordWildCards #-}
module BusinessLogic where

import           Types
import           Data.Time.Calendar
import           Database

extrapolate :: Frequency -> Day -> [Day]
extrapolate OneTime  d = pure d
extrapolate Monthly  d = []
extrapolate BiWeekly d = []

mkBudgetItem :: BudgetDefinition -> Day -> BudgetItem
mkBudgetItem BudgetDefinition {..} day = BudgetItem { .. }
 where
  budgetItemAmount      = budgetDefinitionAmount
  budgetItemDate        = day
  budgetItemDescription = budgetDefinitionDescription

getItems :: BudgetDefinition -> [BudgetItem]
getItems bd@BudgetDefinition {..} =
  mkBudgetItem bd
    <$> extrapolate budgetDefinitionFrequency budgetDefinitionStartDate
