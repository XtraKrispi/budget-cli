{-# LANGUAGE RecordWildCards #-}

module BusinessLogic where

import Data.Time.Calendar (Day, addDays, addGregorianMonthsClip)
import Database (ActionedBudgetItem (..), BudgetDefinition (..))
import Database.Persist (Entity (..))
import DatabaseTypes (Frequency (..))
import Types (BudgetItem (..))

extrapolate :: Frequency -> Day -> [Day]
extrapolate OneTime d = pure d
extrapolate Monthly d = (`addGregorianMonthsClip` d) <$> [0, 1 ..]
extrapolate BiWeekly d = (\i -> addDays (14 * i) d) <$> [0, 1 ..]

mkBudgetItem :: Entity BudgetDefinition -> Day -> BudgetItem
mkBudgetItem (Entity k BudgetDefinition {..}) day = BudgetItem {..}
  where
    budgetItemDefinitionId = k
    budgetItemAmount = budgetDefinitionAmount
    budgetItemDate = day
    budgetItemDescription = budgetDefinitionDescription

getItems :: Entity BudgetDefinition -> [BudgetItem]
getItems bd@(Entity _ BudgetDefinition {..}) =
  mkBudgetItem bd
    <$> extrapolate budgetDefinitionFrequency budgetDefinitionStartDate

convertActionedItem :: ActionedBudgetItem -> BudgetItem
convertActionedItem ActionedBudgetItem {..} = BudgetItem {..}
  where
    budgetItemDefinitionId = actionedBudgetItemDefinitionId
    budgetItemAmount = actionedBudgetItemAmount
    budgetItemDate = actionedBudgetItemDate
    budgetItemDescription = actionedBudgetItemDescription
