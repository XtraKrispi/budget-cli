{-# LANGUAGE OverloadedStrings #-}

module DatabaseTypes where

import Data.Text (Text)
import Database.Persist (PersistField (..), PersistValue (PersistText), SqlType (SqlString))
import Database.Persist.Sql (PersistFieldSql (..))

data Frequency = OneTime | Monthly | BiWeekly
  deriving (Show)

oneTimeText :: Text
oneTimeText = "one-time"

monthlyText :: Text
monthlyText = "monthly"

biWeeklyText :: Text
biWeeklyText = "bi-weekly"

instance PersistField Frequency where
  toPersistValue OneTime = PersistText oneTimeText
  toPersistValue Monthly = PersistText monthlyText
  toPersistValue BiWeekly = PersistText biWeeklyText
  fromPersistValue (PersistText t)
    | t == oneTimeText = Right OneTime
    | t == monthlyText = Right Monthly
    | t == biWeeklyText = Right BiWeekly
  fromPersistValue _ = Left "Invalid data for Frequency"

instance PersistFieldSql Frequency where
  sqlType _ = SqlString

data ItemStatus = Paid | Skipped

paidText :: Text
paidText = "paid"

skippedText :: Text
skippedText = "skipped"

instance PersistField ItemStatus where
  toPersistValue Paid = PersistText paidText
  toPersistValue Skipped = PersistText skippedText

  fromPersistValue (PersistText t)
    | t == paidText = Right Paid
    | t == skippedText = Right Skipped
  fromPersistValue _ = Left "Invalid item status"

instance PersistFieldSql ItemStatus where
  sqlType _ = SqlString
