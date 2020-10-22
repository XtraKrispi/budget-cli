{-# LANGUAGE RecordWildCards #-}

module Tui where

import Brick
  ( App (..),
    AttrMap,
    BrickEvent (..),
    CursorLocation,
    EventM,
    Next,
    Widget,
    attrMap,
    continue,
    defaultMain,
    halt,
    str,
  )
import Graphics.Vty (Event (..), Key (..), defAttr)

data Name = Name
  deriving (Eq, Ord)

data TuiState = TuiState

runTuiApp :: IO ()
runTuiApp = defaultMain tuiApp TuiState >> pure ()

tuiApp :: App TuiState () Name
tuiApp =
  App
    { appDraw = render,
      appChooseCursor = selectCursor,
      appHandleEvent = handleEvent,
      appStartEvent = initialEvent,
      appAttrMap = attributes
    }

render :: TuiState -> [Widget Name]
render _ = [str "Hello"]

selectCursor :: TuiState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
selectCursor _ _ = Nothing

handleEvent :: TuiState -> BrickEvent Name () -> EventM Name (Next TuiState)
handleEvent s (VtyEvent (EvKey key _))
  | key == KEsc = halt s
  | otherwise = continue s
handleEvent s _ = continue s

initialEvent :: TuiState -> EventM Name TuiState
initialEvent _ = pure TuiState

attributes :: TuiState -> AttrMap
attributes _ = attrMap defAttr []