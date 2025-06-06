module GameData (GameState(..), Condition(..), noCondition, Choice(..), Node(..)) where

data GameState = GameState {
  przygotowanie     :: Int,
  obecnyPrzedmiot   :: String,
  dzień             :: Int,
  czas              :: Int,
  punkty            :: Int,
  zadowolenie       :: Int,
  ada               :: Int,
  c                 :: Int,
  oop               :: Int,
  matematyka        :: Int,
  charyzma          :: Int
}

instance Show GameState where
  show gs = unlines
    [ "=== Game State ==="
    , "Dzień:            " ++ show (dzień gs)
    , "Czas:             " ++ show (czas gs)
    , "Przygotowanie:    " ++ show (przygotowanie gs)
    , "Punkty:           " ++ show (punkty gs)
    , "Zadowolenie:      " ++ show (zadowolenie gs)
    , "Umiejętności:"
    , "  Ada:            " ++ show (ada gs)
    , "  C:              " ++ show (c gs)
    , "  OOP:            " ++ show (oop gs)
    , "  Matematyka:     " ++ show (matematyka gs)
    , "  Charyzma:       " ++ show (charyzma gs)
    ]

data Condition = Condition {
  check       :: GameState -> Bool,
  explanation :: String
}

noCondition = Condition {
  check = \gs -> True,
  explanation = "No conditions apply."
}

data Choice = Choice {
  description :: String,
  targetNode  :: Node,
  condition   :: Condition,
  modifyState :: GameState -> GameState
}

data Node = Node {
  nodeText :: String,
  options  :: [Choice]
}

