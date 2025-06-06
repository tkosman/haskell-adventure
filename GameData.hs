module GameData (GameState(..), Condition(..), noCondition, Choice(..), Node(..)) where

data GameState = GameState {
  hunger::Int,
  drunk::Bool,
  experience::Int
} deriving (Show)

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

