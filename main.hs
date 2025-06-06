-- import Control.Monad (when)
-- import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import System.Info (os)
import System.Process (callCommand)

-- | Game state: each field is an Int, stored in a map
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

-- | A modification to state
type GameStateModifier = GameState -> GameState

-- -- | One choice the player can take
data Choice = Choice {
  description :: String,
  targetNode  :: Node,
  condition   :: Condition,
  modifyState :: GameStateModifier
}

data Node = Node {
  nodeText :: String,
  options  :: [Choice]
}


-- | Apply a choice
applyChoice :: GameState -> Choice -> IO ()
applyChoice gs choice = play (modifyState choice gs) (targetNode choice)

-- clearScreen :: IO ()
clearScreen = callCommand $ if os == "mingw32" then "cls" else "clear"


validIndex :: [a] -> Int -> Bool
validIndex list index = 
  index >= 0 &&
  index < length list

getValidChoice :: GameState -> Node -> IO Choice
getValidChoice gameState node = do
  putStr "> "
  hFlush stdout
  input <- getLine
  let allChoices = options node
  
  -- reads input and parses it as a pair of Int and rest of the text
  case reads input :: [(Int, String)] of
    [(choiceIndex, "")] ->
      if validIndex allChoices choiceIndex && 
        check (condition (allChoices !! choiceIndex)) gameState
        then return (allChoices !! choiceIndex)
        else do
          putStrLn "Invalid or unavailable choice. Try again."
          getValidChoice gameState node
    _ -> do
      putStrLn "Please enter a valid number."
      getValidChoice gameState node

-- | Game loop with styled output and inline locked choices
play :: GameState -> Node -> IO ()
play gs node = do
  clearScreen
  putStrLn "#########################################"
  putStrLn (show gs)
  putStrLn "#########################################"
  putStrLn $ "\n " ++ nodeText node ++ "\n"

  let allChoices = options node

  mapM_ (
    \(index, choice) -> do
      if (check (condition choice) gs)
        then putStrLn $ show index ++ ". " ++ description choice
        else putStrLn $ show index ++ ". 🔒 " ++ description choice ++ " [" ++ (explanation (condition choice)) ++ "]"
    ) (zip [0..] allChoices)

  choice <- getValidChoice gs node
  applyChoice gs choice

-- | Sample nodes
gameOver :: Node
gameOver = Node "Game Over. Thanks for playing!" []

-- Forward declarations using Haskell laziness
startNode, uniNode, breakfastNode :: Node

startNode = Node "You are in your room! What do you want to do?" [
  Choice {
    description = "Go to university",
    targetNode  = uniNode,
    condition   = noCondition,
    modifyState = (\gs -> gs { experience = experience gs + 10})
  },
  Choice "Eat breakfast"    breakfastNode (Condition (\gs -> experience gs > 5) "experience > 5") id,
  Choice "Go back to sleep" gameOver      noCondition                      id
  ]

uniNode = Node "You're at the university. You learned a lot!"
  [ Choice "Go back home" startNode noCondition (\gs -> gs { experience = experience gs + 5}) ]

breakfastNode = Node "Yum! That was a good breakfast."
  [ Choice "Go back to room" startNode noCondition (\gs -> gs { hunger = experience gs - 5}) ]

-- | Initial state
initialState :: GameState
initialState = GameState {
  hunger = 10,
  drunk = False,
  experience = 0
}

-- | Entry point
main :: IO ()
main = do
  putStrLn "Welcome to the Haskell Text Adventure!"
  play initialState startNode
