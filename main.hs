import Control.Monad (when)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)

-- | Game state: each field is an Int, stored in a map
type GameState = Map.Map String Int

-- | A condition to enable a choice
data Condition = Greater String Int
               | Less String Int
               | Equal String Int
               | NotEqual String Int
               deriving (Show)

-- | A modification to state
type Modifier = GameState -> GameState

-- | One choice the player can take
data Choice = Choice
  { description :: String
  , targetNode  :: Node
  , condition   :: Maybe Condition
  , modifyState :: Modifier
  }

-- | A node in the game
data Node = Node
  { nodeText :: String
  , options  :: [Choice]
  }

-- | Evaluate condition
checkCondition :: GameState -> Condition -> Bool
checkCondition gs (Greater key n)   = Map.findWithDefault 0 key gs > n
checkCondition gs (Less key n)      = Map.findWithDefault 0 key gs < n
checkCondition gs (Equal key n)     = Map.findWithDefault 0 key gs == n
checkCondition gs (NotEqual key n)  = Map.findWithDefault 0 key gs /= n

-- | Apply a choice
applyChoice :: GameState -> Choice -> IO ()
applyChoice gs choice = play (modifyState choice gs) (targetNode choice)

-- | Game loop
play :: GameState -> Node -> IO ()
play gs node = do
  putStrLn $ "\n" ++ nodeText node
  let available = filter (maybe True (checkCondition gs) . condition) (options node)
  mapM_ (\(i, c) -> putStrLn $ show i ++ ". " ++ description c) (zip [1..] available)
  putStr "> "
  hFlush stdout
  choiceIndex <- readLn
  if choiceIndex >= 1 && choiceIndex <= length available
    then applyChoice gs (available !! (choiceIndex - 1))
    else do
      putStrLn "Invalid choice. Try again."
      play gs node

-- | Utility to modify a state field
modField :: String -> Int -> Modifier
modField key delta = Map.insertWith (+) key delta

-- | Sample nodes
gameOver :: Node
gameOver = Node "Game Over. Thanks for playing!" []

-- Forward declarations using Haskell laziness
startNode, uniNode, breakfastNode :: Node

startNode = Node "You are in your room! What do you want to do?"
  [ Choice "Go to university" uniNode Nothing (modField "experience" 10)
  , Choice "Eat breakfast" breakfastNode (Just (Greater "experience" 5)) id
  , Choice "Go back to sleep" gameOver Nothing id
  ]

uniNode = Node "You're at the university. You learned a lot!"
  [ Choice "Go back home" startNode Nothing (modField "experience" 5) ]

breakfastNode = Node "Yum! That was a good breakfast."
  [ Choice "Go back to room" startNode Nothing (modField "hunger" (-5)) ]

-- | Initial state
initialState :: GameState
initialState = Map.fromList [("experience", 0), ("hunger", 10)]

-- | Entry point
main :: IO ()
main = do
  putStrLn "Welcome to the Haskell Text Adventure!"
  play initialState startNode
