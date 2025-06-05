import Control.Monad (when)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import System.Info (os)
import System.Process (callCommand)

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

-- | Explain why a condition failed
explainCondition :: GameState -> Condition -> String
explainCondition gs cond = 
  let actual = Map.findWithDefault 0 key gs
      key = case cond of
        Greater k _   -> k
        Less k _      -> k
        Equal k _     -> k
        NotEqual k _  -> k
      expected = case cond of
        Greater _ n   -> "> " ++ show n
        Less _ n      -> "< " ++ show n
        Equal _ n     -> "== " ++ show n
        NotEqual _ n  -> "/= " ++ show n
  in "Requires `" ++ key ++ " " ++ expected ++ "` (currently: " ++ show actual ++ ")"

-- | Render the game state as a string
renderGameState :: GameState -> String
renderGameState gs = unlines $ map (\(k,v) -> k ++ ": " ++ show v) (Map.toList gs)

-- | Apply a choice
applyChoice :: GameState -> Choice -> IO ()
applyChoice gs choice = play (modifyState choice gs) (targetNode choice)

clearScreen :: IO ()
clearScreen = callCommand $ if os == "mingw32" then "cls" else "clear"

getValidChoice :: GameState -> Node -> IO Choice
getValidChoice gs node = do
  putStr "> "
  hFlush stdout
  input <- getLine
  let allChoices = options node
  case reads input :: [(Int, String)] of
    [(choiceIndex, "")] ->
      if choiceIndex >= 1 && choiceIndex <= length allChoices &&
         maybe True (checkCondition gs) (condition (allChoices !! (choiceIndex - 1)))
        then return (allChoices !! (choiceIndex - 1))
        else do
          putStrLn "Invalid or unavailable choice. Try again."
          getValidChoice gs node
    _ -> do
      putStrLn "Please enter a valid number."
      getValidChoice gs node

-- | Game loop with styled output and inline locked choices
play :: GameState -> Node -> IO ()
play gs node = do
  clearScreen
  putStrLn "#########################################"
  putStrLn $ renderGameState gs
  putStrLn "#########################################"
  putStrLn $ "\n " ++ nodeText node ++ "\n"

  let allChoices = options node

  mapM_ (\(i, c) -> do
            let isAvailable = maybe True (checkCondition gs) (condition c)
            if isAvailable
              then putStrLn $ show i ++ ". " ++ description c
              else case condition c of
                     Just cond -> putStrLn $ show i ++ ". 🔒 " ++ description c ++ " [" ++ explainCondition gs cond ++ "]"
                     Nothing   -> return ()
        ) (zip [1..] allChoices)

  choice <- getValidChoice gs node
  applyChoice gs choice

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
