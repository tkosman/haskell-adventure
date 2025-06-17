module Engine (play) where

import System.IO (hFlush, stdout)

import GameData
import Utils


applyChoice :: GameState -> Choice -> IO ()
applyChoice gs choice = play (modifyState choice gs) (targetNode choice)

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
  
  case reads input :: [(Int, String)] of
    [(choiceIndex, "")] ->
      if validIndex allChoices choiceIndex && 
        check (condition (allChoices !! choiceIndex)) gameState
        then return (allChoices !! choiceIndex)
        else do
          putStrLn "Nieprawidłowy lub niedostępny wybór. Spróbuj ponownie."
          getValidChoice gameState node
    _ -> do
      putStrLn "Wpisz odpowiednią liczbę."
      getValidChoice gameState node

play :: GameState -> Node -> IO ()
play gs node = do
  clearScreen
  putStrLn "#########################################"
  putStrLn (show gs)
  putStrLn "#########################################"
  putStrLn $ "\n" ++ nodeText node ++ "\n"

  let allChoices = options node

  mapM_ (
    \(index, choice) -> do
      if (check (condition choice) gs)
        then putStrLn $ show index ++ ". " ++ description choice
        else putStrLn $ show index ++ ". 🔒 " ++ description choice ++ " [" ++ (explanation (condition choice)) ++ "]"
    ) (zip [0..] allChoices)

  choice <- getValidChoice gs node
  applyChoice gs choice