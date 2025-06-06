module Main where

import System.Random (randomRIO, mkStdGen, randomR)
-- random number from a range:  randomR (1,10) (randGen gs)

import Nodes
import Engine
import GameData


initialState :: GameState
initialState = GameState {
  randGen = (mkStdGen 0),
  hunger = 10,
  drunk = False,
  experience = 0
}

main :: IO ()
main = do
  num <- randomRIO (1, 1000000)  -- random number between 1 and 10 (inclusive)
  putStrLn "Welcome to the Haskell Text Adventure!"
  play (initialState {randGen = (mkStdGen num)}) startNode
