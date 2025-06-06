module Main where

import Nodes
import Engine

main :: IO ()
main = do
  putStrLn "Welcome to the Haskell Text Adventure!"
  play initialState startNode
