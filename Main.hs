module Main where

import Nodes
import Engine
import GameData

initialState :: GameState
initialState = GameState {
  przygotowanie = 0,
  obecnyPrzedmiot = "",
  dzień = 0,
  czas = 0,
  punkty = 0,
  zadowolenie = 0,
  ada = 0,
  c = 0,
  oop = 0,
  matematyka = 0,
  charyzma = 0
}

main :: IO ()
main = do
  putStrLn "Welcome to the Haskell Text Adventure!"
  play (initialState) startNode
