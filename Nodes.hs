-- GameState.hs
module Nodes (startNode) where

import GameData

gameOver :: Node
gameOver = Node "Game Over. Thanks for playing!" []

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
  [ Choice "Go back to room" startNode noCondition (\gs -> gs { hunger = hunger gs - 5}) ]