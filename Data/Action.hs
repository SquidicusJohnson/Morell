module Action (
  Action
, InverseAction
, act
, inverseAct
) where

class Action g h where
  act :: g -> h -> h

class Action g h => InverseAction g h where
  inverseAct :: g -> h -> h
