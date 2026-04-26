module DustUp.Dice where

data Category
  = Attack
  | Defence
  | Thought

data Dice
  = One
  | Two
  | Three
  | Four
  | Five
  | Six

repr :: Dice -> Integer
repr = \case
  One -> 1
  Two -> 2
  Three -> 3
  Four -> 4
  Five -> 5
  Six -> 6

instance Num Dice where
  fromInteger n = case abs n `mod` 6 of
    0 -> Six
    1 -> One
    2 -> Two
    3 -> Three
    4 -> Four
    5 -> Five
    _ -> error "unreachable"

  abs = id
  a + b = fromInteger (repr a + repr b)
  a - b = fromInteger (repr a - repr b)
  a * b = fromInteger (repr a * repr b)
  signum = const 1
