module DustUp.Ability where

data Ability condition movement modifier
  = condition `Triggered` movement
  | Activated movement
  | Static modifier
