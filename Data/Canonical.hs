module Data.Canonical where

class Canonical a where
  canonicalize :: a -> a
  canonicalize =
    id
