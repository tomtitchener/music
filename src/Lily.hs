-- ToLily and FromLily type classes

module Lily where

class ToLily a where
  -- | Convert a Haskell value to a Lilypond string
  toLily :: a -> String

class FromLily a where
  -- | Convert a Lilypond string to a Haskell value
  parseLily :: String -> a
