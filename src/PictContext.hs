module PictContext where

class Context a where
  getColor :: a -> Int -> Int -> ColorIndex
  
data ColorIndex = Red | Orange | Yellow | Green | Blue | Indigo | Violet
                deriving (Show)
  
toColorIndex :: String -> ColorIndex
toColorIndex str =
  case str of
    "Red" -> Red
    "Orange" -> Orange
    "Yellow" -> Yellow 
    "Green" -> Green 
    "Blue" -> Blue 
    "Indigo" -> Indigo 
    "Violet" -> Violet
