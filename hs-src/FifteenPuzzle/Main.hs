module FifteenPuzzle.Main
  (
    main
  ) where

import Diagrams.Coordinates
import Data.Colour.Names (green, white)
import Data.Colour
import Control.Monad

import Reflex.Cocos2d

newtype Grid = Grid (Array (Int, Int) (Maybe Int)) deriving (Show, Decomposable)
data Action = Action_Up | Action_Down | Action_Left | Action_Right deriving (Enum, Eq, Show)

-- | A default initialized Grid with the empty slot at the lower left corner
newGrid :: Int -> Grid
newGrid width = Grid . listArray ((0,0), (width,width)) $ Nothing : (Just <$> [1..])

val :: (Int, Int) -> Grid -> Maybe Int
val x (Grid arr) = arr ! x

inBounds :: (Int, Int) -> Grid -> Bool
inBounds x (Grid arr) = inRange (bounds arr) x

act :: (Int, Int) -> Action -> Grid -> Maybe Grid
act p@(x,y) act g@(Grid arr)
  | act == Action_Up   , inBounds up g   , val up g    == Nothing = Just $ swap p up
  | act == Action_Down , inBounds down g , val down g  == Nothing = Just $ swap p down
  | act == Action_Left , inBounds left g , val left g  == Nothing = Just $ swap p left
  | act == Action_Right, inBounds right g, val right g == Nothing = Just $ swap p right
  | otherwise                                                     = Nothing
  where swap i j = Grid $ arr // [(i, val j g), (j, val i g)]
        up = (x, y+1)
        low = (x, y-1)
        left = (x-1, y)
        right = (x+1, y)

-- renderGrid :: Dynamic Spider Grid ->


renderTile :: P2 Float -> Dynamic Spider (Maybe Int) -> SpiderNodeBuilder ()
renderTile pos dynNum = void $
  node [ position     := pos
       , dyn' visible := isJust <$> dynNum ] -< do
    sprite_
      [ alphaColor  := opaque green
      , textureRect := Rect 0 100
      ]
    label_
      [ dyn' text       := maybe "" show <$> dynNum
      , systemFontSize  := 20
      , textColor       := opaque white
      ]


main :: IO ()
main = mainScene $ do
  forM_
    [ (x, y)
    | x <- [1..4]
    , y <- [1..4]
    ] $ \(x, y) ->
    buildTile [ position := (x*110) ^& (y*110) ] (show $ 4*y+x)
