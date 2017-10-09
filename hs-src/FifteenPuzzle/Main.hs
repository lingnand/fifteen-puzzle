{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module FifteenPuzzle.Main
  (
    main
  ) where

import           Control.Lens hiding (Index)
import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.Trans.Maybe
import qualified Data.Array as A
import           Data.Bool
import           Data.Colour
import           Data.Colour.Names (green, white, yellow)
import           Data.List
import           Data.Maybe
import           Diagrams.Coordinates
import           Diagrams.Points
import           Diagrams.TwoD (P2, V2(..))
import           Linear.Affine

import           Reflex
import           Reflex.Cocos2d
import           Reflex.EventVisitor

---- Model side

type Index = (Int, Int)

data GridConfig = GridConfig
  { gridWidth :: Int
  }

newtype Grid = Grid { unGrid :: A.Array Index (Maybe Int) } deriving (Show)
instance Decomposable Grid where
  type ComponentKey Grid = Index
  type Component Grid    = Maybe Int
  decomposeDyn = decomposeDyn . fmap unGrid

-- | A default initialized Grid with the puzzle solved
newGrid :: GridConfig -> Grid
newGrid GridConfig{ gridWidth = wid } = Grid $ A.array bounds
  [ (idx, initNum idx) | idx <- A.range bounds]
  where
    bounds = ((0,0), (wid-1,wid-1))
    initNum (x,y) | x == wid-1 && y == 0 = Nothing
                  | otherwise = Just $ (wid-1-y)*wid+x+1

val :: Index -> Grid -> Maybe Int
val x (Grid arr) = arr A.! x

inBounds :: Index -> Grid -> Bool
inBounds x (Grid arr) = A.inRange (A.bounds arr) x

hasWon :: Grid -> Bool
hasWon (Grid arr) = all (uncurry valid) (A.assocs arr)
  where valid (x,y) v
          | x == w, y == 0 = isNothing v
          | otherwise = v == Just ((w-y)*(w+1) + x+1)
        (_, (w, _)) = A.bounds arr

step :: Index -> Action -> Grid -> Maybe Grid
step p@(x,y) act g@(Grid arr)
  | act == ActionUp   , inBounds up g   , isNothing (val up g)    = Just $ swap p up
  | act == ActionDown , inBounds down g , isNothing (val down g)  = Just $ swap p down
  | act == ActionLeft , inBounds left g , isNothing (val left g)  = Just $ swap p left
  | act == ActionRight, inBounds right g, isNothing (val right g) = Just $ swap p right
  | otherwise                                                      = Nothing
  where swap i j = Grid $ arr A.// [(i, val j g), (j, val i g)]
        up = (x, y+1)
        down = (x, y-1)
        left = (x-1, y)
        right = (x+1, y)

randGenStep :: MonadRandom m => Grid -> m (Maybe (Index, Action))
randGenStep g@(Grid arr) = runMaybeT $ do
  ((x,y), _) <- MaybeT . return $ find (isNothing . snd) $ A.assocs arr
  -- generate a list of valid moves
  lift . uniform $ filter (\(loc,_) -> inBounds loc g)
    [ ((x+1,y), ActionLeft)
    , ((x-1,y), ActionRight)
    , ((x,y+1), ActionDown)
    , ((x,y-1), ActionUp)
    ]

randStep :: MonadRandom m => Grid -> m Grid
randStep g = fmap (fromMaybe g) . runMaybeT $ do
  (idx, act) <- MaybeT (randGenStep g)
  MaybeT . return $ step idx act g

---- Render side

data Action = ActionUp | ActionDown | ActionLeft | ActionRight deriving (Enum, Eq, Show)

data RenderConfig = RenderConfig
  { tileWidth    :: Float
  , tileGap      :: Float
  , tileFontSize :: Float
  , gridPos      :: P2 Float
  }

gridScreenWidth :: GridConfig -> RenderConfig -> Float
gridScreenWidth GridConfig{..} RenderConfig{..} = tileWidth * w + tileGap * (w - 1)
  where w = fromIntegral gridWidth

screenToIndex :: GridConfig -> RenderConfig -> P2 Float -> Index
screenToIndex gc@GridConfig{..} rc@RenderConfig{..} p = (gx, gy)
  where lowerLeft = gridPos - pure gScreenW/2
        gScreenW = gridScreenWidth gc rc
        V2 x y = p .-. lowerLeft
        gx = floor $ x / (tileWidth+tileGap)
        gy = floor $ y / (tileWidth+tileGap)

indexToScreen :: GridConfig -> RenderConfig -> Index -> P2 Float
indexToScreen gc rc@RenderConfig{..} = \(x, y) ->
      tileSize * (fromIntegral x ^& fromIntegral y)
    + tileSize/2
    - (pure gScreenW/2)
  where gScreenW = gridScreenWidth gc rc
        tileSize = pure (tileWidth+tileGap)

genStep
  :: (P2 Float -> Index) -- ^ position translator (from screen point to index)
  -> P2 Float            -- ^ touch start point
  -> P2 Float            -- ^ touch end point
  -> (Index, Action)
genStep translator start end = (idx, action)
  where action = vecToAction (end .-. start)
        idx = translator start
        vecToAction (V2 x y)
          | abs x > abs y = bool ActionLeft ActionRight (x > 0)
          | otherwise     = bool ActionDown ActionUp    (y > 0)

renderGrid
  :: CocosBuilder t m
  => GridConfig
  -> RenderConfig
  -> Dynamic t Grid
  -> m ()
renderGrid gc rc dGrid = node [ position := gridPos rc ] -< do
  d <- decomposeDyn dGrid
  runWithAccumDyn'_ . ffor d $ \ls ->
    forM_ ls $ \(idx, mayD) ->
      renderTile rc (toScreen idx) (join <$> mayD)
  where toScreen = indexToScreen gc rc

renderTile
  :: CocosBuilder t m
  => RenderConfig
  -> P2 Float -- ^ position
  -> Dynamic t (Maybe Int)
  -> m ()
renderTile RenderConfig{..} pos dynNum =
  node [ position     := pos
       , dyn' visible := isJust <$> dynNum ] -< do
    sprite_
      [ alphaColor  := opaque green
      , textureRect := Rect 0 (pure tileWidth)
      ]
    label_
      [ dyn' text       := maybe "" show <$> dynNum
      , systemFontSize  := tileFontSize
      , textColor       := opaque white
      ]

main :: IO ()
main = mainScene $ do
  S sz <- getWindowSize
  eeTouch <- getTouchEvents
  eTouchSeq <- accumTouchSeqEvent eeTouch
  let gc = GridConfig
        { gridWidth = 4
        }
      rc = RenderConfig
        { tileWidth = 100
        , tileGap = 10
        , tileFontSize = 20
        , gridPos = midP
        }
      midP = P $ sz/2
      eStep = fforMaybe eTouchSeq $ \sq -> genStep (screenToIndex gc rc) <$>
        sq^?touchFirst.touchLocation <*> sq^?touchLast.touchLocation
  -- performEvent_ $ liftIO . debug . ("/began/ "++) . show <$> eeTouch^.touchBegan
  -- performEvent_ $ liftIO . debug . ("/end/ "++) . show <$> eeTouch^.touchEnded
  -- performEvent_ . fforMaybe eTouchSeq $ \s ->
  --   (\l -> debug $ show (s^.touchFirst) ++ ", " ++ show l) <$> (s^?touchLast)
  -- performEvent_ $ liftIO . debug . ("STEP: "++) . show <$> eStep
  runEventVisitorT_ $ forever $ do
    -- start game
    let grid0 = newGrid gc
        numSteps = 200
    -- initialize grid
    dGrid <- exec $ do
      grid0' <- evalRandTIO $ iterate (>>= randStep) (return grid0) !! numSteps
      accumMaybe (flip $ uncurry step) grid0' eStep
    exec $ renderGrid gc rc dGrid
    -- wait for end game
    waitDynBool $ hasWon <$> dGrid
    eRestart <- exec $ do
      label_
        [ text           := "You have won!"
        , systemFontSize := 50
        , textColor      := opaque yellow
        , position       := midP
        ]
      button_
        [ titleText := "Restart"
        , titleFontSize := 30
        , position := midP - 0^&100
        ]
    -- wait for restart
    wait_ eRestart
