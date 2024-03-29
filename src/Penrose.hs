{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Penrose where

import           Control.Monad       (join)
import           Data.Bool           (bool)
import           Data.Coerce         (coerce)
import           Data.Function       ((&))
import           Data.Monoid         (Any (..), Sum (..))
import           Data.Monoid.Generic
import           Data.Ratio
import           GHC.Generics        (Generic)
import           System.Random

data SVGConfig = SVGConfig
  { halfBar     :: Int
  , halfPattern :: Int
  }

data PointCoord = PointC { _hw :: Sum Int, _hW :: Sum Int }
  deriving (Generic, Eq)
  deriving Semigroup via GenericSemigroup PointCoord
  deriving Monoid via GenericMonoid PointCoord

toTuple PointC{..} = (_hw, _hW)

instance Show PointCoord where
  show = \case
      0              -> "0"
      PointC _hw 0   -> showRational _hw "w"
      PointC 0 _hW   -> showRational _hW "W"
      PointC _hw _hW -> showRational _hw "w" <> "+" <> showRational _hW "W"
    where
      showRational 2 mult       = mult
      showRational (Sum x) mult =
        let r = x % 2
            n = numerator r
            d = denominator r
         in bool (show n <> "/2") (show n) (d == 1) <> "*" <> mult

instance Num PointCoord where
 (+) = (<>)
 (PointC _hw _hW) * (PointC _hw' _hW') = PointC (_hw * _hw') (_hW * _hW')
 abs PointC{..} = PointC (abs _hw) (abs _hW)
 signum PointC{..} = PointC (signum _hw) (signum _hW)
 fromInteger n = PointC (fromIntegral n) (fromIntegral n)
 negate PointC{..} = PointC (negate _hw) (negate _hW)

(*:) :: Int -> PointCoord -> PointCoord
n *: p = fromIntegral n * p

-- | Generic point type, that can work in different
-- coordinate systems
data GPoint c = Point { x :: c, y :: c}
  deriving (Generic, Eq, Show, Functor)
  deriving Semigroup via GenericSemigroup (GPoint c)
  deriving Monoid via GenericMonoid (GPoint c)

-- | Point in the (x,y) coordinate system
type Point  = GPoint PointCoord
-- | Point in the (NW,S) coordinate system
type Point' = GPoint Int

data GLine p = Line { start :: p, end :: p }
  deriving Functor

type Line  = GLine Point
type Line' = GLine Point'

type Matrix = [[TriangleBeams Bool]]
data Coords = Coords { cx :: Int, cy :: Int }
  deriving (Eq, Show)

data TriangleBeams a
  = TriangleBeams
  { ns  :: a
  , nwe :: a
  , swe :: a
  }
  deriving (Generic, Eq, Show, Functor)
  deriving Semigroup via (GenericSemigroup (TriangleBeams a))
  deriving Monoid via (GenericMonoid (TriangleBeams a))

data SummitBeams a
  = SummitBeams
  { n  :: a
  , ne :: a
  , se :: a
  , s  :: a
  , sw :: a
  , nw :: a
  }
  deriving (Generic, Eq, Show, Functor)
  deriving Semigroup via (GenericSemigroup (SummitBeams a))
  deriving Monoid via (GenericMonoid (SummitBeams a))

w, hw, ww, hww :: PointCoord
w = PointC 2 0
-- ^ the width of a bar
hw = PointC 1 0
-- ^ half the width of a bar. This allows us to only work with ints
ww = PointC 0 2
-- ^ the width of a pattern (the height of a triangle).
hww = PointC 0 1
-- ^ half the width of a pattern. This allows us to only work with ints

-- | Helper for compact input of a line
l :: (p, p) -> GLine p
l = uncurry Line

-- | Helper for compact input of a point
p :: (c, c) -> GPoint c
p = uncurry Point

beams :: TriangleBeams [Line]
beams = TriangleBeams
  { ns  = [ l(p(-w,3*hw), p(-w,ww-hw))
          , l(p(0,w),      p(0,ww-w))
          , l(p(w,3*hw),  p(w,ww-hw))
          ]
  , nwe = [ l(p(w, 3*hw),  p(ww-2*w,hww))
          , l(p(w, hw),  p(ww-w,hww-hw))
          , l(p(2*w,0),     p(ww-w, hww-3*hw))
          ]
  , swe = [ l(p(w, ww-3*hw), p(ww-2*w, hww))
          , l(p(w, ww-hw), p(ww-w,   hww+hw))
          , l(p(2*w, ww),     p(ww-w,   hww+3*hw))
          ]
  }

getBeamsLines :: TriangleBeams Bool -> [Line]
getBeamsLines (TriangleBeams ns' nwe' swe') =
  let TriangleBeams{..} = beams
  in join [ filter (const ns')  ns
          , filter (const nwe') nwe
          , filter (const swe') swe
          ]

boundaries' :: SummitBeams Bool -> [Line']
boundaries' SummitBeams{..} =
  let keep = map fst . filter snd
  in keep [
          -- SE
            {-13-} (l(p(0,0), p(0,1)), (sw&&se) || (not se && not sw && (n||s||nw||ne)))
          , {-17-} (l(p(0,0), p(1,1)), se)
          , {-18-} (l(p(0,1), p(1,1)), (n||nw||sw||ne)&&not s&&not se)
          , {-19-} (l(p(0,1), p(1,2)), se)
          , {-21-} (l(p(1,0), p(1,1)), (n||s||sw||nw)&&not se&&not ne)
          , {-22-} (l(p(1,1), p(1,2)), s && not se)
          , {-23-} (l(p(1,0), p(2,1)), se)
          , {-24-} (l(p(1,1), p(2,1)), ne && not se)
          ]

inSE, inN, inSW :: Point' -> Point
inSE Point{..} = Point { x = x *: w
                       , y = (2 * y - x) *: hw
                       }
inN Point{..}  = Point { x = (y - x) *: w
                       , y = (-x -y) *: hw
                       }
inSW Point{..} = Point { x = -y *: w
                       , y = (2 * x - y) *: hw
                       }

render :: (Point' -> Point) -> Line' -> Line
render f = fmap f

boundaries'' :: SummitBeams Bool -> [Line]
boundaries'' sb =
  let se = render inSE <$> boundaries' sb
      n  = render inN  <$> boundaries' (rotateBeams sb)
      sw = render inSW <$> boundaries' (rotateBeams $ rotateBeams sb)
  in  se <> n <> sw

rotateBeams :: SummitBeams a -> SummitBeams a
rotateBeams SummitBeams{..} =
  SummitBeams { n  = sw
              , ne = nw
              , se = n
              , s  = ne
              , sw = se
              , nw = s
              }

toSouth :: Point -> Point
toSouth = (<> Point 0 ww)

toEast :: Point -> Point
toEast = (<> Point ww hww)

move :: (Point -> Point) -> Line -> Line
move f Line{..} = Line (f start) (f end)

getTriangle :: TriangleBeams Bool
            -> SummitBeams Bool
            -> SummitBeams Bool
            -> SummitBeams Bool
            -> [Line]
getTriangle beams north south east =
  let beamsLines = getBeamsLines beams
      northBoundaries = boundaries'' north
      southBoundaries = move toSouth <$> boundaries'' south
      eastBoundaries = move toEast <$> boundaries'' east
  in join [beamsLines, northBoundaries, southBoundaries, eastBoundaries]

readBeams :: Int -> TriangleBeams Bool
readBeams code =
  let ns  = code `mod` 2 == 1
      nwe = code `elem` [2,3,6,7]
      swe = code >= 4
  in TriangleBeams{..}

safeAt :: Monoid m => [m] -> Int -> m
safeAt ls i =
  let inRange = i >= 0 && i < length ls
  in if inRange then ls !! i else mempty

get :: Matrix -> Coords -> TriangleBeams Bool
get matrix Coords{..} =
  let row = safeAt (coerce matrix) cy
  in getAny <$> safeAt row cx

isEmpty :: TriangleBeams Bool -> Bool
isEmpty bs = bs == (getAny <$> mempty)

getSummits :: TriangleBeams Bool
           -> TriangleBeams Bool
           -> TriangleBeams Bool
           -> SummitBeams Bool
getSummits a b c = SummitBeams
  { n  = a & ns
  , ne = a & swe
  , se = b & nwe
  , s  = b & ns
  , sw = c & swe
  , nw = c & nwe
  }
getNorthSummits :: TriangleBeams Bool
                -> TriangleBeams Bool
                -> TriangleBeams Bool
                -> SummitBeams Bool
getNorthSummits current above topLeft = getSummits above current topLeft

getSouthSummits :: TriangleBeams Bool
                -> TriangleBeams Bool
                -> TriangleBeams Bool
                -> SummitBeams Bool
getSouthSummits current below bottomLeft
  | isEmpty below = getSummits current below bottomLeft
  | otherwise     = getAny <$> mempty

getEastSummits :: TriangleBeams Bool
                -> TriangleBeams Bool
                -> TriangleBeams Bool
                -> SummitBeams Bool
getEastSummits current topRight bottomRight
  | isEmpty bottomRight = getSummits topRight bottomRight current
  | otherwise           = getAny <$> mempty

getTriangleAt :: Matrix -> Coords -> [Line]
getTriangleAt matrix cs@Coords{..} =
  let even = cx `mod` 2 == 0
      g = get matrix
      top = bool cy (cy - 1) even
      bottom = bool (cy + 1) cy even
      -- beams in current triangle and neighbors
      current = g cs
      above = g $ Coords cx (cy - 1)
      below = g $ Coords cx (cy + 1)
      topLeft = g $ Coords (cx - 1) top
      bottomLeft = g $ Coords (cx - 1) bottom
      topRight = g $ Coords (cx + 1) top
      bottomRight = g $ Coords (cx + 1) bottom
      -- summits
      north = getNorthSummits current above topLeft
      south = getSouthSummits current below bottomLeft
      east  = getEastSummits  current topRight bottomRight
  in getTriangle current north south east

getMatrix :: Matrix -> [Line]
getMatrix matrix = do
  cy <- [0 .. length matrix - 1]
  let row = matrix !! cy
  cx  <- [0 .. length row - 1]
  let coords = Coords{..}
  let even = cx `mod` 2 == 0
  let offset = Point { x = cx *: ww
                     , y = cy *: ww + bool hww 0 even }
  let lines = getTriangleAt matrix coords
  move (<> offset) <$> lines

defaultSVGConfig :: SVGConfig
defaultSVGConfig = SVGConfig 5 40

getSVG :: SVGConfig -> (PointCoord,PointCoord) -> [Line] -> String
getSVG SVGConfig{..} (width, height) lines =
  let path = foldMap (toPathElement . move (<> Point w w)) lines
      toCoord PointC{..} = show . getSum $ _hw * Sum halfBar + _hW * Sum halfPattern
      toPathElement Line{..} = "M " <> toCoord (x start) <> " "
                                    <> toCoord (y start) <> " "
                            <> "L " <> toCoord (x end) <> " "
                                    <> toCoord (y end) <> " "
  in "<svg width=\""<> toCoord width <> "\" height=\""<> toCoord height <> "\" xmlns=\"http://www.w3.org/2000/svg\">\
  \<path d=\"" <> path <> "\" fill=\"transparent\" stroke=\"black\"></path>\
  \</svg>"

renderMatrix :: [[Int]] -> String
renderMatrix rawMatrix =
  let height = 2 * w + hww + length rawMatrix *: ww
      width = 2 * w + maximum (length <$> rawMatrix) *: ww
  in getSVG defaultSVGConfig (width, height) . getMatrix . fmap (fmap readBeams) $ rawMatrix

data Mode = Full | Random deriving Read

genMatrix :: Mode -> Int -> Int -> IO [[Int]]
genMatrix Full w h = pure . replicate h . replicate w $ 7
genMatrix Random w h = traverse (const $ traverse (const $ randomRIO (0,7)) [1..w]) [1..h]
