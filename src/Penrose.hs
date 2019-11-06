{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
module Penrose where

import           Control.Monad (join)
import           Data.Bool     (bool)
import           Data.Coerce   (coerce)
import           Data.Function ((&))
import           Data.Monoid   (Any (..))
import           Debug.Trace
import           System.Random

data Point = Point { x :: Int, y :: Int}
  deriving (Eq, Show)

instance Semigroup Point where
  (Point x y) <> (Point x' y') = Point (x + x') (y + y')

data Line = Line { start :: Point, end :: Point }

type Matrix = [[TriangleBeams Bool]]
data Coords = Coords { cx :: Int, cy :: Int }
  deriving (Eq, Show)

data TriangleBeams a
  = TriangleBeams
  { ns  :: a
  , nwe :: a
  , swe :: a
  }
  deriving (Eq, Show, Functor)

instance Semigroup m => Semigroup (TriangleBeams m) where
    TriangleBeams{..} <> (TriangleBeams ns' nwe' swe') =
        TriangleBeams (ns <> ns') (nwe <> nwe') (swe <> swe')

instance Monoid m => Monoid (TriangleBeams m) where
    mempty = TriangleBeams mempty mempty mempty

data SummitBeams a
  = SummitBeams
  { n  :: a
  , ne :: a
  , se :: a
  , s  :: a
  , sw :: a
  , nw :: a
  }
  deriving (Eq, Show, Functor)

instance Semigroup m => Semigroup (SummitBeams m) where
    l <> r =
        SummitBeams
          { n  = n  l <> n r
          , ne = ne l <> ne r
          , se = se l <> se r
          , s  = s  l <> s r
          , sw = sw l <> sw r
          , nw = nw l <> nw r
          }

instance Monoid m => Monoid (SummitBeams m) where
    mempty = SummitBeams mempty mempty mempty mempty mempty mempty


w, hw, ww, hww :: Int
w = 2*hw
hw = 5
ww = 2*hww
hww = 25

l :: (Point, Point) -> Line
l = uncurry Line

p :: (Int, Int) -> Point
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

boundaries :: SummitBeams Bool -> [Line]
boundaries SummitBeams{..} =
  let keep = map fst . filter snd
  in keep [
            {-01-} (l(p(-2*w,0),p(-w,-hw)),    sw)
          , {-02-} (l(p(-2*w,0),p(-w, hw)),    nw && not sw)
          , {-03-} (l(p(-w,-3*hw),p(-w,-hw)), n)
          , {-04-} (l(p(-w,-hw),p(-w,hw)),  not sw && not nw && (n || s || se || ne))
          , {-05-} (l(p(-w,hw),p(-w,3*hw)),   s && not sw)
          , {-06-} (l(p(-w,-3*hw),p(0,-w)),      nw && not n)
          , {-07-} (l(p(-w,-hw),p(0,-w)),      not n && not nw && (ne||sw||s||se))
          , {-08-} (l(p(0,0),p(-w,-hw)),       (n&&sw) || (not n && not sw && (se || ne || nw || s)))
          , {-09-} (l(p(-w,hw),p(0,0)),        sw)
          , {-10-} (l(p(-w,hw),p(0,w)),        not sw && not s && (nw || se || n || ne))
          , {-11-} (l(p(-w,3*hw),p(0,w)),        sw)
          , {-12-} (l(p(0,-w),p(0,0)),            n)
          , {-13-} (l(p(0,0),p(0,w)),             (not sw || se) && (not se || sw) && (n || s || sw || nw || se || ne))
          , {-14-} (l(p(0,-w),p(w,-3*hw)),       not n && ne)
          , {-15-} (l(p(0,-w),p(w,-hw)),       not n && not ne && (nw || sw || se || s))
          , {-16-} (l(p(0,0),p(w,-hw)),        (n&&se) || (not n && not se && (sw || ne || nw || s)))
          , {-17-} (l(p(0,0),p(w,hw)),         se)
          , {-18-} (l(p(0,w),p(w,hw)),         (n||nw||sw||ne)&&not s&&not se)
          , {-19-} (l(p(0,w),p(w,3*hw)),         se)
          , {-20-} (l(p(w,-3*hw),p(w,-hw)),   n)
          , {-21-} (l(p(w,-hw),p(w,hw)),    (n||s||sw||nw)&&not se&&not ne)
          , {-22-} (l(p(w,hw),p(w,3*hw)),     s  && not se)
          , {-23-} (l(p(w,-hw),p(2*w,0)),      se)
          , {-24-} (l(p(w,hw),p(2*w,0)),       ne && not se)
          ]

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
      northBoundaries = boundaries north
      southBoundaries = move toSouth <$> boundaries south
      eastBoundaries = move toEast <$> boundaries east
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

getNorthSummits :: TriangleBeams Bool
                -> TriangleBeams Bool
                -> TriangleBeams Bool
                -> SummitBeams Bool
getNorthSummits current above topLeft = SummitBeams
  { n  = above & ns
  , ne = above & swe
  , se = current & nwe
  , s  = current & ns
  , sw = topLeft & swe
  , nw = topLeft & nwe
  }

getSouthSummits :: TriangleBeams Bool
                -> TriangleBeams Bool
                -> TriangleBeams Bool
                -> SummitBeams Bool
getSouthSummits current below bottomLeft | isEmpty below = SummitBeams
  { n  = current & ns
  , ne = current & swe
  , se = below & nwe
  , s  = below & ns
  , sw = bottomLeft & swe
  , nw = bottomLeft & nwe
  }
                                         | otherwise = getAny <$> mempty

getEastSummits :: TriangleBeams Bool
                -> TriangleBeams Bool
                -> TriangleBeams Bool
                -> SummitBeams Bool
getEastSummits current topRight bottomRight | isEmpty bottomRight = SummitBeams
  { n  = topRight & ns
  , ne = topRight & swe
  , se = bottomRight & nwe
  , s  = bottomRight & ns
  , sw = current & swe
  , nw = current & nwe
  }
                                            | otherwise = getAny <$> mempty

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
  in traceShow (cs, Coords (cx -1) top) $ getTriangle current north south east

getMatrix :: Matrix -> [Line]
getMatrix matrix = do
  cy <- [0 .. length matrix - 1]
  let row = matrix !! cy
  cx  <- [0 .. length row - 1]
  let coords = Coords{..}
  let even = cx `mod` 2 == 0
  let offset = Point { x = cx * ww
                     , y = cy * ww + bool hww 0 even }
  let lines = getTriangleAt matrix coords
  move (<> offset) <$> lines

getSVG :: (Int,Int) -> [Line] -> String
getSVG (width, height) lines =
  let path = foldMap (toPathElement . move (<> Point w w)) lines
      toPathElement Line{..} = "M " <> show (x start) <> " "
                                    <> show (y start) <> " "
                            <> "L " <> show (x end) <> " "
                                    <> show (y end) <> " "
  in "<svg width=\""<> show width <> "\" height=\""<> show height <> "\" xmlns=\"http://www.w3.org/2000/svg\">\
  \<path d=\"" <> path <> "\" fill=\"transparent\" stroke=\"black\"></path>\
  \</svg>"

renderMatrix :: [[Int]] -> String
renderMatrix rawMatrix =
  let height = 2*w + hww + length rawMatrix * ww
      width = 2*w + maximum (length <$> rawMatrix) * ww
  in getSVG (width, height) . getMatrix . fmap (fmap readBeams) $ rawMatrix

data Mode = Full | Random deriving Read

genMatrix :: Mode -> Int -> Int -> IO [[Int]]
genMatrix Full w h = pure . replicate h . replicate w $ 7
genMatrix Random w h = traverse (const $ traverse (const $ randomRIO (0,7)) [1..w]) [1..h]
