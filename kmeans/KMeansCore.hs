--
-- Adapted from the K-Means example in the remote-0.1.1 package,
--   (c) Jeff Epstein <jepst79@gmail.com>
--

module KMeansCore where

import Control.DeepSeq
import Data.Binary
import qualified Data.ByteString.Char8 as B
import Data.List

-- -----------------------------------------------------------------------------
-- Points

data Point = Point {-#UNPACK#-}!Double {-#UNPACK#-}!Double
    deriving (Show,Read,Eq)

-- deepseq changed in GHC 7.10 to use Generic instances, so for backwards
-- compatibility define it manually.
instance NFData Point where
  rnf (Point _ _) = () -- all fields are strict

-- <<point-ops
zeroPoint :: Point
zeroPoint = Point 0 0

sqDistance :: Point -> Point -> Double
sqDistance (Point x1 y1) (Point x2 y2) = ((x1-x2)^(2::Int)) + ((y1-y2)^(2::Int))
-- >>

instance Binary Point where
  put (Point a b) = put a >> put b
  get = Point <$> get <*> get

readPoints :: FilePath -> IO [Point]
readPoints f = do
  s <- B.readFile f
  let ls = map B.words $ B.lines s
      points = [ Point (read (B.unpack sx)) (read (B.unpack sy))
               | (sx:sy:_) <- ls ]
  --
  return points

-----------------------------------------------------------------------------
-- Clusters

data Cluster
  = Cluster { clId   :: {-# UNPACK #-} !Int
            , clCent :: {-# UNPACK #-} !Point
            }
  deriving (Show,Read,Eq)

instance NFData Cluster where
  rnf (Cluster _id _cent) = () -- all fields are strict

makeCluster :: Int -> [Point] -> Cluster
makeCluster clid points =
  Cluster { clId    = clid
          , clCent  = Point (a / fromIntegral count) (b / fromIntegral count)
          }
 where
  (Point a b) = foldl' addPoint zeroPoint points
  count = length points

  addPoint :: Point -> Point -> Point
  addPoint (Point e f) (Point c d) = Point (e+c) (f+d)
