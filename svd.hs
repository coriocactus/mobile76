module Main where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Map.Strict as M
import Data.List (sortOn)
import Data.Ord (Down(..))
import Numeric.LinearAlgebra

type UserID = Int
type ItemID = Int
type Ranking = [ItemID]  -- items in order from most to least preferred
type UserRankings = M.Map UserID Ranking

-- | converts ordered rankings to a matrix representation
rankingsToMatrix :: UserRankings -> [ItemID] -> Matrix Double
rankingsToMatrix rankings allItems =
  let n = M.size rankings
      m = length allItems
      itemIndices = M.fromList $ zip allItems [0..]

      -- convert an ordered ranking to rank values
      rankingToValues :: Ranking -> VS.Vector Double
      rankingToValues orderedItems =
        let rankMap = M.fromList $ zip orderedItems [1..]
            lookupRank item = case M.lookup item rankMap of
              Just r  -> fromIntegral r
              Nothing -> fromIntegral (length orderedItems + 1)  -- put unranked items at the end
        in VS.fromList $ map lookupRank allItems

      rows = map (rankingToValues . (rankings M.!)) (M.keys rankings)
  in fromRows rows

-- | calculate mean of each row
rowMeans :: Matrix Double -> VS.Vector Double
rowMeans m = VS.fromList $ map mean $ toRows m
  where mean v = VS.sum v / fromIntegral (VS.length v)

-- | center the matrix by subtracting row means
centerMatrix :: Matrix Double -> Matrix Double
centerMatrix m =
  let means = rowMeans m
      rows = toRows m
      centeredRows = zipWith centerRow rows (VS.toList means)
      centerRow row meanVal = VS.map (\x -> x - meanVal) row
  in fromRows centeredRows

-- | compute svd and return user ordering based on first left singular vector
svdOneDimensionalOrdering :: UserRankings -> [ItemID] -> [(UserID, Double)]
svdOneDimensionalOrdering rankings allItems =
  let users = M.keys rankings
      mat = centerMatrix $ rankingsToMatrix rankings allItems
      -- use the correct svd api from hmatrix
      (u, _, _) = svd mat
      -- extract first left singular vector (users × 1)
      firstDimension = flatten $ takeColumns 1 u
  in sortOn snd $ zip users (VS.toList firstDimension)

main :: IO ()
main = do
  let allItems = [1,2,3,4,5]

      userRankings = M.fromList
        [ (1, [1,2,3,4,5])
        , (2, [5,4,3,2,1])
        , (3, [3,2,1,4,5])
        , (4, [2,3,4,5,1])
        , (5, [4,5,1,2,3])
        , (6, [1,3,5,2,4])
        , (7, [2,1,4,3,5])
        , (8, [5,3,2,1,4])
        , (9, [4,2,5,3,1])
        , (10, [3,4,2,5,1])
        ]

  let ordering = svdOneDimensionalOrdering userRankings allItems
  putStrLn "One-dimensional ordering of users:"
  print ordering
