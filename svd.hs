module Main where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Map.Strict as M
import Data.List (sortOn)
import Data.Ord (Down(..))
import Numeric.LinearAlgebra

type UserID = Int
type ItemID = Int
type Rank = Int
type Ranking = [(ItemID, Rank)]
type UserRankings = M.Map UserID Ranking

-- | converts raw rankings to a matrix representation
rankingsToMatrix :: UserRankings -> [ItemID] -> Matrix Double
rankingsToMatrix rankings itemIds = 
  let n = M.size rankings
      m = length itemIds
      itemMap = M.fromList $ zip itemIds [0..]
      
      lookupRank :: UserID -> ItemID -> Double
      lookupRank uid iid = case M.lookup uid rankings >>= \r -> lookup iid r of
          Just rank -> fromIntegral rank
          Nothing   -> 0  -- handle missing rankings
          
      matrixList = [ [ lookupRank uid iid | iid <- itemIds ] | uid <- M.keys rankings ]
  in (n >< m) $ concat matrixList

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
svdOneDimensionalOrdering rankings itemIds =
  let users = M.keys rankings
      mat = centerMatrix $ rankingsToMatrix rankings itemIds
      -- use the correct svd api from hmatrix
      (u, _, _) = svd mat
      -- extract first left singular vector (users × 1)
      firstDimension = flatten $ takeColumns 1 u
  in sortOn snd $ zip users (VS.toList firstDimension)

-- | process pairwise comparisons into a complete ranking
pairwiseToRanking :: [(ItemID, ItemID)] -> [ItemID] -> Ranking
pairwiseToRanking comparisons allItems =
  let wins = foldr (\(winner, loser) acc -> M.insertWith (+) winner 1 acc) M.empty comparisons
      -- count wins for each item (items not in comparisons get 0)
      allWins = foldr (\item acc -> M.insertWith (\_ old -> old) item 0 wins) wins allItems
  in sortOn (Down . snd) $ M.toList allWins

-- | main function to process user preferences from pairwise comparisons
processPreferences :: M.Map UserID [(ItemID, ItemID)] -> [ItemID] -> [(UserID, Double)]
processPreferences pairwisePrefs allItems =
  let rankings = M.map (`pairwiseToRanking` allItems) pairwisePrefs
  in svdOneDimensionalOrdering rankings allItems

main :: IO ()
main = do
  let allItems = [1..5]
      -- user 1 prefers: 1 > 2 > 3 > 4 > 5
      user1Prefs = [(1,2), (1,3), (1,4), (1,5), (2,3), (2,4), (2,5), (3,4), (3,5), (4,5)]
      -- user 2 prefers: 5 > 4 > 3 > 2 > 1
      user2Prefs = [(5,4), (5,3), (5,2), (5,1), (4,3), (4,2), (4,1), (3,2), (3,1), (2,1)]
      -- user 3 prefers: 3 > 2 > 1 > 4 > 5
      user3Prefs = [(3,2), (3,1), (3,4), (3,5), (2,1), (2,4), (2,5), (1,4), (1,5), (4,5)]
      
      pairwisePrefs = M.fromList [(1, user1Prefs), (2, user2Prefs), (3, user3Prefs)]
      
  let ordering = processPreferences pairwisePrefs allItems
  putStrLn "One-dimensional ordering of users:"
  print ordering
