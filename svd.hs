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

-- calculate percentage of variance explained by each singular value
varianceExplained :: [Double] -> [Double]
varianceExplained singularValues =
  let totalVariance = sum $ map (^2) singularValues
      proportions = map (\s -> (s^2) / totalVariance) singularValues
  in proportions

-- update your svd function to report this information
svdAnalysis :: UserRankings -> [ItemID] -> [Double]
svdAnalysis rankings allItems =
  let users = M.keys rankings
      mat = centerMatrix $ rankingsToMatrix rankings allItems
      (u, s, _) = svd mat
      singularValues = VS.toList s
      firstDimension = flatten $ takeColumns 1 u
      explained = varianceExplained singularValues
  in explained

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

-- | compute multi-dimensional svd and return a one-dimensional ordering
svdWeightedOrdering :: UserRankings -> [ItemID] -> Int -> [(UserID, Double)]
svdWeightedOrdering rankings allItems dims =
  let users = M.keys rankings
      mat = centerMatrix $ rankingsToMatrix rankings allItems
      (u, s, _) = svd mat
      
      actualDims = min dims (VS.length s)
      uReduced = takeColumns actualDims u
      sReduced = VS.take actualDims s
      
      weights = VS.toList sReduced
      totalWeight = sum weights
      normWeights = map (/ totalWeight) weights
      
      userScores = map (\row -> 
        sum $ zipWith (*) (VS.toList row) normWeights) (toRows uReduced)
  in sortOn snd $ zip users userScores

-- | compute a one-dimensional ordering based on the principal curve
principalCurveOrdering :: UserRankings -> [ItemID] -> Int -> [(UserID, Double)]
principalCurveOrdering rankings allItems dims =
  let users = M.keys rankings
      mat = centerMatrix $ rankingsToMatrix rankings allItems
      (u, s, _) = svd mat
      
      actualDims = min dims (VS.length s)
      embedding = takeColumns actualDims u
      
      -- get embedded points in reduced space
      points = toRows embedding
      
      -- initial curve is the first principal component
      pcaDim1 = case toColumns embedding of
                  [] -> VS.replicate (rows embedding) 0
                  (v:_) -> VS.fromList $ VS.toList v
      
      -- create origin vector with safe length determination
      originSize = case points of
                     [] -> 0
                     (p:_) -> VS.length p
      origin = VS.replicate originSize 0
      
      -- calculate projections onto the principal curve
      projections = map (\p -> projectToCurve p pcaDim1 origin) points
  in sortOn snd $ zip users projections
  where
    -- project a point to the principal component line
    projectToCurve :: VS.Vector Double -> VS.Vector Double -> VS.Vector Double -> Double
    projectToCurve point direction origin =
      let vec = VS.zipWith (-) point origin
          dirNorm = sqrt $ VS.sum $ VS.map (^2) direction
          normalizedDir = if dirNorm > 0 
                          then VS.map (/ dirNorm) direction
                          else direction  -- avoid division by zero
          projection = VS.sum $ VS.zipWith (*) vec normalizedDir
      in projection

-- | find the number of singular values that explain at least target% of variance
componentsForVariance :: [Double] -> Double -> Int
componentsForVariance explained target = 
  let cumulative = scanl1 (+) explained
      sufficient = length $ takeWhile (< target) cumulative
  in sufficient + 1  -- number of components needed

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
        , (11, [1,4])
        ]

  let explained = svdAnalysis userRankings allItems
  putStrLn "SVD Analysis:"
  putStrLn $ show explained

  let targetVariance = 0.9
      numComponents = componentsForVariance explained targetVariance
  putStrLn $ "\nNumber of components explaining at least " ++ show (targetVariance * 100) ++ "% variance:"
  putStrLn $ show numComponents
  
  let ordering = svdOneDimensionalOrdering userRankings allItems
  putStrLn "\nOne-dimensional ordering:"
  putStrLn $ show ordering
  putStrLn $ show $ map fst ordering
  
  let ordering = svdWeightedOrdering userRankings allItems numComponents
  putStrLn "\nMulti-dimensional ordering:"
  putStrLn $ show ordering
  putStrLn $ show $ map fst ordering

  let ordering = principalCurveOrdering userRankings allItems numComponents
  putStrLn "\nPrincipal curve ordering:"
  putStrLn $ show ordering
  putStrLn $ show $ map fst ordering
