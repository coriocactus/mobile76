data Player = Player
  { playerName :: String
  , playerRating :: Double
  } deriving (Show, Eq)

data MatchResult = Win | Draw | Loss deriving (Show, Eq)

createPlayer :: String -> Player
createPlayer name = Player name 1500.0

calculateExpectedScore :: Player -> Player -> Double
calculateExpectedScore a b =
  1.0 / (1.0 + 10.0 ** ((bRating - aRating) / 400.0))
  where
    aRating = playerRating a
    bRating = playerRating b

resultToScore :: MatchResult -> Double
resultToScore Win  = 1.0
resultToScore Draw = 0.5
resultToScore Loss = 0.0

updateRating :: Double -> Player -> Player -> MatchResult -> Player
updateRating kFactor player opponent result =
  player { playerRating = newRating }
  where
    expected = calculateExpectedScore player opponent
    actual = resultToScore result
    newRating = playerRating player + kFactor * (actual - expected)

updateRatings :: Player -> Player -> MatchResult -> (Player, Player)
updateRatings player1 player2 result =
  (updateRating 32 player1 player2 result, updateRating 32 player2 player1 (flipResult result))
  where
    flipResult Win  = Loss
    flipResult Loss = Win
    flipResult Draw = Draw

main :: IO ()
main = do
  let alice = createPlayer "Alice"
      bob = createPlayer "Bob"

  putStrLn "Initial ratings:"
  putStrLn $ "Alice: " ++ show (playerRating alice)
  putStrLn $ "Bob: " ++ show (playerRating bob)

  let (alice', bob') = updateRatings alice bob Win

  putStrLn ""
  putStrLn "After Alice wins against Bob:"
  putStrLn $ "Alice: " ++ show (playerRating alice')
  putStrLn $ "Bob: " ++ show (playerRating bob')

  let (alice'', bob'') = updateRatings alice' bob' Loss

  putStrLn ""
  putStrLn "After Bob wins against Alice:"
  putStrLn $ "Alice: " ++ show (playerRating alice'')
  putStrLn $ "Bob: " ++ show (playerRating bob'')
