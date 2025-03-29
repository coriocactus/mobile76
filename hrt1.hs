import Data.List (sortOn)
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map

splitter p s = case dropWhile p s of
  "" -> []
  s' -> w : splitter p s''
    where (w, s'') = break p s'

parseMatch :: String -> (String, Int, Int, String)
parseMatch match =
  let [team_one_part, team_two_part] = splitter (== ':') match
      [team_one_name, team_one_goal] = splitter (== ' ') team_one_part
      (team_two_goal, team_two_name) =
        case splitter (== ' ') team_two_part of
          (g:rest) -> (g, unwords rest)
          []       -> error "Invalid match format: missing team two data"
  in (team_one_name, read team_one_goal :: Int, read team_two_goal :: Int, team_two_name)

computePoints team_one_goal team_two_goal
  | team_one_goal > team_two_goal = (3, 0)
  | team_one_goal < team_two_goal = (0, 3)
  | otherwise = (1, 1)

postMatchScoreboard scoreboard match =
  let (team_one_name, team_one_goal, team_two_goal, team_two_name) = parseMatch match
      (team_one_point, team_two_point) = computePoints team_one_goal team_two_goal
  in Map.insertWith (+) team_one_name team_one_point $ Map.insertWith (+) team_two_name team_two_point scoreboard

solution matches =
  let finalScoreboard = foldl postMatchScoreboard Map.empty matches
      sortedTeams = sortOn (Down . snd) $ Map.toList finalScoreboard
  in map (\(team, points) -> team ++ " " ++ show points) sortedTeams

main = do
  print $ parseMatch "Liverpool 3:2 PSG"
  print $ solution ["Liverpool 3:2 PSG", "RedStar 0:0 Napoli", "PSG 6:1 RedStar", "Napoli 1:0 Liverpool"]
