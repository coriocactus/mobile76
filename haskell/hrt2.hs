import qualified Data.Map.Strict as Map

solution arr pieces =
    let pieceMap = Map.fromList [(head p, p) | p <- pieces]
        match [] [] = True
        match [] _ = False
        match _ [] = False
        match x remainingPieces =
            case Map.lookup (head x) pieceMap of
                Just piece ->
                    let pieceLength = length piece
                    in take pieceLength x == piece && match (drop pieceLength x) (filter (/= piece) remainingPieces)
                Nothing -> False
    in match arr pieces
