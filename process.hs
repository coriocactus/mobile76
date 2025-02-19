{- cabal:
    build-depends:
        base,
        process,
        directory,
        async,
-}

import System.Process
import System.Exit
import System.Directory
import Control.Concurrent.Async (concurrently)

runProcessAndWait :: FilePath -> [String] -> IO ExitCode
runProcessAndWait cmd args = do
    (_, _, _, ph) <- createProcess (proc cmd args)
    waitForProcess ph
    (_, _, _, ph) <- createProcess (proc cmd args)
    waitForProcess ph

main :: IO ()
main = do
    setCurrentDirectory "/Users/joshwong/mobile76/haskell/cord"
    (exitCode, exitCode2) <- concurrently (runProcessAndWait "git"
        [ "fetch"
        , "-q"
        , "--atomic"
        , "https://github.com/cordcivilian/cord.git"
        , "main"
        ]) (runProcessAndWait "git"
        [ "fetch"
        , "-q"
        , "--atomic"
        , "https://github.com/cordcivilian/cord.git"
        , "main"
        ])
    print $ show (exitCode, exitCode2)
    (exitCode, exitCode2) <- concurrently (runProcessAndWait "git"
        [ "reset"
        , "--hard"
        , "origin/main"
        ]) (runProcessAndWait "git"
        [ "reset"
        , "--hard"
        , "origin/main"
        ])
    print $ show (exitCode, exitCode2)
