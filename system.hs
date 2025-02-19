import qualified System.Process as Process
import qualified System.Directory as Dir

repoRoot = "/Users/joshwong"
localRepo = "/Users/joshwong/catalhoyuk"
remoteRepo = "git@cordcivilian:cordcivilian/catalhoyuk.git"

cloneRepo :: IO ()
cloneRepo = do
    putStrLn "local repo not found, cloning"
    Dir.setCurrentDirectory repoRoot
    Process.callProcess "git" ["clone", remoteRepo]

fetchLatestVersion :: IO ()
fetchLatestVersion = do
    putStrLn "local repo found, fetching latest version"
    Dir.setCurrentDirectory localRepo
    Process.callProcess "git" ["fetch", "-q", remoteRepo, "main"]
    Process.callProcess "git" ["reset", "--hard", "origin/main"]
    Process.callProcess "git" ["clean", "-dxqf"]

main :: IO ()
main = do 
    exists <- Dir.doesDirectoryExist localRepo
    if exists then fetchLatestVersion else cloneRepo

