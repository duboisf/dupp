import Control.Monad (unless, when)
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)

main =
    getArgs >>= \args ->
    if length args < 2
       then doPrettyPrintDU args >> exitWith ExitSuccess
       else putStrLn usage >> exitFailure

usage :: String
usage = "usage: dupp [PATH]"

doPrettyPrintDU :: [String] -> IO ()
doPrettyPrintDU args =
    getDUOutput path >>=
    printGroups . formatSizeField 0 . groupSameSuffix . sortOnFst . parseOutput . lines
    where path = if null args then "." else head args
          sortOnFst = sortBy $ comparing fst

printGroups :: [[(Float, String, String)]] -> IO ()
printGroups list =
    putStrLn "" >>
    printGroups' list >>
    putStrLn seperator >>
    printf "    Total for %s: %0.1f %s\n" totalPath totalSize totalSuffix >>
    putStrLn ""
        where seperator = replicate 40 '-'
              lastRow@(totalSize, totalSuffix, totalPath) = last . last $ list
              printGroups' [] = return ()
              printGroups' (x:xs) =
                  printGroup x >>
                  unless (null xs) (putStrLn seperator >> printGroups' xs)
              printGroup [] = return ()
              printGroup (currentRow@(size, suffix, path):ys) =
                  when (currentRow /= lastRow) $
                     printf "%7.1f %s   %s\n" size suffix path >>
                     printGroup ys

data DirSize = DirSize {
      dsSize :: Int
    , dsPath :: FilePath
    }

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p l = partitionM' p l ([], [])
  where
    partitionM' :: Monad m => (a -> m Bool) -> [a] -> ([a], [a]) -> m ([a], [a])
    partitionM' _ [] r = return r
    partitionM' p (x:xs) (ys, zs) =
        p x >>= \bool ->
        if bool
           then partitionM' p xs (x:ys, zs)
           else partitionM' p xs (ys, x:zs)

getFilesRecursively :: FilePath -> IO [FilePath]
getFilesRecursively dir = getFilesRecursively' [dir]
  where
    getFilesRecursively' [] = return []
    getFilesRecursively' (dir:dirs) =
        getDirectoryContents dir >>=
        partitionM doesDirectoryExist . map (dir </>) . filter (`notElem` [".", ".."]) >>= \(newDirs, files) ->
        getFilesRecursively' (dirs ++ newDirs) >>= \newFiles ->
        return (files ++ newFiles)

--getDirSizes :: [FilePath] -> IO [DirSize]
--getDirSizes x:xs =
--    getDirectoryContents x >>= \contents ->
--    :

getDUOutput :: FilePath -> IO String
getDUOutput path =
    readProcessWithExitCode "du" ["--max-depth=1", path] "" >>= \(code, out, err) ->
    case code of
         ExitSuccess -> return out
         _           -> putStr err >> exitFailure

parseOutput :: [String] -> [(Integer, String)]
parseOutput []     = []
parseOutput (x:xs) = (read size :: Integer, tail path) : parseOutput xs
    where (size, path) = span (/= '\t') x

groupSameSuffix :: [(Integer, String)] -> [[(Integer, String)]]
groupSameSuffix l = group 1024 l
    where group :: Integer -> [(Integer, String)] -> [[(Integer, String)]]
          group _     [] = []
          group limit xs = currentGroup : group (limit * 1024) rest
              where (currentGroup, rest) = span (\x -> fst x < limit) xs

formatSizeField :: Int -> [[(Integer, String)]] -> [[(Float, String, String)]]
formatSizeField _ []     = []
formatSizeField n (x:xs) = map formatGroup x : formatSizeField (n + 1) xs
    where formatGroup (size, path) = (size', suffixes !! n, path)
              where size' = fromInteger size / (fromIntegral 1024 ^ n)

suffixes :: [String]
suffixes = ["KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"]

