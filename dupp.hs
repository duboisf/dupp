import Control.Exception (bracket)
import Control.Monad (filterM, liftM, unless, when)
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit
import System.FilePath ((</>))
import System.IO (hClose, hFileSize, openFile, IOMode(ReadMode))
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

getDUOutput = undefined

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
      dsSize :: Integer
    , dsPath :: FilePath
    } deriving Show

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

getDirSizes :: Int -> FilePath -> IO [DirSize]
getDirSizes maxDepth path = getDirSizes' 1 [path]
  where
    getDirSizes' :: Int -> [FilePath] -> IO [DirSize]
    getDirSizes' _ [] = return []
    getDirSizes' depth (path:paths) =
        if depth < maxDepth
           then getDirsAndFiles path >>= \(dirs, files) ->
                getDirSizes' (depth + 1) (dirs ++ paths) >>= \dirSizes ->
                return $ DirSize (foldr (\x y -> dsSize x + y) 0 dirSizes) path : dirSizes
           else getFilesRecursively path >>= \files ->
                mapM getFileSize files >>= \fileSizes ->
                fmap (DirSize (sum fileSizes) path :) (getDirSizes' (depth + 1) paths)

getFileSize :: FilePath -> IO Integer
getFileSize path = bracket (openFile path ReadMode) hClose hFileSize

getDirContents :: FilePath -> IO [FilePath]
getDirContents p = return . filter (`notElem` [".", ".."]) =<< getDirectoryContents p

getDirsAndFiles :: FilePath -> IO ([FilePath], [FilePath])
getDirsAndFiles path = partitionM doesDirectoryExist =<< getDirContents path

getDirs :: FilePath -> IO [FilePath]
getDirs = liftM fst . getDirsAndFiles

getFiles :: FilePath -> IO [FilePath]
getFiles = liftM snd . getDirsAndFiles

getFilesRecursively :: FilePath -> IO [FilePath]
getFilesRecursively dir = getFilesRecursively' [dir]
  where
    getFilesRecursively' [] = return []
    getFilesRecursively' (dir:dirs) =
        getDirectoryContents dir >>=
        partitionM doesDirectoryExist . map (dir </>) . filter (`notElem` [".", ".."]) >>= \(newDirs, files) ->
        fmap (files ++) $ getFilesRecursively' (dirs ++ newDirs)

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

