import Control.Monad (unless, when)
import Data.List (delete, sortBy)
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)

main =
    getArgs >>=
    fixArgs >>= \args ->
    doPrettyPrintDU args >> exitWith ExitSuccess

fixArgs :: [String] -> IO [String]
fixArgs args =
    if "-h" `elem` args
       then hPutStrLn stderr "(Warning: ignoring -h parameter)\n" >>
            return (delete "-h" args)
       else return args

usage :: String
usage = "usage: dupp [PATH]"

doPrettyPrintDU :: [String] -> IO ()
doPrettyPrintDU args =
    getDUOutput args >>=
    printGroups . formatSizeField 0 . groupSameSuffix . sortOnFst . parseOutput . lines
        where sortOnFst = sortBy $ comparing fst

printGroups :: [[(Float, String, String)]] -> IO ()
printGroups list =
    printGroups' list >>
    putStrLn "" >>
    printf "%7.1f %s   %s\n" totalSize totalSuffix totalPath
        where lastRow@(totalSize, totalSuffix, totalPath) = last . last $ list
              printGroups' [] = return ()
              printGroups' (x:xs) =
                  printGroup x >>
                  unless (null x || null xs || xs == [[lastRow]]) (putStrLn "") >>
                  printGroups' xs
              printGroup [] = return ()
              printGroup (currentRow@(size, suffix, path):ys) =
                  when (currentRow /= lastRow) $
                     printf "%7.1f %s   %s\n" size suffix path >>
                     printGroup ys

getDUOutput :: [String] -> IO String
getDUOutput args =
    readProcessWithExitCode "du" args "" >>= \(code, out, err) ->
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

