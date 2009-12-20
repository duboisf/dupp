import Data.List (sortBy)
import System.Environment (getArgs)
import System.Exit
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
          sortOnFst = sortBy (\x y -> fst x `compare` fst y)

printGroups :: [[(Float, String, String)]] -> IO ()
printGroups list =
    putStrLn "" >>
    printGroups' list >>
    putStrLn seperator >>
    printf "    Total for %s: %0.1f %s\n" totalPath totalSize totalSuffix >>
    putStrLn ""
        where seperator = take 40 . repeat $ '-'
              lastRow@(totalSize, totalSuffix, totalPath) = last . last $ list
              printGroups' [] = return ()
              printGroups' (x:xs) =
                  printGroup x >>
                  if null xs
                     then return ()
                     else putStrLn seperator >>
                          printGroups' xs
              printGroup [] = return ()
              printGroup (currentRow@(size, suffix, path):ys) =
                  if currentRow /= lastRow
                     then printf "%7.1f %s   %s\n" size suffix path >>
                          printGroup ys
                     else return ()

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
          group limit xs = let (currentGroup, rest) = span (\x -> fst x < limit) xs
                               in [currentGroup] ++ group (limit * 1024) rest

formatSizeField :: Int -> [[(Integer, String)]] -> [[(Float, String, String)]]
formatSizeField _ []     = []
formatSizeField n (x:xs) = formatGroup x : formatSizeField (n + 1) xs
    where formatGroup :: [(Integer, String)] -> [(Float, String, String)]
          formatGroup [] = []
          formatGroup ((size, path):rest) = (size', suffixes !! n, path) : formatGroup rest
            where size' = (fromInteger size) / (fromIntegral 1024 ^ n)

suffixes :: [String]
suffixes = ["KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"]

