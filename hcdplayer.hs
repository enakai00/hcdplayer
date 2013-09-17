-- hcdplay.hs
--
-- ver1.0 2013/09/17 Tested with Fedora17/GHC7.0.4 
--
-- usage: hcdplay <directory of music files>
--

import System
import System.IO
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.Cmd (system)
import System.Console.Haskeline 
import Text.Printf (printf)
import Text.Regex.Posix ((=~))
import Data.List (sort)
import Data.Maybe
import List (nub)
import Control.Monad
import Codec.Binary.UTF8.String

type Albums = (String, [String]) -- (Artist, [Album])
type Dirpath = String

-- Misc functions --

puts = putStrLn . Codec.Binary.UTF8.String.decodeString

split :: String -> String -> (String,String)
split pat str = fstAndThird (str =~ pat :: (String,String,String))
    where fstAndThird (a, b, c)
              | a == str  = ("", "")
              | otherwise = (a, c)

addNumber :: [String] -> [String]
addNumber list = map (\(x,y) -> printf "%2d. %s" x y)
                     (zip ([1..] :: [Int]) list)

selectMenu :: String -> (Int -> Bool) -> IO Int
selectMenu ask range = runInputT defaultSettings $ loop ask range
    where loop :: String -> (Int -> Bool) -> InputT IO Int
          loop ask range = do
            n' <- getInputLine ask
            case n' of
                Nothing  -> return $ -1
                Just "q" -> return $ -1
                Just n   -> do
                    num <- case reads n of
                               [(x, "")] -> return x
                               _         -> return $ -99
                    if range num then return num
                                 else loop ask range -- Tail Recursion

main :: IO ()
main = do
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    topDir <- fmap head getArgs
    albums <- selectArtist topDir
    when (albums == Nothing) $ exitWith ExitSuccess
    albumDir <- selectAlbum topDir $ fromJust albums
    when (albumDir == Nothing) $ main               -- Tail Recursion
    playSongs $ fromJust albumDir
    main                                            -- Tail Recursion

selectArtist :: Dirpath -> IO (Maybe Albums)
selectArtist topDir = do
    dirs <- getDirectoryContents topDir
    let artists = filter ((/=)"") $
                  sort $ nub $ map (fst.split " - ") dirs 
    system "clear"
    puts "Welcome to cdplayer.\n"
    mapM_ puts $ addNumber artists
    puts ""
    num <- selectMenu "Select artist (q:quit)? "
                      (\x -> x > 0 && x <= length artists)
    let artist = artists !! (num - 1)
    let albums = sort $ filter ((/=)"") $
                 map (snd.split ( "^" ++ artist ++ " - ")) dirs
    if num == -1 then return Nothing
                 else return $ Just (artist, albums)

selectAlbum :: Dirpath -> Albums -> IO (Maybe Dirpath)
selectAlbum topDir (artist, albums) = do 
    system "clear"
    puts $ "Albums of " ++ artist ++ "\n"
    mapM_ puts $ addNumber $ albums
    puts ""
    num <- selectMenu "Select album (q:back)? "
                      (\x -> x > 0 && x <= length albums)
    let albumDir = topDir </> artist ++ " - " ++ (albums !! (num - 1))
    if num == -1 then return Nothing
                 else return $ Just albumDir

playSongs :: String -> IO()
playSongs albumDir = do
    system "clear"
    system $ "cd \"" ++ albumDir ++ "\";mplayer * 2>/dev/null" 
    return ()
