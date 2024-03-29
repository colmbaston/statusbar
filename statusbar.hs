{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent

import System.IO
import System.Directory
import System.Process
import System.Environment

import Data.Char
import Data.List
import Data.Either
import Data.Functor
import Data.Array.IO
import Data.Map qualified as M
import Data.Map (Map)
import Data.Time.Clock
import Data.Text (Text, pack)
import Data.Text    qualified as T
import Data.Text.IO qualified as T
import Data.Attoparsec.Text as P

-- | MAIN LOOP | --

main :: IO ()
main = do hSetBuffering stdout LineBuffering
          as <- zipWithM (\n b -> ($ n) <$> M.lookup b blocks) [1 ..] <$> getArgs
          case concat as of
            [] -> do p <- getProgName
                     putStrLn ("Usage: " ++ p ++ " [space-separated list of blocks]")
                     putStrLn ("Available blocks: " ++ intercalate ", " (M.keys blocks))
            bs -> do v <- newEmptyMVar
                     mapM_ (forkIO . ($ v)) bs
                     ta <- newArray (1, length bs) ""
                     forever (update v ta)

update :: MVar (Int, Text) -> IOArray Int Text -> IO ()
update v ta = do (i, new) <- takeMVar v
                 current  <- readArray ta i
                 when (new /= current)
                   (do writeArray ta i new
                       s <- filter (not . T.null) <$> getElems ta
                       T.putStr "| "
                       T.putStr (T.intercalate " | " s)
                       T.putStrLn " ")

-- | POLLING | --

oneSecond :: Int
oneSecond = 1_000_000

oneMinute :: Int
oneMinute = 60 * oneSecond

pollMicroseconds :: Int -> IO Text -> Int -> MVar (Int, Text) -> IO ()
pollMicroseconds n p i v = forever (p >>= putMVar v . (i, ) >> waitMicroseconds n)

waitMicroseconds :: Int -> IO ()
waitMicroseconds n = do ps <- fromInteger . diffTimeToPicoseconds . utctDayTime <$> getCurrentTime
                        threadDelay (n - (ps `quot` 1_000_000) `rem` n)

watchFile :: FilePath -> IO Text -> Int -> MVar (Int, Text) -> IO ()
watchFile fp p i v = forever (do b <- doesFileExist fp
                                 if b
                                   then p >>= putMVar v . (i, ) >> callProcess "inotifywait" ["-qqre", "modify", fp]
                                   else threadDelay oneSecond)

-- | XML | --

data XML = Leaf Text | Node Text [XML] deriving Show

xmlParse :: Parser XML
xmlParse = choice [do void (char '<')
                      tag <- takeTill (=='>')
                      void (char '>')
                      cs  <- manyTill xmlParse (string "</")
                      void (string tag *> char '>')
                      pure (Node tag cs),
                   Leaf <$> takeTill (=='<')]

xmlIndex :: [Text] -> XML -> [XML]
xmlIndex []     xml           = pure xml
xmlIndex (t:ts) (Node tag cs) = guard (tag == t) *> msum (map (xmlIndex ts) cs)
xmlIndex _             _      = mzero

-- | BLOCKS | --

type Block = MVar (Int, Text) -> IO ()

blocks :: Map String (Int -> Block)
blocks = M.fromList [("battery",   battery),
                     ("datetime",  datetime),
                     ("dropbox",   dropbox),
                     ("solaredge", solaredge),
                     ("sync",      sync),
                     ("playerctl", playerctl),
                     ("volume",    volume)]

systemCommand :: String -> [String] -> IO Text
systemCommand c as = pack . (\(_, x, _) -> x) <$> readProcessWithExitCode c as ""

runParser :: Text -> Parser Text -> Text -> Text
runParser n p = fromRight (n <> ": parse error") . parseOnly p

battery :: Int -> Block
battery = pollMicroseconds oneMinute (runParser "battery" p <$> c)
  where
    c :: IO Text
    c = systemCommand "acpi" []

    p :: Parser Text
    p = do void (string "Battery 0: ")
           void (takeTill isDigit)
           x <- P.takeWhile isDigit
           pure ("Battery: " <> x <> "%")

datetime :: Int -> Block
datetime = pollMicroseconds oneSecond (runParser "datetime" p <$> c)
  where
    c :: IO Text
    c = systemCommand "date" ["+%A %d/%m/%Y @ %H:%M:%S"]

    p :: Parser Text
    p = takeTill (=='\n')

dropbox :: Int -> Block
dropbox = pollMicroseconds oneSecond (runParser "dropbox" p <$> c)
  where
    c :: IO Text
    c = systemCommand "dropbox-cli" ["status"]

    p :: Parser Text
    p = choice [""                      <$ string "Up to date",
                "Dropbox: Not running!" <$ string "Dropbox isn't running!",
                "Dropbox: Syncing..."   <$ pure ()]

solaredge :: Int -> Block
solaredge = pollMicroseconds (5 * oneMinute) (runParser "solaredge" p <$> c)
  where
    c :: IO Text
    c = do msite <- lookupEnv "SOLAR_EDGE_SITE"
           mapi  <- lookupEnv "SOLAR_EDGE_API_KEY"
           case sequence [msite, mapi] of
             Nothing          -> hPutStrLn stderr "SOLAR_EDGE environment variables not set" $> ""
             Just [site, api] -> systemCommand "curl" ["https://monitoringapi.solaredge.com/sites/" ++ site ++ "/overview.xml?api_key=" ++ api]
             _                -> error "impossible"

    p :: Parser Text
    p = do xml <- xmlIndex ["siteOverviewList", "dateValueSeries", "siteOverview"] <$> xmlParse
           let xmlPower  = concatMap (xmlIndex ["currentPower", "power"])  xml
               xmlEnergy = concatMap (xmlIndex ["lastDayData",  "energy"]) xml
           case xmlPower ++ xmlEnergy of
             [Leaf power, Leaf energy] -> pure ("Current Power: " <> power <> " W, Energy Today: " <> energy <> " Wh")
             _                         -> mzero

sync :: Int -> Block
sync = watchFile fp (runParser "sync" p <$> c)
  where
    fp :: FilePath
    fp = "/home/colm/.cache/sync-status"

    c :: IO Text
    c = do b <- doesFileExist fp
           if b
             then T.pack <$> readFile fp
             else pure ""

    p :: Parser Text
    p = do status <- takeTill (== '\n')
           pure (if status == "waiting" then "" else T.append "Sync Status: " status)

playerctl :: Int -> Block
playerctl = pollMicroseconds oneSecond (runParser "playerctl" p <$> c)
  where
    c :: IO Text
    c = do t <- systemCommand "playerctl" ["metadata", "title"]
           a <- systemCommand "playerctl" ["metadata", "artist"]
           if T.null t || T.null a
              then pure ""
              else do s <- T.init <$> systemCommand "playerctl" ["status"]
                      let statusText = if s /= "Playing" then " [" <> s <> "]" else ""
                      pure (T.init t <> " - " <> T.init a <> statusText <> "\n")

    p :: Parser Text
    p = takeTill (== '\n')

volume :: Int -> Block
volume = pollMicroseconds oneSecond (runParser "volume" p <$> c)
  where
    c :: IO Text
    c = systemCommand "amixer" ["sget","Master"]

    p :: Parser Text
    p = do takeTill (=='[') >> void (char '[')
           x <- P.takeWhile isDigit
           takeTill (=='[') >> void (char '[')
           b <- choice [True  <$ string "on",
                        False <$ string "off"]
           pure ("Volume: " <> if b
                                 then x <> "%"
                                 else "Muted")
