{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent

import System.IO
import System.Process
import System.Environment

import           Data.Char
import           Data.List
import           Data.Either
import           Data.Array
import           Data.Array.IO
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Time.Clock
import           Data.Text (Text, pack)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Data.Attoparsec.Text as P

-- | MAIN LOOP | --

main :: IO ()
main = do hSetBuffering stdout LineBuffering
          as <- zipWithM (\n b -> ($n) <$> M.lookup b blocks) [1..] <$> getArgs
          case concat as of
            [] -> do p <- getProgName
                     putStrLn ("Usage: " ++ p ++ " [space-separated list of blocks]")
                     putStrLn ("Available blocks: " ++ intercalate ", " (M.keys blocks))
            bs -> do v <- newEmptyMVar
                     mapM_ (forkIO . ($v) . thread) bs
                     let l = length bs
                     ta <- newArray (1,l) ""
                     forever (update v (listArray (1,l) bs) ta)

update :: MVar Int -> Array Int Block -> IOArray Int Text -> IO ()
update v ba ta = do i <- takeMVar v
                    let b = ba ! i
                    new     <- parser b <$> command b
                    current <- readArray ta i
                    when (new /= current)
                      (do writeArray ta i new
                          s <- filter (not . T.null) <$> getElems ta
                          T.putStr "| "
                          T.putStr (T.intercalate " | " s)
                          T.putStrLn " ")

-- | POLLING | --

oneSecond :: Int
oneSecond = 1000000

pollMicroseconds :: Int -> Int -> MVar Int -> IO ()
pollMicroseconds n i v = forever (putMVar v i >> waitMicroseconds n)

waitMicroseconds :: Int -> IO ()
waitMicroseconds n = do ps <- fromIntegral . diffTimeToPicoseconds . utctDayTime <$> getCurrentTime
                        threadDelay (n - ps `quot` oneSecond `rem` n)

-- | BLOCKS | --

data Block = Block { command :: IO Text, parser :: Text -> Text, thread :: MVar Int -> IO () }

blocks :: Map String (Int -> Block)
blocks = M.fromList [("battery",   battery),
                     ("datetime",  datetime),
                     ("dropbox",   dropbox),
                     ("playerctl", playerctl),
                     ("volume",    volume)]

systemCommand :: String -> [String] -> IO Text
systemCommand c as = pack . (\(_,x,_) -> x) <$> readProcessWithExitCode c as ""

runParser :: Text -> Parser Text -> Text -> Text
runParser n p = fromRight (n <> ": parse error") . parseOnly p

battery :: Int -> Block
battery = Block c (runParser "battery" p) . pollMicroseconds (60 * oneSecond)
  where
    c :: IO Text
    c = systemCommand "acpi" []

    p :: Parser Text
    p = do string "Battery 0: "
           takeTill isDigit
           x <- P.takeWhile isDigit
           pure ("Battery: " <> x <> "%")

datetime :: Int -> Block
datetime = Block c (runParser "datetime" p) . pollMicroseconds oneSecond
  where
    c :: IO Text
    c = systemCommand "date" ["+%A %d/%m/%Y @ %H:%M:%S"]

    p :: Parser Text
    p = takeTill (=='\n')

dropbox :: Int -> Block
dropbox = Block c (runParser "dropbox" p) . pollMicroseconds oneSecond
  where
    c :: IO Text
    c = systemCommand "dropbox-cli" ["status"]

    p :: Parser Text
    p = choice [""                      <$ string "Up to date",
                "Dropbox: Not running!" <$ string "Dropbox isn't running!",
                "Dropbox: Syncing..."   <$ pure ()]

playerctl :: Int -> Block
playerctl = Block c (runParser "playerctl" p) . pollMicroseconds oneSecond
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
    p = takeTill (=='\n')

volume :: Int -> Block
volume = Block c (runParser "volume" p) . pollMicroseconds oneSecond
  where
    c :: IO Text
    c = systemCommand "amixer" ["sget","Master"]

    p :: Parser Text
    p = do takeTill (=='[') >> char '['
           x <- P.takeWhile isDigit
           takeTill (=='[') >> char '['
           b <- choice [True  <$ string "on",
                        False <$ string "off"]
           pure ("Volume: " <> if b
                                 then x <> "%"
                                 else "Muted")
