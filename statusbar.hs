{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent

import System.IO
import System.Process
import System.Environment

import           Data.Char
import           Data.List
import           Data.Monoid
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
                     updateLoop v (listArray (1,l) bs) ta

updateLoop :: MVar Int -> Array Int Block -> IOArray Int Text -> IO ()
updateLoop v ba ta = forever (do i <- takeMVar v
                                 let b = ba ! i
                                 new     <- parser b <$> command b
                                 current <- readArray ta i
                                 when (new /= current)
                                   (do writeArray ta i new
                                       s <- filter (not . T.null) <$> getElems ta
                                       T.putStr "| "
                                       T.putStr (T.intercalate " | " s)
                                       T.putStrLn " "))

-- | POLLING | --

pollMicroseconds :: Int -> Int -> MVar Int -> IO ()
pollMicroseconds n i v = forever (putMVar v i >> waitMicroseconds n)

waitMicroseconds :: Int -> IO ()
waitMicroseconds n = do ps <- fromIntegral . diffTimeToPicoseconds . utctDayTime <$> getCurrentTime
                        threadDelay (n - ps `quot` 1000000 `rem` n)

-- | BLOCKS | --

data Block = Block { command :: IO Text, parser :: Text -> Text, thread :: MVar Int -> IO () }

blocks :: Map String (Int -> Block)
blocks = M.fromList [("battery",  battery),
                     ("datetime", datetime),
                     ("dropbox",  dropbox),
                     ("volume",   volume)]

runParser :: Text -> Parser Text -> Text -> Text
runParser n p = either (const (n <> ": parse error")) id . parseOnly p

battery :: Int -> Block
battery = Block c (runParser "battery" p) . pollMicroseconds 60000000
  where
    c :: IO Text
    c = pack <$> readProcess "acpi" [] ""

    p :: Parser Text
    p = do string "Battery 0: "
           takeTill isDigit
           x <- P.takeWhile isDigit
           pure ("Battery: " <> x <> "%")

datetime :: Int -> Block
datetime = Block c (runParser "datetime" p) . pollMicroseconds 60000000
  where
    c :: IO Text
    c = pack <$> readProcess "date" ["+%A %d/%m/%Y @ %H:%M"] ""

    p :: Parser Text
    p = takeTill (=='\n')

dropbox :: Int -> Block
dropbox = Block c (runParser "dropbox" p) . pollMicroseconds 1000000
  where
    c :: IO Text
    c = pack <$> readProcess "dropbox-cli" ["status"] ""

    p :: Parser Text
    p = choice [const ""                    <$> string "Up to date",
                const "Dropbox: Syncing..." <$> pure ()]

volume :: Int -> Block
volume = Block c (runParser "volume" p) . pollMicroseconds 1000000
  where
    c :: IO Text
    c = pack <$> readProcess "amixer" ["sget","Master"] ""

    p :: Parser Text
    p = do takeTill (=='[')
           char '['
           x <- P.takeWhile isDigit
           replicateM_ 2 (takeTill (=='[') >> char '[')
           b <- choice [const True  <$> string "on",
                        const False <$> string "off"]
           pure ("Volume: " <> if b
                                 then x <> "%"
                                 else "Muted")
