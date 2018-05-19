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
import           Data.Text (Text, pack, unpack)
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
                                 r <- parseOnly (parser b) <$> command b
                                 writeArray ta i (either (const (name b <> ": parse error")) id r)
                                 s <- filter (not .T.null) <$> getElems ta
                                 T.putStrLn ("| " <> T.intercalate " | " s <> " "))

-- | POLLING | --

pollMicroseconds :: Int -> Int -> MVar Int -> IO ()
pollMicroseconds n i v = forever (putMVar v i >> waitMicroseconds n)

waitMicroseconds :: Int -> IO ()
waitMicroseconds n = do ps <- fromIntegral . diffTimeToPicoseconds . utctDayTime <$> getCurrentTime
                        threadDelay (n - ps `quot` 1000000 `rem` n)

-- | BLOCKS | --

data Block = Block { name :: Text,  command :: IO Text, parser :: Parser Text, thread :: MVar Int -> IO () }

blocks :: Map String (Int -> Block)
blocks = M.fromList [("volume",   volume),
                     ("battery",  battery),
                     ("datetime", datetime)]

datetime :: Int -> Block
datetime = Block "datetime" c p . pollMicroseconds 60000000
  where
    c :: IO Text
    c = pack <$> readProcess "date" ["+%A %d/%m/%Y @ %H:%M"] ""

    p :: Parser Text
    p = takeTill (=='\n')

battery :: Int -> Block
battery = Block "battery" c p . pollMicroseconds 60000000
  where
    c :: IO Text
    c = pack <$> readProcess "acpi" [] ""

    p :: Parser Text
    p = do string "Battery 0: "
           takeTill isDigit
           x <- P.takeWhile isDigit
           pure ("Battery: " <> x <> "%")

volume :: Int -> Block
volume = Block "volume" c p . pollMicroseconds 1000000
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
