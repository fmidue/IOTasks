module Test.IOTasks.Output (
  Output, newOutput,
  putT, putLnT, printT,
  putP, putLnP, printP,
  oFlush
  ) where

import Control.Monad.Extra (ifM)

import System.Console.ANSI
import System.IO (Handle, hPutStr, hFlush)
import Data.IORef

data Output = Output
  { handle :: Handle
  , _tempSize :: IORef TextDim
  , _tempOutput :: IORef TextDim -> Handle -> String -> IO ()
  , _permanentOutput :: IORef TextDim -> Handle -> String -> IO ()
  }

oFlush :: Output -> IO ()
oFlush = hFlush . handle

data TextDim = TextDim { width :: Int, height :: Int, offset :: Int } deriving (Eq, Show)

emptyDim :: TextDim
emptyDim = TextDim 0 1 0

stringDim :: String -> TextDim
stringDim "" = emptyDim
stringDim s = TextDim (maximum $ map length ls) (length ls) (nonEmptyLength $ last ls)
  where
    ls = lines' s

nonEmptyLength :: String -> Int
nonEmptyLength = length . dropWhile (== ' ') . reverse

lines' :: String -> [String]
lines' s
  | last s == '\n' = lines s ++ [""]
  | otherwise = lines s

shrinkDim :: TextDim -> TextDim -> TextDim
shrinkDim (TextDim w h o) (TextDim x y z) = TextDim (max 0 $ w-x) newHeight newOffset
  where
    newHeight = max 1 $ h-(y-1)
    newOffset
      | newHeight < h = z
      | otherwise = max 0 $ o + z

newOutput :: Handle -> IO Output
newOutput h = do
  dRef <- newIORef emptyDim
  ifM (hSupportsANSI h)
    (pure $ Output h dRef (resetCursorAndWrite False) (resetCursorAndWrite True))
    (pure $ Output h dRef (const hPutStr') (const hPutStr))
  where
    hPutStr' h s = hPutStr h $ if last s == '\n' then s else s ++ "\n"

resetCursorAndWrite :: Bool -> IORef TextDim -> Handle -> String -> IO ()
resetCursorAndWrite perm dRef h s = do
  d <- readIORef dRef
  hCursorUpLine h (height d - 1)
  hCursorForward h (offset d)
  let s' = padString d s
  hPutStr h s'
  writeIORef dRef $ if perm
    then shrinkDim d (stringDim s)
    else (stringDim s'){offset = offset d}

padString :: TextDim -> String -> String
padString d s = unlines' . map (extendTo (width d) ' ') . extendTo (height d) "" $ lines' s
  where
    extendTo :: Int -> a -> [a] -> [a]
    extendTo n x xs = xs ++ replicate (n - length xs) x
    unlines' :: [String] -> String
    unlines' xs
      | newLineTerminated = unlines (init xs) ++ last xs
      | otherwise = init $ unlines xs
      where newLineTerminated = last s == '\n'

putT :: Output -> String -> IO ()
putT (Output h d f _) = f d h

putLnT :: Output -> String -> IO ()
putLnT h = putT h . (++ "\n")

printT :: Show a => Output -> a -> IO ()
printT h = putLnT h . show

putP :: Output -> String -> IO ()
putP (Output h d _ g) = g d h

putLnP :: Output -> String -> IO ()
putLnP h = putP h . (++ "\n")

printP :: Show a => Output -> a -> IO ()
printP h = putLnP h . show
