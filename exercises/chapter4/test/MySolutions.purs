module Test.MySolutions where

import Prelude
import Data.Array (null, head, tail, filter, (..), cons, elem, any, (:))
import Data.Maybe (fromMaybe)
import Test.Examples
import Control.Alternative (guard)
import Data.Maybe (Maybe(..), isNothing)
import Data.Foldable (foldl, foldr)
import Data.Path

infix 5 filter as <$?>

-- Note to reader: Add your solutions to this file
isEven :: Int -> Boolean
isEven n = 
    if n < 0 then
        isEven(-n)
    else if n == 0 then
        true
    else if n == 1 then
        false
    else
        isEven (n - 2)

countEven :: Array Int -> Int
countEven arr = 
    if null arr then
        0
    else 
        if isEven $ fromMaybe 1 $ head arr then
            1 + countEven arrTail
        else
            countEven arrTail
    where
    arrTail :: Array Int
    arrTail = (fromMaybe [] $ tail arr)

squared :: Array Number -> Array Number
squared arr = map (\n -> n*n) arr

keepNonNegative :: Array Number -> Array Number
keepNonNegative arr = filter (\n -> n >= 0.0) arr

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite arr = (\n -> n >= 0.0) <$?> arr

isPrime :: Int -> Boolean
isPrime n =  n > 1 && (length $ factors n) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct arr1 arr2 = do
    x <- arr1
    y <- arr2
    pure [x, y]

triples :: Int -> Array (Array Int)
triples n = do
    x <- 1 .. n
    y <- x .. n
    z <- y .. n
    guard $ (x * x) + (y * y) == (z * z)
    pure [x, y, z]

primeFactors :: Int -> Array Int
primeFactors n = do
    p <- findPrimeH n
    guard $ p > 1
    cons p $ primeFactors (n / p)
    where
        findPrimeH :: Int -> Array Int
        findPrimeH i = [fromMaybe 0 $ head <<< filter (isPrime && (\x -> i `mod` x == 0)) $ 2 .. i]

allTrue :: Array Boolean -> Boolean
allTrue = foldl (\x acc -> acc && x) true

fibTailRec :: Int -> Int
fibTailRec 0 = 0
fibTailRec 1 = 1
fibTailRec n = fib' n 2 0 1
  where
  fib' :: Int -> Int -> Int -> Int -> Int
  fib' limit count n1 n2 =
    if limit == count then
      n1 + n2
    else
      fib' limit (count + 1) n2 (n1 + n2)

reverser :: forall a. Array a -> Array a
reverser = foldr (\x xs -> xs <> [x]) []

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> cons x xs) []

onlyFiles :: Path -> Array Path
onlyFiles = filter (not isDirectory) <<< allFiles

onlyDirectories :: Path -> Array Path
onlyDirectories = filter isDirectory <<< allFiles

whereIs :: Path -> String -> Maybe Path
whereIs path name = searchFile path (onlyDirectories path)
    where
    searchFile :: Path -> Array Path -> Maybe Path
    searchFile p ps = do
        hp <- head ps
        tp <- tail ps
        if any (\x -> (filename x) == (filename p) <> name) (ls p) then
            pure p
        else
            searchFile hp tp

-- これはわからんわ
largestSmallest :: Path -> Array Path
largestSmallest path = foldl loop [] (onlyFiles path) where
  loop :: Array Path -> Path -> Array Path
  loop [largest, smallest] current | size current < size smallest = [largest, current]
                                   | size current > size largest  = [current, smallest]
                                   | otherwise                    = [largest, smallest]
  loop [last] current              | size current > size last     = [current, last]
                                   | otherwise                    = [last, current]
  loop arr current                                                = current : arr
