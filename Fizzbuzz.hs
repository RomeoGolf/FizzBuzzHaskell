module Fizzbuzz where

import Control.Applicative
import Data.Maybe (fromJust)
import Control.Monad.Reader
import Control.Monad.State

{-
 - Write a program that prints the numbers from 1 to 100.
 - But for multiples of three print “Fizz” instead of the
 - number and for the multiples of five print “Buzz”.
 - For numbers which are multiples of both three and
 - five print “FizzBuzz”
 -}

-- first stupid literally recursive solution
fizzbuzz1 :: [String]
fizzbuzz1 = helper 1 where
    helper :: Int -> [String]
    helper 101 = []             -- stopper
    helper n
        | n `mod` 5 == 0 && n `mod` 3 == 0 = "fizzbuzz" : helper (n + 1)
        | n `mod` 3 == 0 = "fizz" : helper (n + 1)
        | n `mod` 5 == 0 = "buzz" : helper (n + 1)
        | otherwise = show n : helper (n + 1)

-- a bit trickly stupid solution with "case..of"
fizzbuzz2 :: [String]
fizzbuzz2 = helper 1 where
    helper :: Int -> [String]
    helper 101 = []
    helper n = case n `mod` 15 of
        0 -> "fizzbuzz" : helper (n + 1)
        x | (x `elem` [3, 6, 9, 12]) -> "fizz" : helper (n + 1)
        x | (x `elem` [5, 10]) -> "buzz" : helper (n + 1)
        _ -> show n : helper (n + 1)

-- with a list comprehension
fizzbuzz3 :: [String]
fizzbuzz3 = [f x | x <- [1..100]] where
    f n
        | n `mod` 5 == 0 && n `mod` 3 == 0 = "fizzbuzz"
        | n `mod` 3 == 0 = "fizz"
        | n `mod` 5 == 0 = "buzz"
        | otherwise = show n

-- with a map
fizzbuzz4 :: [String]
fizzbuzz4 = map f [1..100] where
    f :: Int -> String
    f n
        | n `mod` 15 == 0 = "fizzbuzz"
        | n `mod` 3 == 0 = "fizz"
        | n `mod` 5 == 0 = "buzz"
        | otherwise = show n

-- stupid solution with a list monad and do-notation
fizzbuzz5 :: [String]
fizzbuzz5 = do
    x <- [1..100]
    if x `mod` 15 == 0
        then return "fizzbuzz"
        else if x `mod` 3 == 0
            then return "fizz"
            else if x `mod` 5 == 0
                then return "buzz"
                else return (show x)

-- with an Alternative
fizzbuzz6 :: [String]
fizzbuzz6 = let
    -- if match to a multiple then return corresponding string
    match :: (Int -> Int -> Bool) -> Int -> String -> Int -> Maybe String
    match p mult s n
        | p mult n = Just s
        | otherwise = Nothing

    pred :: Int -> Int -> Bool          -- predicate on a multiple
    pred mult n = (n `mod` mult) == 0

    -- alternatives:
    a15 = match pred 15 "fizzbuzz"
    a3 = match pred 3 "fizz"
    a5 = match pred 5 "buzz"
    aOther = \n -> Just (show n)

    toFizzBuzz n = fromJust $ a15 n <|> a3 n <|> a5 n <|> aOther n 

    in map toFizzBuzz [1..100]

-- with a Reader monad
fizzbuzz7 :: [String]
fizzbuzz7 = let
    readN :: Int -> String -> String -> Reader Int String
    readN mult outStr inStr =
        case inStr of
            [] -> do
                r <- ask
                if r `mod` mult == 0 then return outStr
                    else return []
            _ -> return inStr

    readOther, read3, read5, read15 :: String -> Reader Int String
    read3 = readN 3 "fizz"
    read5 = readN 5 "buzz"
    read15 = readN 15 "fizzbuzz"
    readOther [] = do
            r <- ask
            return $ show r
    readOther s = return s
    {-
    readOther s = case s of
        [] -> do
            r <- ask
            return $ show r
        _ -> return s
    -}

    in map (runReader (return "" >>= read15 >>= read3 >>= read5 >>= readOther)) [1..100]

-- mad composition
fizzbuzz8 :: [String]
fizzbuzz8 = (f3. f5 . f15 . show) <$> [1..100] where
    f15 x = if (read x) `mod` 15 == 0 then "fizzbuzz" else x
    f5 "fizzbuzz" = "fizzbuzz"
    f5 x = if (read x) `mod` 5 == 0 then "buzz" else x
    f3 "fizzbuzz" = "fizzbuzz"
    f3 "buzz" = "buzz"
    f3 x = if (read x) `mod` 3 == 0 then "fizz" else x


-- parser for an Int list

newtype ParserInt a = ParserInt {apply :: [Int] -> [(a, [Int])]}

instance Functor ParserInt where
    fmap f p = ParserInt f' where
        f' intList = [(f a, intList') | (a, intList') <- apply p intList]

instance Applicative ParserInt where
    pure a = ParserInt f where
        f intList = [(a, intList)]
    pf <*> pv = ParserInt f where
        f l = [(g a, l'') | (g, l') <- apply pf l, (a, l'') <- apply pv l']

instance Alternative ParserInt where
    empty = ParserInt f where
        f _ = []
    p <|> q = ParserInt f where
        f l = let ps = apply p l
            in if null ps
                then apply q l
                else ps

anyNumber :: ParserInt String
anyNumber = ParserInt f where
    f [] = []
    f (x:xs) = [(show x, xs)]

satisfy :: (Int -> Bool) -> String -> ParserInt String
satisfy pred outStr = ParserInt f where
    f [] = []
    f (x:xs) | pred x = [(outStr, xs)]
             | otherwise = []

predMod :: Int -> Int -> Bool       -- predicate for the 'satisfy' function
predMod mult x = x `mod` mult == 0

{-mod3, mod5, mod15 :: ParserInt String-}
mod3  = satisfy (predMod 3)  "fizz"
mod5  = satisfy (predMod 5)  "buzz"
mod15 = satisfy (predMod 15) "fizzbuzz"

fizzbuzz9Parser :: ParserInt [String]
fizzbuzz9Parser = (:) <$> (mod15 <|> mod3 <|> mod5 <|> anyNumber) <*> fizzbuzz9Parser <|> pure []

fizzbuzz9 :: [String]
fizzbuzz9 = (fst . head) $ apply fizzbuzz9Parser [1..100]

-- evalState fizzbuzz10 1
fizzbuzz10 :: StateT Int IO ()
fizzbuzz10 = do
  int <- get
  let lp = lift . putStrLn
  when (mod int 15 == 0) (lp "fizzbuzz")
  when (mod int 5 == 0) (lp "buzz")
  when (mod int 3 == 0) (lp "fizz")
  unless (mod int 15 == 0 || mod int 5 == 0 || mod int 3 == 0) (lp $ show int)
  put (int + 1)
  unless (int == 100) fizzbuzz10

-- ****************** --
testData = [ "1", "2","fizz", "4","buzz","fizz", "7", "8","fizz","buzz","11","fizz","13","14","fizzbuzz",
            "16","17","fizz","19","buzz","fizz","22","23","fizz","buzz","26","fizz","28","29","fizzbuzz",
            "31","32","fizz","34","buzz","fizz","37","38","fizz","buzz","41","fizz","43","44","fizzbuzz",
            "46","47","fizz","49","buzz","fizz","52","53","fizz","buzz","56","fizz","58","59","fizzbuzz",
            "61","62","fizz","64","buzz","fizz","67","68","fizz","buzz","71","fizz","73","74","fizzbuzz",
            "76","77","fizz","79","buzz","fizz","82","83","fizz","buzz","86","fizz","88","89","fizzbuzz",
            "91","92","fizz","94","buzz","fizz","97","98","fizz","buzz"]
passed = "Passed"
failed = "Failed"
tests = [fizzbuzz1, fizzbuzz2, fizzbuzz3, fizzbuzz4, fizzbuzz5, fizzbuzz6, fizzbuzz7, fizzbuzz8, fizzbuzz9]
runTests = map (\x -> if x == testData then passed else failed) tests
test
    | all (== passed) runTests = passed
    | otherwise = failed
