{-# LANGUAGE TemplateHaskell #-}
-- A Practical Introduction to Template Haskell
module Main where

import Metaprograms
import Language.Haskell.TH

-- Primary uses:
-- 1) Compile-time code generator to avoid writing boilerplate
--    (example: deriving Aeson and Lens instances)
-- 2) Fraomework for creating embedded domain specific languages (EDSLs)
--    (example: the shakespearean template languages)

---------------------------------------
-- 2.1 Template Haskell as a Code Generator

-- See Metaprogram.sh for curryN
-- Run the curryN function at compile time to generate functions
$(genCurries 20)
$(genCurries' 20)

printThree :: (Int, Int, Int) -> IO ()
printThree (a,b,c) = putStrLn $ "I see: " ++ show a ++ ", " ++ show b ++ ", " ++ show c

printThreeC :: Int -> Int -> Int -> IO ()
printThreeC = curry3 printThree

printThreeC' :: Int -> Int -> Int -> IO ()
printThreeC' = curry3' printThree

-- Object programs created by Template Haskll are represented as regular ADTs in the AST,
-- with expressions (Exp), patterns, (Pat), declarations (Dec), and types (Type).
-- Additionally, all Haskell identifiers are represented by the abstract Name data type.

-- Syntax Construction Functions
-- Because TH in the Q monad can be verbose and unwieldy, TH provides syntax
-- construction functions that hide the monadic nature of the code.
-- See genCurries' for an example

-- Quotation Brackets
-- Even more succinctly, we can use [| .. |] quotation brackets.
-- By default, we get the one for expressions [e| .. |], but there's also
-- patterns: [p| .. |]
-- declarations: [d| .. |]
-- types: [t| .. |]
-- Inside the quotation brackets we write normal, concrete Haskell syntax that gets lifted
-- into the template haskell object program in the Q monad.
-- For all e, the expression `$([| e |]) == e` holds; namely, the quotation brackets
-- lift e into template haskell, and the splicing operation runs TH object programs to
-- generate regular haskell code isomorphically.

-- Within TH, we can quote regular Haskell value and type identifiers:
-- 'mapM refers to the normal Haskell function `Name` mapM

-- 2.1.1) Generic Map Function
-- `mapN :: Int -> Q Dec` where $(mapN 1) generates the regular map, while
-- $(mapN 2) generates (a -> b -> c) -> [a] -> [b] -> [c], and so on.

-- $(mapN 3)

-- Demonstrating scoping rules
plus42 :: Int -> Int
plus42 x = $staticX + x

timesTwo :: Int -> Int
timesTwo x = $dynamicX + x

-- This will take the closest x in scope. Very dangerous to do this at the top level,
-- since importing another x or hiding it from the Metaprograms import will change
-- the plus42' behavior!
-- For instance, import Metaprograms hiding (x) and `x = 52` will cause this to print 52 + y
plus42' :: Int -> Int
plus42' y = $dynamicX + y

plus52 :: Num a => a -> a
plus52 y = let x = 52 in $dynamicX + y

--------------------
-- 2.1.2 Reification
-- Allows a metaprogram to query compile-time information about other program parts
-- while constructing the object program.
-- To demonstrate this, we will derive functor instances for the following data types
data Result e a = Err e | Ok a
data List a = Nil | Cons a (List a)
data Tree a = Leaf a | Node (Tree a) a (Tree a)

$(deriveFunctor ''Result)
$(deriveFunctor ''List)
$(deriveFunctor ''Tree)

---------
-- 2.2. Embedded DSLs
-- Skipped the example on regular expressions for now

-- Hamlet
-- Fairly self-explanatory to use, with a couple nice features, including:
-- * Significant whitespace instead of closing </html> tags
-- * Type-safe, compile-checked links.

main :: IO ()
main = do
    putStrLn "Currying examples:"
    printThree (1,2,3)
    printThreeC 1 2 3
    printThreeC' 1 2 3

    putStrLn "\nScoping examples:"
    putStrLn $ "plus42 4 = " ++ show (plus42 4)
    putStrLn $ "timesTwo 4 = " ++ show (timesTwo 4)
    putStrLn $ "plus42' 4 = " ++ show (plus42' 4)
    putStrLn $ "plus52 4 = " ++ show (plus52 4)

