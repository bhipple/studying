{-# LANGUAGE TemplateHaskell #-}
module Main where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Depends
import Control.Monad

curryN :: Int -> Q Exp
curryN n = do
    f <- newName "f"
    xs <- replicateM n (newName "x")
    let args = map VarP (f:xs)
        ntup = TupE (map VarE xs)
    return $ LamE args (AppE (VarE f) ntup)

main :: IO ()
main = do
    let m = $(stringE . show =<< thisModule)
    putStrLn $ "Hello!  This module is: " ++ m

    let tup = $(tupE $ take 4 $ cycle [ [| "hi" |] , [| 5 |] ])
    print tup

    --putStrLn ($(sel 2 3) ("a","b","c"))
