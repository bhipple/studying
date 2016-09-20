{-# LANGUAGE TemplateHaskell #-}
module Main where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Depends

main :: IO ()
main = do
    let m = $(stringE . show =<< thisModule)
    putStrLn $ "Hello!  This module is: " ++ m

    let tup = $(tupE $ take 4 $ cycle [ [| "hi" |] , [| 5 |] ])
    print tup

    --putStrLn ($(sel 2 3) ("a","b","c"))
