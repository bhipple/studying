{-# LANGUAGE TemplateHaskell #-}
module Depends where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- Because the compiler doesn't do multiple passes on a source file,
-- we can't use a function in a splice that is defined in the same module.
{-- TODO: Fix this
sel :: Int -> Int -> ExpQ
sel i n = [| \x -> $(caseE [| x |] [alt]) |]
    where alt :: MatchQ
          alt = match pat (normalB rhs) []

          pat :: Pat
          pat = tupP (map varP as)

          rhs :: ExpQ
          rhs = varE(as !! (i -1)) -- !! is 0 based

          as :: [Name]
          as = mkName <$> ["a" ++ show i | i <- [1..n] ]
-}
