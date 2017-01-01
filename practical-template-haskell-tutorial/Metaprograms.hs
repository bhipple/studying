{-# LANGUAGE TemplateHaskell #-}
-- Template Haskell programs must be fully compiled and typechecked
-- before they can be spliced.
module Metaprograms where

import Control.Monad (replicateM)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Traversable (for)

-- Meta function that curries a function taking an arbitrary n-tuple
-- `curryN` will take an input N and build a lambda that pattern matches
-- against a function f with N argument variables x1,x2,...,xn in its body.
-- It then applies f to the n arguments, as we'd expect a curry function to do.
curryN :: Int -> Q Exp
curryN n = do
    -- `newName :: String -> Q Name` is a monadic gensym function to avoid name clashes.
    -- newName names cannot be captured; if we wanted a capturable name, we could use `mkName`.
    -- A Name is an abstract type representing names in the abstract sytnax tree.
    f <- newName "f"

    -- Generate an array of n new names
    xs <- replicateM n (newName "x")

    -- Convert these names into Pat variables, suitable for passing to
    -- a lambda expression or function.
    let args = map VarP (f:xs)

    -- First we apply VarE to the Names, which constructs them into Expressions.
    -- Then we take the [e1..en] array and convert it to a (e1,e2,...,en) tuple
    let ntup = TupE (map VarE xs)

    -- Return a Lambda Exp that takes a function and n inputs in (args),
    -- and applies the function to the n-tuple.
    return $ LamE args (AppE (VarE f) ntup)

-- Generate the first n curry functions
genCurries :: Int -> Q [Dec]
genCurries n = for [1..n] mkCurryDec
    where mkCurryDec ith = do
            -- Get the function expression
            cury <- curryN ith

            -- Give it a name that is captured with `mkName` exactly,
            -- so it can be referred to elsewhere. If we used newName, we'd
            -- be guaranteed to get a generated name not referred to anywhere else
            -- in the program, which would prevent others from calling it!
            let name = mkName $ "curry" ++ show ith

            -- Construct a function declaration with a normmal body
            -- (other alternative is GuardedB).
            -- The function is a name and a clause, which consists of a list
            -- of patterns, a function body, and a list of declarations.
            return $ FunD name [Clause [] (NormalB cury) []]

-- Using Syntax Construction Functions. Equivalent to the above.
genCurries' :: Int -> Q [Dec]
genCurries' n = for [1..n] mkCurryDec
    where mkCurryDec ith = funD name [clause [] (normalB (curryN ith)) []]
            where name = mkName $ "curry" ++ show ith ++ "'"
-- funD, clause, and normalB are equivalent to their data constructors
-- FunD, Clause, and NormalB, but they're in the Q monad and allow us to
-- write a program more succinctly by letting us avoid the Q wrapping and unwrapping.

mapN :: Int -> Q Dec
mapN n
    -- `funD :: Name -> [ClauseQ] -> DecQ`
    -- Our function will have two clauses, so that mapN 3 will generate:
    -- map3 f (x:xs) (y:ys) (z:zs) = f x y z : map3 f xs ys zs
    -- map3 _ _ _ _ = []
    | n >= 1 = funD name [cl1, cl2]
      -- TODO: Consider using LiquidHaskell to put a verification measure here instead ;)
    | otherwise = fail "mapN: argument n may not be <= 0."
    where
        -- First, generate a name for the function that can be captured and reference elsewhere
        name = mkName $ "map" ++ show n

        -- And then generate some internal names for our xs and ys
        -- Recall `data Clause = Clause [Pat] Body [Dec]
        -- Start out by generating some names for our f, xs, ys
        cl1 = do f <- newName "f"
                 -- The "heads" to the cons patterns.
                 xs <- replicateM n (newName "x")
                 -- The "tails" to the cons patterns.
                 ys <- replicateM n (newName "ys")
                 -- Arguments are the function and all of the cons patterns
                 let argPatts, consPatts :: [PatQ]
                     argPatts = varP f : consPatts
                     -- consPatts are the (x:xs), (y:ys), (z:zs), etc. patterns
                     -- Note that the variable names are a little confusing here;
                     -- the xs list is a list of "head" names x, not a the first
                     -- list to the mapN argument! And likewise the ys are the
                     -- "tail" names of xs, etc.
                     consPatts = [ [p| $(varP x) : $(varP ys) |]
                                 | (x,ys) <- xs `zip` ys ]

                     -- Where the magic happens! We take the function and fold
                     -- it over the [x,y,z] elements, and in the other instance we
                     -- get the recursive call with the function on xs, ys, zs
                     apply :: ExpQ -> [Name] -> ExpQ
                     apply = foldl (\ g x -> [| $g $(varE x) |])

                     first, rest :: ExpQ
                     -- Apply the function to the head x, y, z elements
                     first = apply (varE f) xs
                     -- Apply the mapN function to the rest of the xs, ys, zs elements
                     rest = apply (varE name) (f:ys)
                 -- Return the cons of the application of the function to the head
                 -- elements with the recursive application of the function to the
                 -- tail arguments
                 clause argPatts (normalB [| $first : $rest |]) []
        -- This is the clause that represents the otherwise and returns []
        -- wildP refers to the underscore operator, _. We use n+1 of them, since
        -- we have all the arguments and the function itself that we can ignore.
        -- '[] referes to Haskell's built-in list constructor [] within TH
        -- conE makes the expression a Data Constructor expression, in this case
        -- the constructor for the standard list.
        cl2 = clause (replicate (n+1) wildP) (normalB (conE '[])) []

-- In general, Template Haskell code obeys typical static lexical scoping rules.
-- The only exception is identifiers with mkName, which are captured and whose
-- names reference the closest variable in scope (even if that shadows another)
x :: Int
x = 42

-- This will always refer to the global x = 42 value above, regardless of scope
staticX :: Q Exp
staticX = [| x |]

-- This will capture the closest x value
dynamicX :: ExpQ
dynamicX = varE (mkName "x")


-- Reification and Derivation
data Deriving = Deriving { tyCon :: Name, tyVar :: Name }

-- In order to satisfy a functor definition, all values of type a must be replaced
-- with values of type b, and the mapping must be structure preserving.
deriveFunctor :: Name -> Q [Dec]
deriveFunctor ty = do
    -- First we get some information about the type ty using reify.
    -- We expect the info to match `TyConI Dec`, which is a plain type
    -- declaration.
    (TyConI tyCon) <- reify ty

    -- Get the type's type constructor name, the type's declared type variables,
    -- and its exposed constructors
    (tyConName, tyVars, cs) <- case tyCon of
            -- Within a type delcaration, it could be a Data declaration DataD,
            -- a newtype declaration NewtypeD, a TySynD, or a handful of other things.
            DataD _ nm tyVars Nothing cs _ -> return (nm, tyVars, cs)
            NewtypeD _ nm tyVars Nothing c _ -> return (nm, tyVars, [c])
            _ -> fail "deriveFunctor: tyCon may not be a type synonym."

    -- tyVar is the rightmost type variable of the Data type
    let (KindedTV tyVar StarT) = last tyVars
        instanceType = conT ''Functor `appT`
            (foldl apply (conT tyConName) (init tyVars))

    -- Put this state into the Q Monad for accessing later
    putQ $ Deriving tyConName tyVar

    -- Generate the instance declaration
    sequence [instanceD (return []) instanceType [genFmap cs]]
    where
        apply t (PlainTV name) = appT t (varT name)
        apply t (KindedTV name _) = appT t (varT name)

genFmap :: [Con] -> Q Dec
genFmap cs = do funD 'fmap (map genFmapClause cs)

-- Recursively map the provided funct ion f :: a -> b over all of
-- a constructor's fields of type a, while leaving other fields untouched
genFmapClause :: Con -> Q Clause
genFmapClause c@(NormalC name fieldTypes) = do
    f <- newName "f"
    fieldNames <- replicateM (length fieldTypes) (newName "x")

    let pats = varP f:[conP name (map varP fieldNames)]
        body = normalB $ appsE $
                conE name : map (newField f) (zip fieldNames fieldTypes)

    clause pats body []

newField :: Name -> (Name, StrictType) -> Q Exp
newField f (x, (_, fieldType)) = do
    Just (Deriving typeCon typeVar) <- getQ
    case fieldType of
      -- If it's a variable of type A, then apply f to it.
      VarT typeVar' | typeVar' == typeVar
                        -> [| $(varE f) $(varE x) |]
      -- If it's an application of a function and the leftmost argument is of type a,
      -- then apply f to it
      ty `AppT` VarT typeVar' | leftmost ty == (ConT typeCon) && typeVar' == typeVar
                        -> [| fmap $(varE f) $(varE x) |]
      -- Otherwise, just leave it alone
      _                 -> [| $(varE x) |]

leftmost :: Type -> Type
leftmost (AppT ty1 _) = leftmost ty1
leftmost ty = ty
