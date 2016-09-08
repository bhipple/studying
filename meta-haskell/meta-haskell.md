# Template Meta-Programming for Haskell

* Quasi-Qutation => Gensym
* Staged type-checking co-routines between compile- and run-time
* Reification

## The Basic Idea
* Printf implementation
    * C: `printf "Error: %s on line %d." msg line`
        * Not type safe!
    * TH: `$(printf "Error: %s on line %d") msg line`
        * `$` means splice, or evaluate at compile time
        * Generates: `(\s0 -> \n1 -> "Error: " ++ s0 ++ " on line " ++ show n1)`
    * QuasiQuote Notation
        * `[| ... |]` is the quasi-quote notation

```haskell
printf :: String -> Expr
printf s = gen (parse s) [| "" |]

data Format = D | S | L String
parse :: String -> [Format]

gen :: [Format] -> Expr -> Expr
gen [] x = x
gen (D:xs) x = [| \n -> $(gen xs [| $x++show n |]) |]
gen (S:xs) x = [| \s -> $(gen xs [| $x++s |]) |]
gen (L s:xs) x = gen xs [| $x ++ $(lift s) |]
```

## Why templates?
* Conditional compilation
* Program reification
* Algorithmic program construction
* Abstractions that transcend the abstraction mechanisms accessible in the language
* Optimizations
* Compile-time functions written and executed in natural Haskell code

## Flexible Constructions
* Selecting the kth element out of a n-tuple, e.g. selecting the 1st element
from a triple:

```haskell
$(sel 1 3) x
sel :: Int -> Int -> Expr
sel i n = [| \x -> case x of ... |]
--                          ^ does not work because the can't just write ...!

sel :: Int -> Int -> Expr
sel i n = lam [pvar "x"] (caseE (var "x") [alt])
    where alt :: Match
          alt = simpleM pat rhs

          pat :: Patt
          pat = ptup (map pvar as)

          rhs :: Expr
          rhs = var (as !! (i-1))

          as :: [String]
          as = ["a" ++ show i | i <- [1..n]]

-- Syntax for Patterns
pvar :: String -> Patt              -- x
ptup :: [Patt] -> Patt              -- (x,y,z)
pcon :: String -> [Patt] -> Patt    -- (Fork x y)
pwild :: Patt                       -- _

-- Syntax for Expressions
var :: String -> Expr               -- x
tup :: [Expr] -> Expr               -- (x,3+y)
app :: Expr -> Expr -> Expr         -- f x
lam :: [Patt] -> Expr -> Expr       -- \x y -> 5
caseE :: Expr -> [Match] -> Expr    -- case x of ...
simpleM :: Patt -> Expr -> Match    -- x:xs -> 2

-- Data representations
data Lit = Int Int | Char Char

data Pat
    = Plit Lit              -- { 5 or 'c' }
    | Pvar String           -- { x }
    | Ptup [Pat]            -- { (p1,p2) }
    | Pcon String [Pat]     -- data T1 = C1 t1 t2; {C1 p1 p1} = e
    | Ptilde Pat            -- { ~p }
    | Paspat String Pat     -- { x @ p }
    | Pwild                 -- { _ }

type Match p e d = (p,Body e,[d])   -- case e of { pat -> body where decs }
type Clause p e d = ([p],Body e,[d]) -- f { p1 p2 = body where decs }

data Exp
    = Var String
    | Con String
    | Lit Lit
    | App Exp Exp
    | Lam [Pat] Exp
    | Tup [Exp]
    | Cond Exp Exp Exp
    | Let [Dec] Exp
    | Case Exp [Match Pat Exp Dec]      -- { case e of m1; m2 }
    | Do [Statement Pat Exp Dec]        -- { do { p <- e1; e2 } }
    | Comp [Statement Pat Exp Dec]      -- { [(x,y) | x <- xs, y <- ys] }
    | ArithSeq (DotDot Exp)             -- { [1,2,..10]}
    | ListExp [Exp]                     -- { [1,2,3] }

data Body e
    = Guarded [(e,e)]                   -- f p { | e1 = e2 | e3 = e4 } where ds
    | Normal e                          -- f p = { e } where ds

data Statement p e d
    = BindSt p e                        -- { p <- e }
    | LetSt [d]                         -- { let f x = e }
    | NobindSt e                        -- { print e}
    | ParSt [[Statement p e d]]         -- { x <- xs | y <- ys, z <- zs }

data DotDot e
    = From e                            -- [ { 0..} ]
    | FromThen e e
    | FromTo e e
    | FromThenTo e e e                  -- [ {0,2,..12} ]

data Dec
    = Fun String [Clause Pat Exp Dec]   -- { f p1 p2 = b where decs }
    | Val Pat (Body Exp) [Dec]          -- { p = b where decs }
    | Data String [String] [Constr] [String]    -- { data T x = A x | B (T x) deriving (Z,W) }
    | Class [Typ] Typ [Dec]             -- { class Eq a => Eq [a] where ds }
    | Instance [Typ] Typ [Dec]          -- { instance Show w => Show [w] where ds }
    | Proto Name Type                   -- { length :: [a] -> Int }

data Constr = Constr String [Typ]

data Tag
    = Tuple Int                 -- (,,)
    | Arrow                     -- (->)
    | List                      -- ([])
    | Name String deriving Eq   -- Tree

data Typ
    = Tvar String               -- a
    | Tcon Tag                  -- T or [] or (->) or (,,) etc
    | Tapp Typ Typ              -- T a b
```
