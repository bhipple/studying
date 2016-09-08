1. Template Meta-Programming for Haskell

* Quasi-Qutation => Gensym
* Staged type-checking co-routines between compile- and run-time
* Reification

2. The Basic Idea
* Printf implementation
    * C: `printf "Error: %s on line %d." msg line`
        * Not type safe!
    * TH: `$(printf "Error: %s on line %d") msg line`
        * `$` means splice, or evaluate at compile time
        * Generates: `(\s0 -> \n1 -> "Error: " ++ s0 ++ " on line " ++ show n1)`
    * QuasiQuote Notation
        * `[| ... |]` is the quasi-quote notation

\begin{code}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
Stop -- wall to block syntastic errors from going further
\end{code}

\begin{code}
printf :: String -> Expr
printf s = gen (parse s) [| "" |]

data Format = D | S | L String
parse :: String -> [Format]

gen :: [Format] -> Expr -> Expr
gen [] x = x
gen (D:xs) x = [| \n -> $(gen xs [| $x ++ show n |]) |]
gen (S:xs) x = [| \s -> $(gen xs [| $x ++ s |]) |]
gen (L s:xs) x = gen xs [| $x ++ $(lift s) |]
\end{code}

3. Why templates?
* Conditional compilation
* Program reification
* Algorithmic program construction
* Abstractions that transcend the abstraction mechanisms accessible in the language
* Optimizations
* Compile-time functions written and executed in natural Haskell code

4. Flexible Constructions
* Selecting the kth element out of a n-tuple, e.g. selecting the 1st element
from a triple:

\begin{code}
$(sel 1 3) x
sel :: Int -> Int -> Expr
--sel i n = [| \x -> case x of ... |]
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

-- Equivalent, using the QuasiQuote syntactic sugar
sel' :: Int -> Int -> Expr
sel' i n = [| \x -> $(caseE [| x |] [alt]) |]
    where alt = simpleM pat rhs
          pat = ptup (map pvar as)
          rhs = var (as !! (i-1))
          as = ["a" ++ show i | i <- [1..n] ]

-- Syntax for Patterns
pvar :: String -> Patt              -- x
ptup :: [Patt] -> Patt              -- (x,y,z)
pcon :: String -> [Patt] -> Patt    -- (Fork x y)
pwild :: Patt                       -- _

-- Syntax for Expressions
var :: String -> Expr               -- x
var = return . Var

tup :: [Expr] -> Expr               -- (x,3+y)
tup es = do { es1 <- sequence es; return (Tup es1) }

app :: Expr -> Expr -> Expr         -- f x
app x y = do { a <- x; b <- y; return (App a b) }

lam :: [Patt] -> Expr -> Expr       -- \x y -> 5
lam ps e = do { e2 <- e; return (Lam ps e2) }

caseE :: Expr -> [Match] -> Expr    -- case x of ...
simpleM :: Patt -> Expr -> Match    -- x:xs -> 2

-- Monadic Syntax Operator
instance Monad Q
gensym :: String -> Q String
runQ :: Q a -> IO a
qIO :: IO a -> Q a

type Expr = Q Exp
type Patt = Pat
type Decl = Q Dec
type Type = Typ
type Mat = Match Pat Exp Dec
type Mtch = Match Patt Expr Decl
type Cls = Clause Pat Exp Dec
type Clse = Patt Expr Decl
type Stm = Statement Pat Exp Dec
type Stmt = Statement Patt Expr Decl

plit = Plit; pvar = Pvar
ptup = Ptup; pcon = Pcon
ptilde = Ptilde; paspat = Paspat
pwild = Pwild

con = return . Con
lit = return . Lit

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
\end{code}

A further example: implementing the zipN function

\begin{code}
zipN :: Int -> Expr
zipN n = [| let zp = $(mkZip n [| zp |]) in zp |]

-- Helper function: genPE "x" 2 -> ([pvar "x1", var "x2"'], [var "x1", var "x2"])
genPE :: String -> Int -> ([Patt], [Expr])
genPE s n = let ns = [s ++ show i | i <- [1..n]]
            in (map pvar ns, map var ns)

mkZip :: Int -> Expr -> Expr
mkZip n name = lam pYs (caseE (tup eYs) [m1,m2])
    where
      (pXs, eXs) = genPE "x" n
      (pYs, eYs) = genPE "y" n
      (pXSs, eXSs) = genPE "xs" n
      pcons x xs = [p| $x : $xs |]
      b = [| $(tup eXs) : $(apps (name : eXSs)) |]
      m1 = simpleM (ptup (zipWith pcons pXs pXSs)) b
      m2 = simpleM (ptup (copies n pwild)) (con "[]")

apps :: [Expr] -> Expr
apps [x] = x
apps (x:y:zs) = apps ( [| $x $y |] : zs )

\end{code}

Typically, the simple QuasiQuote notation will be enough, but we need to drop
down to the more explicit style in some cases, such as when dealing with
indeterminate length.

5. Declarations and Reification
Generalized deriving is inspired by a library called DrIFT.

\begin{code}
data T a = Tip a | Fork (T a) (T a)

splice (genEq (reifyDecl T))
\end{code}

splice (...) appears where a declaration group is needed; contrast with $(...), which is for an expression.

6. Static Scoping
Consider:
\begin{code}
cross2a :: Expr -> Expr -> Expr
cross2a f g = [| \(x,y) -> ($f x, $g y) |]
\end{code}

It had better be the case that cross2a (var "x") (var "y") results in \(x0,y1) -> (x x0, y y1).
While the quasiquotes do this, the more verbose variant does not, so we need
gensym to perform alpha reductions.

\begin{code}
cross2b f g = lam [ptup [pvar "x", pvar "y"]]
                (tup [app f (var "x"), app g (var "y")])
-- Busted! Results in \(x,y) -> (x x, y y)

cross2c :: Expr -> Expr -> Expr
cross2c f g = do
    x <- gensym "x"
    y <- gensym "y"
    ft <- f
    gt <- g
    return (Lam [Ptup [Pvar x,Pvar y]]
                 (Tup [App ft (Var x)
                      ,App gt (Var y)]))
\end{code}

The Three Layers of Template Haskell:
- Bottom layer: ordinary algebraic data types (program fragments) and the quatation monad (gensym, failure, IO)
- Middle layer: syntax-construction functions: tup, app, etc. to lift algebraic data constructors into Q
- Top layer: The QuasiQuote notation

The latter two layers are convenience interfaces to the first.

-- Syntax Construction Functions
Using the monadic construction functions allows us to simplify cross2c slightly:
\begin{code}
cross2d :: Expr -> Expr -> Expr
crodd2d f g = do 
    x <- gensym "x"
    y <- gensym "y"
    lam [ptup [pvar x, pvar y]]
        (tup [app f (var x), app g (var y)])
\end{code}

Note that Patt and Type are simply type synonyms, because we do not need to
gensym for patterns or types the way we do for expressions and declarations!

\begin{code}
cross2e f g = do
    (vf,p) <- genpat (ptup [pvar "x", pvar "y"])
    lam [p] (tup [app f (vf "x"), app g (vf "y")])

-- Alpha reduces a whole pattern by returning a new pattern and a function which
-- maps the names of the variables in the original pattern to Exprs with the
-- names of the variables in the alpha reduced pattern.
genpat :: Patt -> Q (String -> Expr, Patt)
\end{code}
