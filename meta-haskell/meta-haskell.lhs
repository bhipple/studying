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

- =============================================================================
-                             3: Why templates?
- =============================================================================
* Conditional compilation
* Program reification
* Algorithmic program construction
* Abstractions that transcend the abstraction mechanisms accessible in the language
* Optimizations
* Compile-time functions written and executed in natural Haskell code

- =============================================================================
-                         4: Flexible Constructions
- =============================================================================
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

- =============================================================================
-                      5: Declarations and Reification
- =============================================================================
Generalized deriving is inspired by a library called DrIFT.

\begin{code}
data T a = Tip a | Fork (T a) (T a)

splice (genEq (reifyDecl T))
\end{code}

splice (...) appears where a declaration group is needed; contrast with $(...), which is for an expression.

- =============================================================================
-                             6: Static Scoping
- =============================================================================
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

6.3. Syntax Construction Functions
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

-- ============================================================================
--                      7: Typing Template Haskell
-- ============================================================================
Basic Hindley-Milner type checking is insuffucient because the type of a spliced
expression that we're checking may depend on the value of its (not spliced)
arguments, as in the printf case.

This moves in three stages:
1. Type-check the body of the splice, with normal typing rules
  (often resulting in type :: Expr)
2. Compile, execute, and splice result in place of the splice expression
3. Standard type-check the result as-if the human had written that

Consequences: TH is compile-template only! Once the outer splices are finished
evaluating, all inner splices must also be finished! Can't punt to runtime TH.

Compiler moves between these states:
* Compiling State (C)
    - Compiling regular code
-> Bracket State (B)
    * Compiling code inside quasi-quotes
-> Splicing (S)
    * Encounters expression escaped inside quasi-quoting brackets

also counts levels, to keep track of nested depths.

7.2. Declarations
Since further compiled calls may depend on declarations generated from previous
splices, the program is compiled top-to-bottom!
e.g.,
\begin{code}
splice (genZips 20)
foo = zip3 "fee" "fie" "fum"
\end{code}

Moreover, splices have to be top level declarations, like ``data`, `class`, and
`instance`. This is to avoid ambiguity about where a variable in the program is
bound. It also prevents a nested splice from shadowing a binding of its parent
splice unintentionally.

Think of a top-level splice as a "programmable import."

- =============================================================================
-                            8: Quatation Monad
- =============================================================================
8.1 Reification
Allows TH to query the state of the compiler's internal symbol tables.

\begin{code}
module M where

data T a = Tip a | Fork (T a) (T a)

repT :: Decl
repT = reifyDecl T

-- The type of the length function
lengthType :: Type
lengthType = reifyType length

-- Get the fixity of the % operator
percentFixity :: Q Int
percentFixity = reifyFixity (%)

-- Get the source file and line number
here :: Q String
here = reifyLocn

-- Get -DDEBUG compiler flag
reifyOpt "DEBUG"
\end{code}

This can be particularly useful for implementing assertions!

8.4 Printing Code
We have `runQ :: Q a -> IO a` to print code generated by Template Haskell.
This is useful for learning and debugging, among other things.

8.5 Implementing Q
\begin{code}
newtype Q a = Q (Env -> IO a)
\end{code}

As of the writing of the paper, the environment for Q contained:
* Mutable location used as a name supply for gensym
* Source location of the top-level splice that invoked the evaluation
* Compiler's symbol table
* Command-line switches

- =============================================================================
-                   9. Quasi-quotes and Lexical Scoping
- =============================================================================
TH has hygienic macros, which means: every occurrence of a variable is bound
to the value that is lexically in scope at the occurrence site in the original
source program, before any template expansion.

9.1. Cross-stage Persistence
We use the concept of "original name" Foo:Bar to mean the Bar defined in the
module Foo. Original names are not accessible in normal Haskell. Including in
generated code the value of a variable that exists at compile-time is called
"cross-stage persistence."

9.2. Dynamic Scoping
If instead we want dynamic scoping, we can do this:
\begin{code}
genSwapDyn x = [| $(var "swap") x |]
\end{code}
and now when this expands to `swap (4,5)`, the swap we get will be the
swap in scope at the splice site, regardless of what was in scope at
the definition of genSwapDyn.  Cool!

9.3. Implementing quasi-quote
Implemented in terms of original names, syntax constructor functions, gensym,
do, return, and lift.

TranslateExpression function:
\begin{code}
trE :: VEnv -> Exp -> Exp

trE  cl (App a b) = App (App (Var "app") (trans a)) (trans b)
...

type VEnv = String -> VarClass
data VarClass = Orig ModName | Lifted | Bound
\end{code}

The environment handles different treatments for variables depending on where they're bound:
* Orig m => v is bound at the top level of module m, with original name m:v
* Lifted => v is bound outside the quasi-quotes, but not at the top level. Generate a call to `lift`
* Bound => v is bound in the quasi-quotes and needs alpha-renaming with gensym

- =============================================================================
-                            10. Related Work
- =============================================================================
10.1. C++ Templates
Extraordinarily baroque functional programming language full of ad hoc tricks
and conventions with high barrier to entry. Operates fully in the type system.

10.2 Scheme Macros
Aside from the obvious static vs. dynamic typing differences, the scheme system
is both more powerful and less tractable than TH in the following ways:
* Arbitrary recursive binding: `(foo k (+ k 1))` can exapnd to `(lambda k (* 2 (+ k 1)))`.
  while TH restricts declarations to the top level.
  TH can do the above as: `$(foo "k" [| $(var "k") + 1 |])`
* Scheme has no special syntax for macros at the call site, while TH must use the
  splice notation $.  TH doesn't have to have special syntax at the definition site,
  while Scheme requires `define-syntax`. Hence Scheme macros must be called by name, while
  TH macros are completely higher-order and first class and can be passed as args,
  used in lambdas, etc.
* Scheme allows side-effects

10.3. MetaML
Features in TH inspired by MetaML:
* Quasi-quote notation
* Type-safety before template executation and at compile time
* Lexical scoping with alpha reduction to avoid inadvertent capture
* Cross-stage persistence

And some differences:
* All TH code is built at compile-time, and there is no runtime overhead; MetaML
  allows runtime macros
* TH uses ADT representation of code, which allows reflection without sacrificing
  lexical scoping or quasi-quotation
* TH uses delayed checking with just-in-time type checking
* All hand-written code is reifiable in TH
