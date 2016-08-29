# [Typeclasses vs the World](https://www.youtube.com/watch?v=hIZxTQP1ifo)

## Typeclasses vs. OO vtables
* Dispatch is done by pattern matching and matching types, rather than a hierarchy
of types.
* There is no object with a vtable; it's all driven by the types.
* This dispatch is all done at compile time.

## Typeclasses vs. Implicits
Languages like Scala have "implicits" via abstract classes. One nice thing about
this approach is that it requires writing less code, because we can define e.g.
monoid in terms of semigroup.

* Big advantage for functional programming: we make simple, dump, reusable data.
    * We can just define a pair or a maybe or an either and use it everywhere
    * Don't need to make EnterpriseBoolComparator or deal with nulls everywhere.
    * Contraints are placed where on data when it's used in fuctions, not
      baked into the type itself!
    * We can use the same data types and combinators over and over again
* Refactoring benefits
    * If there's an Monoid instance for the Foo typeclass, there is only *one*.
    * It doesn't matter where you got the foos, they'll have the same instance definition
        * Contrast with vtables and BankAccount objects that seem the same to you but
          because they came from somewhere else they have different AddInterest functions.
    * Scala has rules about using the implementation "closest" to where you are, that
      creates an extra liability when moving code around.

## Typeclasses vs. ML Modules
* ML Modules allow us to parameterize a module with values instantiated by another module
    * E.g., write a number of functions parameterized over a RNG provided by another module
* Both are similarly powerful, but typeclasses tend to result in fewer code changes when refactoring.

## Constraints
* As of GHC 7.4, we now have a `Constraint` data type available with the `ConstraintKinds` extension
* This lets us talk about variables of type Constraint: the LHS of `=>`
* Lots of category theory stuff here
    * Take away is a proof of the property that the data types form a category and going from Ord a => Eq [a]
      doesn't matter if we go Ord a => Ord [a] => Eq [a] vs. Ord a => Eq a => Eq [a]
    * It is provably true that providence does not matter; the category composition is thin and commutable

## Local Instances
* See Kmett's `Data.Reflection` to define local instances for your own custom newtypes

```haskell
data Proxy a = Proxy

class Reifies s a | s -> a where
    reflect :: p s -> a

reify :: a -> (forall s. Reifies s a => Proxy s -> r) -> r
```

## Takewaway point
* Implicits are interesting, and often convenient -- but we give up a lot of ability to reason about our program
* ML Modules are awesome and involve interesting pros/cons vs. Typeclasses
* At times we lose some flexiblity (local instances) in exchange for reasoning
  power, but there are ways to deal with that (Reflection).
