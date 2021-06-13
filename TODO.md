## Better predicate descriptions

Some predicates could do a better job explaining why they succeed or fail.  For
example, `elemsAre` or `each` could explain which elements don't match.  This
means expanding the API to `Predicate` to include an optional explanation.

## Whole-method matching

Sometimes you want to be able to define matching predicates that span multiple
arguments to a method.  There are whole-method matchers in frameworks like gMock
to handle this.  For example, I may want to require that two arguments are equal
to each other.  HMock should have a way to do this.

## Instances for effect systems

An increasing number of people are using libraries like `eff`, `polysemy`,
`fused-effects`, or `freer-simple`, which do not work with mtl-style classes.
It would be a very compelling feature if I could also make HMock work for some
of these effect systems.

* `freer-simple`
* `fused-effects`
* `polysemy`
* `eff`
* Maybe a `haxl` data source?

## Wrappers to save responses from integration tests.

One nice feature of a mock framework could be to save interactions during an
integration test, and then automatically set up the related expectations.

Open questions:

1. How would one run a test in record mode?  You'd need to set up the same
   environment as production, but insert a proxy monad that intercepts all
   mockable calls.  Making this easy to integrate could be tricky.
2. How does one record the arguments and return values?  If all of the arguments
   have the right instances to be serializable and comparable for equality, then
   this works.  In practice, one probably hopes for support for more than just
   this.  For example, suppose a call takes a function as an argument.  We might
   want to accept any function?  We might want to somehow wrap the function with
   a proxy that records its arguments and results and verifies that those match?
   Or should this not be recordable at all?  And what happens if the record sees
   a method that can't be recorded because of lacking serialization, etc.?  I
   suppose it has to be a runtime error at record time.
3. How do you handle ordering constraints?  The easy thing to do is add them all
   to a single inSequence, but even more interesting would be to offer some
   other heuristics or guidance.  For example, you could list certain actions
   that should be ignored or stubbed, etc.  Some kind of interactive experience
   to work from a method call log and make these ordering choices would be
   awesome.

## Mockable with Typeable polymorphic return values.

``` haskell
class MonadFoo m where
    foo :: forall a. Typeable a => a -> m a

makeMockable ''MonadFoo
```

Currently, `foo` is considered unmockable, because matching methods is no longer
enough to prove equality of the return types, so we cannot be sure that the
response has the right type for the actual call.  But there is a `Typeable`
constraint, so we should be able to check this at runtime instead!  That is, one
should be able to write something like:

``` haskell
whenever $ Foo_ (typed @Bool anything) |-> True
```

Unifying the return types requires a `cast` in `matchAction`, but we should have
the constraints that we would need to implement this.

## Mockable with unconstrained polymorphic return values.

Now consider polymorphic return types without the `Typeable` constraint.

``` haskell
class MonadFoo m where
    foo :: forall a. a -> m a

makeMockable ''MonadFoo
```

Again, this is considered unmockable now.  You could try to write a polymorphic
expectation, like this:

``` haskell
-- Matches foo applied to any type of argument.
whenever $ Foo_ anything :-> \(Foo x) -> return x
```

However, `(:->)` has an ambiguous type here.  My instinct is to try to promote
the ambiguous type to a rank 2 type, thereby limiting what the programmer may
write to those things which can unify with *any* type acceptable to `foo`.  In
order to represent the relationship between types of the arguments and return
value, the type of `Matcher` would probably need to mention its argument types,
as well.  This could be done in a type-level list.

If we do this right and get lucky, it could generalize both the type-specific
`Rule` with `Typeable` constraints and the existing behavior for polymorphic
arguments, which are already universally quantified with rank-n types in the
`Matcher`.  But my attempts to make this work have so far failed.
