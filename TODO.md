## Ambiguity resolution

Currently, it's an error when more than one `Matcher` in the expectations
applies to the current `Action`.  It's more common in other mock frameworks to
adopt rules for choosing which match to prefer.  For example, sometimes they
choose the most specific match, or just the most recently added.

I definitely don't like gMock's "most recently added" rule, but most specific is
appealing.  This would mean adding some kind of partial order on specificity of
predicates.  We'd probably just adopt a three-tier system, where `eq x` >
anything else matching `x` > `__`.

## Have a plan for functional dependencies

``` haskell
class MonadFoo a m | m -> a where
  foo :: a -> m ()

makeMockable ''MonadFoo
```

This fails, because the derived instance for `MonadFoo a (MockT m)` doesn't
satisfy the functional dependency: the same `MockT m` could be used for many
choices of `a`.

One can do this instead:

``` haskell
deriveMockable ''MonadFoo2

instance (Monad m, Typeable m) => MonadFoo2 Int (MockT m) where
  foo2 x = mockMethod (Foo2 x)
```

However, this is actually kind of anti-modular.  If you later end up defining an
instance for `String` instead of `Int`, in a different test or anything that you
import, you get an error because of the functional dependency again.

A work-around for this is to declare a `newtype`, like this:

``` haskell
newtype MyTestT m a = MyTestT {runMyTestT :: m a}
  deriving (Functor, Applicative, Monad)

instance (Monad m, Typeable m) => MonadFoo2 Int (MockT (MyTestT m)) where
  foo2 x = mockMethod (Foo2 x)
```

This is a bit of a pain.  I should think about how to help people do the right
thing here.

## Mockable with polymorphic return values.

``` haskell
class MonadFoo m where
    foo :: forall a. a -> m a

makeMockable ''MonadFoo
```

The derived instance fails to compile, because matching methods is no longer
enough to prove equality of the return types, so we cannot be sure that the
response has the right type for the actual call.

There are two things you might want in this case.  First, you may want to write
type-specific expectations like this:

``` haskell
-- Matches foo applied only to Bool.
mock $ whenever $ Foo_ __ :=> const (return True)
```

This is a perfectly good matcher and response, since the polymorphic return
type on the `Matcher` can unify with the `Bool` in the response.  However, it's
not implementable because there's no way (without a `Typeable a` constraint on
the method) to tell whether the actual call to the method is instantiating `a`
with `Bool`.  I don't know any way to cleverly dodge that requirement.
Reluctantly, I accept that in order to write this expectation, the user must
modify `MonadFoo` to add that `Typeable` constraint.

Second, you could try to write a polymorphic expectation, like this:

``` haskell
-- Matches foo applied to any type of argument.
mock $ whenever $ Foo_ __ :=> \(Foo x) -> return x
```

However, `(:=>)` has an ambiguous type here.  My instinct is to try to promote
the ambiguous type to a rank 2 type, thereby limiting what the programmer may
write to those things which can unify with *any* type acceptable to `foo`.  In
order to represent the relationship between types of the arguments and return
value, the type of `Matcher` would probably need to mention its argument types,
as well.  This could be done in a type-level list.

If we do this right and get lucky, it could provide the type-specific case,
since adding the (regrettably necessary) `Typeable` constraint to the method
would allow you to write `Foo_ (typed @Int __)` as a matcher.  Ideally, this
should also replace the existing behavior for polymorphic arguments, which are
already universally quantified with rank-n types in the Matcher.  But my
attempts to make this work have so far failed.
