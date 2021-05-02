## Verify TH code with more different types

The TH code is not particularly hardened.  I suspect you would be able to break
it with lots of innocuous or cosmetic changes.  It needs some work toward
systematic completeness.

## Easier TH API for simple types

Especially since best practice is now to derive polymorphic instances for
multi-param classes, I'd like to revert to the simpler usage (so you write
`makeMockable ''MonadFoo` instead of `makeMockable [t|MonadFoo|]`).  The type
usage can be renamed.

## Derive ExactMockable more often

Currently, we only derive ExactMockable when the arguments to all methods are
monomorphic and instances of `Eq` and `Show`.  If the argument is typed by a
type class parameter, then we can add the necessary `Eq` and `Show` constraints
to the instance context.

Another thought is that it's unfortunate that `ExactMockable` is derived (or
not) for the entire class, when *almost* all methods might be exactly mockable
after all.  One wonders if a different design might be more convenient.  For
instance, exact versions could just be top-level functions, which are defined
for precisely the methods where they make sense, and have the necessary context
built in.  For instance:

``` haskell
class MonadFoo a m where
    f :: String -> m ()
    g :: Num b => b -> m ()
    h :: a -> m ()

instance Typeable a => Mockable (MonadFoo a) where
    data Matcher (MonadFoo a) :: * -> * where
        F_ :: Predicate String -> Matcher (MonadFoo a) ()
        G_ :: (forall b. Num b => Predicate b) -> Matcher (MonadFoo a) ()
        H_ :: Predicate a -> Matcher (MonadFoo a) ()
    ...

-- An exact matcher for f, since it has 
f_ :: Typeable a => String -> Matcher (MonadFoo a) ()
f_ x = F_ (eq x)

-- A parameterized exact matcher for h, since it is typed by a class param.
h_ :: (Typeable a, Eq a, Show a) -> Matcher (MonadFoo a) ()
h_ x = H_ (eq x)
```

## Have a plan for functional dependencies

``` haskell
class MonadFoo a m | m -> a where
  foo :: a -> m ()

makeMockable [t| MonadFoo |]
```

This fails, because the derived instance for `MonadFoo a (MockT m)` doesn't
satisfy the functional dependency: the same `MockT m` could be used for many
choices of `a`.

One can do this instead:

``` haskell
deriveMockable [t|MonadFoo2|]

instance (Monad m, Typeable m) => MonadFoo2 Int (MockT m) where
  foo2 x = mockMethod (Foo2 x)
```

However, this is actually kind of anti-modular.  If anyone later ends up
defining an instance for `String` instead of `Int`, in anything that you import,
you get an error.

A work-around for this is to declare a `newtype`, like this:

``` haskell
deriveMockable [t|MonadFoo2|]

newtype MyTestT m a = MyTestT {runMyTestT :: m a}
  deriving (Functor, Applicative, Monad)

instance (Monad m, Typeable m) => MonadFoo2 Int (MockT (MyTestT m)) where
  foo2 x = mockMethod (Foo2 x)
```

This is a bit of a pain.  I should think about how to help people do the right
thing here.

## More priorities for actions

Currently, it's an error when more than one `Matcher` in the expectations
applies to the current `Action`.  It's more common in other mock frameworks to
adopt rules for choosing which match to prefer.

* One such rule is to choose the most specific match.  This would mean adding
  some kind of partial order on predicates.  We'd probably just adopt a
  three-tier system, where `eq x` > anythiong else matching `x` >
  `__`.  Then instead of looking for a unique match, we're looking for a
  unique maximally specific match.
* Another rule is to have user-specified priorities.  A simple answer along
  these lines would be to have `whenever` get lower priority than `expect` and
  `expectN`, so it can be used to set a default action when there's no
  expectation.  One might then add an `expectMany` that is like `whenever` but
  without the low priority.

## Use source locations in mock messages.

We should be able to use the `HasCallStack` machinery to tell the user which
specific `expect` or `whenever` call we're referring to when talking about an
expectation.  This is nicer than just printing the method name and argument
predicates.  For example, if you have a sequence of three expectations, you can
see which one failed.

Similarly, we could try to capture the call stack in `mockAction` and include
it in errors.  Instead of just knowing there was an unexpected call from
somewhere unspecified and what the method and parameters were, you can look at
where the call actually came from.

## Mockable with polymorphic return values.

``` haskell
class MonadFoo m where
    foo :: forall a. a -> m a

makeMockable [t| MonadFoo |]
```

The derived instance fails to compile, because matching methods is no longer
enough to prove equality of the return types, so we cannot be sure that the
response has the right type for the actual call.

There are two things you might want to have happen in this case:

First, you could write a type-specific expectation, like this:

``` haskell
-- Matches foo applied only to Bool.
mock $ whenever $ Foo_ __ :=> const (return True)
```

This is a perfectly good matcher and response, since the polymorphic return
type on the `Matcher` can unify with the `Bool` in the response.  However, it's
not implementable because there's no way (without a `Typeable a` constraint on
the method) to tell whether the actual call to the method is instantiating
`a ~ Bool`.  I don't know any way to cleverly dodge that requirement.
Reluctantly, I accept that in order to write this expectation, the user must
modify `MonadFoo` to add that `Typeable` constraint.

Second, you could try to write a polymorphic expectation, like this:

``` haskell
-- Matches foo applied to any type of argument.
mock $ whenever $ Foo_ __ :=> \(Foo x) -> return x
```

`(:=>)` has an ambiguous type here.  My instinct is to try to promote the
ambiguous type to a rank 2 type, thereby limiting what the programmer may write
to those things which can unify with *any* type acceptable to `foo`.  I haven't
yet worked out what that would look like.

## Mockable when the monad is mentioned in an argument.

``` haskell
class MonadFoo m where
    foo :: (Int -> m ()) -> m ()
```

This poses a problem because `Action MonadFoo a` doesn't specify the monad,
which one needs to know in order to set expectations properly.

It's possible this could be solved by changing the `Action` and `Match` types
to have the monad as a phantom parameter, similar to what was done with
`Expected`, so that `match` could unify them.
