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

## Instance contexts

``` haskell
class MonadFoo a m where
  foo :: a -> m ()

makeMockable [t| MonadFoo Int |]
```

Using `makeMockable` here requires `FlexibleInstances`.  One can also ask
whether there should be a way to add context to the derived `Mockable` instance,
so that `FlexibleInstances` isn't needed.  For instance,
`makeMockableCtx [t| a ~ Int |] [t| MonadFoo a |]` could generate an instance
like

``` haskell
instance (a ~ Int) => Mockable (MonadFoo a) where {...}
```

In the instance, `foo` would have the type `a ~ Int => a -> m ()`, which is the
same as `Int -> m ()`.

## Mockable with polymorphic return values.

``` haskell
class MonadFoo m where
    foo :: a -> m a
```

Currently, this will fail to derive an instance.  Matching seems challenging in
this case.  Currently, the matcher relies on the idea that matching the methods
is enough to infer equality of return types, but now that's not the case.

Even more interesting is the desire to let people set up type-specific and
type-agnostic mocks.  That is, I'd like you to be able to write either

``` haskell
-- Matches foo applied only to Bool.
mock $ whenever $ Foo_ any_ :=> const (return True)
```

or

``` haskell
-- Matches foo applied to any type of argument.
mock $ whenever $ Foo_ any_ :=> \(Foo x) -> return x
```

## Mockable when the monad is mentioned in an argument.

``` haskell
class MonadFoo m where
    foo :: (Int -> m ()) -> m ()
```

In addition to being a non- `Show` able argument (see above), this poses a
problem because `Action MonadFoo a` doesn't specify the monad, which one needs
to know in order to set expectations properly.

It's possible this could be solved by changing the `Action` and `Match` types
to have the monad as a phantom parameter, similar to what was done with
`Expected` , so that `match` could unify them.

## More priorities for actions

Currently, it's an error when more than one `Matcher` in the expectations
applies to the current `Action`.  It's more common in other mock frameworks to
adopt rules for choosing which match to prefer.

* One such rule is to choose the most specific match.  This would mean adding
  some kind of partial order on predicates.  We'd probably just adopt a
  four-tier system, where `none_` > `eq_ x` > anythiong else matching `x` >
  `none_`.  Then instead of looking for a unique match, we're looking for a
  unique maximally specific match.
* Another rule is to have user-specified priorities.  A simple answer along
  these lines would be to have `whenever` get lower priority than `expect` and
  `expectN`, so it can be used to set a default action when there's no
  expectation.  One might then add an `expectMany` that is like `whenever` but
  without the low priority.
