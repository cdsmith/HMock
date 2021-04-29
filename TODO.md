## Deriving for multi-param type classes

``` haskell
class MonadMultiParam a m where
  multiParamMethod :: Int -> m ()
```

The current TH code doesn't work for multi-param classes, since it expects a
`Name` and then treats it as having the kind `(* -> *) -> Constraint` without
any additional parameters.  This means you cannot mock a fair number of
parameterized monads.

The TH methods need to take types as parameters, not names, so that you can ask
for an instance of `MonadError String`, for example, not just `MonadError`.

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

## Mockable for classes without `Show` / `Eq` on arguments.

``` haskell
class MonadFoo m where
    foo :: (Int -> Int) -> m Int
```

One challenge here is the `exactly` method, which requires at least `Eq` for all
arguments to implement correctly.  I wonder if `exactly` ought to be in a
subclass called something like `ExactMockable` instead, which would become the
constraint for `(|->)` .

There's an easier solution for `showAction` and `showMatch` .  These are only
used for error messages, so the TH-generated code should check whether a `Show`
instance is available, and if not, show a placeholder string for the unshowable
argument, possibly mentioning the type.  (The same check could decide whether it
generates an `ExactMockable` instance.)

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
