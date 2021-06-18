## Consider more flexible semantics

https://link.springer.com/content/pdf/10.1007/978-3-642-54804-8_27.pdf makes a
case for a more compositional and orthogonal semantics for mocks, which includes
ambiguity checking.

Open questions:

1. Can this be made ergonomic?  I'm not interested in this at all if it makes it
   harder to do the simple cases.
2. Should we restore ambiguity checking?  The article makes a strong case for
   it.  Earlier versions of HMock did check (dynamically) for ambiguous
   expectations, and could do so again.  (I have no interest in, nor can I even
   implement) static ambiguity checking, since HMock matchers are more flexible
   than those in the article.)
3. Is there a use case for arbitrary choice and repetition operators?  Probably
   so.
4. There seem to be two meanings to repetition.  One is `x* = empty + (x . x*)`,
   and the other is `x* = empty + (x || x*)`.  The paper linked above proposes
   only the first, but I'm not happy with this bias toward sequential
   expectations (which I believe should be the exception, not the rule).  On the
   other hand, the interleave rule is much more susceptible to ambiguity.  Maybe
   both should be offered.
5. Is there also a use case for an intersection operator?  Since responses are
   optional in HMock, it's possible we could add yet another operator, which
   requires that *two* execution plans match simultaneously.  It would be an
   error if both gave responses for the same action.  The reason this is
   compelling to me is that it lets you isolate certain properties of the call
   sequence, and enforce them "on the side" without worrying about how they
   modify your main expectations.  Example: I want to be able to say "if you
   open a file, you must close it", and I want to say that independent of
   whether I assert that you should open a specific filename.

## Consider failing lax mocks on mismatched parameters or multiplicity

gMock makes a distinction netween uninteresting and unexpected methods.  See
https://google.github.io/googletest/gmock_cook_book.html#uninteresting-vs-unexpected.
An uninteresting method is one that has no expectations at all.  An unexpected
method is one that DOES have expectations, but those expectations don't match
the arguments, multiplicity constraints, etc.  HMock could easily make a similar
distinction.  I'm not sure if it's a good idea.

One interesting use case is that in gMock, you can explicitly say "this should
never happen".  Since gMock is lax by default for uninteresting methods, this
converts a method from "uninteresting" (thus, accepted) to "unexpected" (thus,
rejected).

## Default setup per class

In many cases, there's a sort of natural default set of behaviors for mocks of a
class.  One might hope for a way to write these alongside `makeMockable`, so
that you don't have to separately export a `setup` action for each mockable
class.

A design might look something like this:

``` haskell
class MockSetup cls => Mockable cls where { ... }
class Mockable cls => MockSetup cls where
  setupMock :: Proxy cls -> MockT m ()
```

The contract would be that setupMock is called exactly once before the first
expectation is added *or* mocked method is resolved for a given class.

The reason for separating `MockSetup` from `Mockable` is that `Mockable` is
usually defined in Template Haskell, while `MockSetup` should be defined by the
user.  I don't really understand overlapping instances all that well, but I'm
tempted to say there should be an overlappable default instance for `MockSetup`
that does nothing at all.

## `byDefault` to override default responses

The built-in defaults for `mockMethod` and friends are closely tied to the
`Default` type class.  We want people to be able to customize them.  I will add
a `byDefault` operation, which replaces the default action for actions matching
a `Matcher`.  The expectation must have precisely one response, and that
response will be chosen instead of the built-in default when no response matches
an expectation.

`byDefault` is similar to `whenever`.  The difference is that `byDefault`
inserts a new default response for *other* expectations (include pre-existing
ones) that have no response.  On the other hand, `whenever` adds a new
expectation *on top of* the existing expectations.  Calls that match the
`whenever` will never even see the expectations masked by it.  This is similar
to the distinction between gMock's `ON_CALL().WillByDefault()` versus
`EXPECT_CALL().WillRepeatedly()`.

Open questions:

1. Should `byDefault` make the expectation lax?  I think no, as this is rather
   ad hoc.  You can use `whenever` to accomplish this, but only if there are
   no interesting matchers already added.

## Side effects?

Sometimes I want to add behavior (such as logging, new expectations, whatever)
to an action, but without preventing that action from satisfying additional
expectations.  What I want is effectively a side effect.  It's kind of like a
response, but it:

1. Is only run if the action is accepted without it.
2. Doesn't have a return value.
3. Doesn't fulfill the expectation.

The `whenever` name is perfect for this, so if this is implemented, I'd probably
first rename the existing `whenever` to `expectAny`, and then add this as
`whenever`.

## Better predicate descriptions

Some predicates could do a better job explaining why they succeed or fail.  For
example, `elemsAre` or `each` could explain which elements don't match.  This
means expanding the API to `Predicate` to include an optional explanation.

## Whole-method matching

Sometimes you want to be able to define matching predicates that span multiple
arguments to a method.  There are whole-method matchers in frameworks like gMock
to handle this.  For example, I may want to require that two arguments are equal
to each other.  HMock should have a way to do this.

The way to do this would be to add an instance for
`Expectable cls name m r (Predicate (Action cls name m r))`.  I think I have a
grand vision here that `(Predicate (Action cls name m r))` should become the
internal type used for matching actions.  But the problem is explainability and
partial matching.  I need:
* To be able to find the closest matches, so that I can suggest them when an
  action fails to match.
* To be able to explain why an action didn't match by referring to the specific
  argument or arguments that failed.

## Instances for more systems

MTL-style type classes are only one way of building an API layer for effectful
actions.  Some others include:

* Effect systems, like `eff`, `polysemy`, `fused-effects`, or `freer-simple`
* API layers like `servant` or `haxl`
* Plain old Haskell modules, with or without backpack

I'd like to evaluate how effectively I could wire HMock's internals to each of
these to expand the ability to mock more bits of code.

Many of these systems already have their own action types, which could be
trivially wrapped rather than deriving a new `Action` class.  This might mean
that `Action` should be an injective type family rather than a data family.
Then `Mockable` might need some kind of API distinction, such as writing
`instance Mockable (MTLStyle MonadFoo)`.  The TH code would need to be system-
specific.

For plain old Haskell modules, we'd need a global set of expectations rather
than tracking it in a monad wrapper.  Then we'd need `MockT` to be able to
delegate to that global system, and we'd need a way to reset it before and
after a test.  Maybe also track a version number, so that stray threads from
previous tests don't make a mess in the current test and just fail instead.  As
far as how to delegate to the mock implementations, this might be handled by
backpack, or by just creating a new module with an identical API and using CPP
in the system under test to import one or the other module.

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
