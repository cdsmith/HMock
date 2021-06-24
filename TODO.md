## Instances for more systems

* Priority: High
* Accept Patch: Probably
* Complexity: Very High

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

## Response-less `allowUnexpected`

* Priority: High
* Accept Patch: Yes
* Complexity: Low

`allowUnexpected` takes an `Expectable`, but requires that it have one response.
Instead, it should allow zero or one response.  The one-response variant should
just be shorthand for combining the zero-response version and also calling
`setDefault` for the same matcher.  This should be documented, once it's true.

Implementation-wise, defaults are currently a list of (Bool, response).
Instead, there should be two lists: one for matchers that can be unexpected
(no responses there), and another for default responses (no Bool flag).

## Fix the API for TH generators

* Priority: High
* Accept Patch: Maybe (depends on UX concerns)
* Complexity: Low

20 variants based on combinatorial explosion is a lot!  I mean, using the record
syntax for options is annoying, but so it looking through a list of 20 methods
and wondering which variant does what you want.

## Side effects?

* Priority: Medium
* Accept Patch: Probably
* Complexity: Low

Sometimes I want to add behavior (such as logging, new expectations, whatever)
to an action, but without preventing that action from satisfying additional
expectations.  What I want is effectively a side effect.  It's kind of like a
response, but it:

1. Is only run if the action is accepted without it.
2. Doesn't have a return value.
3. Doesn't fulfill the expectation.

The perfect name for this operation is `whenever`.

The counter-argument is that this feature breaks abstraction boundaries.  Since
all methods are intercepted regardless of where they match in the expectation
set, changes that aren't related to the part of code you're thinking about can
nevertheless produce behavior in your test.  This is probably an acceptable
disadvantage, though, provided that it's documented clearly.

## Better predicate descriptions

* Priority: Medium
* Accept Patch: Yes
* Complexity: Medium to High

Some predicates could do a better job explaining why they succeed or fail.  For
example, `elemsAre` or `each` could explain which elements don't match.  This
means expanding the API to `Predicate` to include an optional explanation.

## Whole-method matching

* Priority: Medium
* Accept Patch: Yes
* Complexity: Medium to High

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

## Wrappers to save responses from integration tests.

* Priority: Medium
* Accept Patch: Probably
* Complexity: Very High

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

* Priority: Medium
* Accept Patch: Yes
* Complexity: Medium

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
expectAny $ Foo_ (typed @Bool anything) |-> True
```

Unifying the return types requires a `cast` in `matchAction`, but we should have
the constraints that we would need to implement this.

## Mockable with unconstrained polymorphic return values.

* Priority: Medium
* Accept Patch: Yes
* Complexity: High

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
expectAny $ Foo_ anything :-> \(Foo x) -> return x
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
