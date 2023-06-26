# HMock - A Flexible Mock Framework for Haskell

[![CI](https://github.com/cdsmith/HMock/actions/workflows/ci.yml/badge.svg)](https://github.com/cdsmith/HMock/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/HMock)](https://hackage.haskell.org/package/HMock)

[HMock](https://hackage.haskell.org/package/HMock) provides a flexible and
composable mock framework for Haskell, with functionality that generally
matches or exceeds that of Mockito for Java, GoogleMock for C++, and other
mainstream languages.

WARNING: Hmock's API is likely to change soon.  Please ensure you use an upper
bound on the version number.  The current API works fine for mocking with
MTL-style classes.  I want HMock to also work with effect systems, servant,
haxl, and more.  To accomplish this, I'll need to make breaking changes to the
API.

## Quick Start

1.  Define classes for the functionality you need to mock.  To mock anything
    with HMock, it needs to be implemented using a `Monad` subclass.

    ``` haskell
    import Prelude hiding (readFile, writeFile)
    import qualified Prelude

    class (Monad m) => MonadFilesystem m where
      readFile :: FilePath -> m String
      writeFile :: FilePath -> String -> m ()

    instance MonadFilesystem IO where
      readFile = Prelude.readFile
      writeFile = Prelude.writeFile
    ```

2.  Implement the code to test, using this class.

    ``` haskell
    copyFile :: MonadFilesystem m => FilePath -> FilePath -> m ()
    copyFile a b = readFile a >>= writeFile b
    ```

3.  Make the class `Mockable` using the provided Template Haskell splices.

    ``` haskell
    makeMockable [t|MonadFilesystem|]
    ```

4.  Set up expectations and run your code.

    ```haskell
    test_copyFile :: IO ()
    test_copyFile = runMockT $ do
      expect $ ReadFile "foo.txt" |-> "contents"
      expect $ WriteFile "bar.txt" "contents"

      copyFile "foo.txt" "bar.txt"
    ```

    * `runMockT` runs code in the `MockT` monad transformer.
    * `expect` expects a method to be called exactly once.
    * `ReadFile` and `WriteFile` match the function calls.  They are defined
      by `makeMockable`.
    * `|->` separates the method call from its result.  If it's left out, the
      method will return a default value (see `Data.Default`), so there's no
      need to specify `()` as a return value.

## Why mocks?

Mocks are not always the right tool for the job, but they play an important role
in testing practice.

* If possible, we prefer to test with actual code.  Haskell encourages writing
  much of the application logic with pure functions, which can be trivially
  tested.  However, this isn't all of the code, and bugs are quite likely to
  appear in glue code that connects the core application logic to its outside
  effects.

* If testing the actual code is not possible, we prefer to test with high
  quality fake implementations.  These work well for relatively simple effects.
  However, they are difficult to maintain when the behavior of an external
  system is complex, poorly specified, and/or frequently changing.  Incomplete
  or oversimplified fakes can make some of the most bug-prone code, such as
  error handling and unusual cases, very difficult to test.

* Use of a mock framework allows a programmer to test code that uses complex
  effectful interfaces, including all of the dark corners where nasty bugs tend
  to hide.  They also help to isolate test failures: when a component is broken,
  one test fails and is easy to find, rather than everything downstream failing
  at once.

## Why HMock?

HMock was designed to help Haskell programmers adopt good habits when testing
with mocks.  When testing with mocks, there are some dangers to look out for:

* **Over-assertion** happens when your test requires things you don't care
  about.  If you read two files, you usually don't care in which order they are
  read, so your tests should not require an order.  Even when your code needs to
  behave a certain way, you usually don't need to check that in every single
  test.  Each test should ideally test one property.  However, a simplistic
  approach to mocks may force you to over-assert just to run your code at all.

* **Over-stubbing** happens when you remove too much functionality from your
  code, and end up assuming part of the logic you wanted to test.  This makes
  your test less useful.  Again, a simplistic approach to mocks can lead you to
  stub too much by not providing the right options to get realistic behavior
  from your methods.

* **Fragile tests** happen when your expectations match too often or at
  unexpected times, leading to incorrect behavior that's merely an artifact of
  problems with mocks.  Mainstream mock frameworks are not particularly
  compositional, leading to frequent struggles with unexpected rules firing at
  off times and breaking other tests.

HMock is designed to help you avoid these mistakes, by offering:

### Flexible ordering constraints

With HMock, you choose which constraints to enforce on the order of methods.
If certain methods need to happen in a fixed sequence, you can use `inSequence`
to check that.  But if you don't care about the order, you need not check it.
If you don't care about certain methods at all, `expectAny` will let you set a
response without limiting when they are called.  Using `expectN`, you can make
a method optional, or limit the number of times it can occur.

These tools let you express more of the exact properties you intend to test, so
that you don't fall into the over-assertion trap.  This also unlocks the
opportunity to test concurrent or otherwise non-deterministic code.

### Flexible matchers

In HMock, you specify exactly what you care about in method parameters, by
using `Predicate`s.  A `Predicate a` is essentially `a -> Bool`, except that it
can be printed for better error messages.  If you want to match all parameters
exactly, there's a shortcut for doing so.  But you can also ignore arguments you
don't care about, or only make partial assertions about their values.  For
example, you can use `hasSubstr` to match a key word in a logging message,
without needing to copy and paste the entire string into your test.

Because you need not compare every argument, HMock can even be used to mock
methods whose parameters have no `Eq` instances at all.  You can write a mock
for a method that takes a function as an argument, for example.  You can even
mock polymorphic methods.

### Flexible responses

In HMock, you have a lot of options for what to do when a method is called.
For example:

1. You can look at the arguments.  Need to return the third argument?  No
   problem; just look at the `Action` that's passed in.
2. You can invoke other methods.  Need to forward one method to another?  Want
   to set up a lightweight fake without defining a new type and instance?  It's
   easy to do so.
3. You can add additional expectations.  Need to be sure that every opened file
   handle is closed?  The response runs in `MockT`, so just add that expectation
   when the handle is opened.
4. You can perform actions in a base monad.  Need to modify some state for a
   complex test?  Need to keep a log of info so that you can assert a property
   at the end of the test?  Just run your test in `MockT (State Foo)` or
   `MockT (Writer [Info])`, and call `get`, `put`, and `tell` from your
   responses.

These flexible responses help you to avoid over-stubbing.  You can even use
HMock to delegate to a lightweight fake.  Not only does this avoid defining
a new type for each fake instance, but you can also easily inject errors and
other unusual behavior as exceptions to the fake implementation.

### Composable mocking primitives

HMock's expectations expand on the ideas from Svenningsson, et al. in [An
Expressive Semantics of Mocking](
https://link.springer.com/content/pdf/10.1007%2F978-3-642-54804-8_27.pdf).
The key idea is to offer compositional primitives.  Anything you can do to a
single call, you can also do to an entire sequence of calls.  You can express
repeated sequences of calls, choice between two options, etc.  Because the core
mocking language is more expressive, you're less likely to need to stub out to
secondary mechanisms, such as recording calls and asserting about them later,
the way you may be used to in other languages, and this also makes your tests
more composable.

### Reusable mocks

With HMock, your mocks are independent of the specific monad stack or
combination of interfaces that your code uses.  You can write tests using any
combination of `Mockable` classes, and each part of your test code depends only
on the classes that you use directly.  This frees you to share convenience
libraries for testing, and reuse these components in different combinations
as needed.

You can also set up default behaviors for your mocks by implementing the
`Mockable` class manually, bundling sensible defaults with your derived mock
implementations for all users.

### Configurable Severity

HMock allows you to control the severity of several situations that can arise
during testing.  You can modify each of these situations to be `Ignore`d,
trigger a `Warning` but continue the test, or throw an `Error`, by passing the
severity to a `MockT` action as explained below.

These conditions whose severity you can adjust are:

1. **Ambiguous expectations**

   Ambiguious expectations happen when one action matches more than one
   expectation.  By default, this is `Ignore`d, and the most recently added
   expectation is chosen.

   Svenningsson, et al. argue that ambiguity resolution rules are non-composable
   and that enforcing ambiguity is therefore necessary for composability.  If
   you agree, you may want to set a higher severity for ambiguous expectations.
   To do so, use `setAmbiguityCheck`.

   Note that unlike Svenningsson, et al., HMock verifies ambiguity dynamically
   at runtime, so failures only occur when an actual action matches more than
   one expectation., and not when it's merely possible for this to occur.

2. **Uninteresting actions**

   An uninteresting action occurs when a method is called, but no expectations
   have been added for that method at all.  By default, this is an `Error`.
   However, some other mock frameworks (for example, gMock) allow you to ignore
   uninteresting actions.  To do so in HMock, use `setUninterestingActionCheck`
   to change this severity.

   Ignoring uninteresting methods is non-composable.  Adding an expectation for
   a method in one part of your test will cause the method to be considered
   "interesting", which can cause an unrelated part of the test to fail.

   Note that `setUninterestingActionCheck Error` (the default) actually treats
   uninteresting methods as unexpected.  If you set a weaker severity for
   unexpected actions, uninteresting actions will also follow that severity.

3. **Unexpected actions**

   An unexpected action occurs when an action is called that has no
   corresponding expectation.  By default, this is an `Error`.  Note that unless
   uninteresting actions are also set to `Error`, an action is only unexpected
   if there is at least one expectation added for the method.  To change the
   severity of unexpected actions, use `setUnexpectedActionCheck`.

   This is generally intended as a temporary technique for collecting
   information about which expectations are needed for a test.  You should be
   careful about leaving it this way.  Prefer `allowUnexpected` if you just want
   to ignore some specific unexpected actions.

4. **Unmatched expectations**

   An unmatched expectation is an expectation that is not matched by any action
   that occurs during the test.  By default, this is an `Error`.  You can change
   this severity by calling `setUnmetExpectationCheck`.

   This is generally intended as a temporary technique for collecting
   information about which expectations are needed for a test.  You should be
   careful about leaving it this way.

## FAQ

Here are a few tips for making the most of HMock.

### What is the difference between `|->` and `|=>`?

In the most general form, an HMock rule contains a response of the form
`Action ... -> MockT m r`.  The action contains the parameters, and the `MockT`
monad can be used to add expectations or do things in the base monad.  You can
build such a rule using `|=>`.

However, it's very common that you don't need this flexibility, and just want
to specify the return value.  In that case, you can use `|->` instead to keep
things a bit more readable.  `m |-> r` is short for `m |=> const (return r)`.

As a mnemonic device for remembering the distinction, you can think of:

* `|->` as ASCII art for `↦`, which associates a function with a result in
  mathematical notation.
* `|=>` as a relative of Haskell's `>>=`, and binds an operation to a Kleisli
  arrow.

### What is the difference between `foo`, `Foo`, and `Foo_`?

These three names have subtly different meanings:

* `foo` is the method of your own class.  This is the function used in the code
  that you are testing.
* `Foo` is an `Action` constructor representing a call to the method.  You will
  typically use this in three places: in an expectation when you know the exact
  expected arguments, as the argument to `mockMethod` and friends, and as a
  pattern to get the parameters in a response.
* `Foo_` is the `Matcher` constructor, and expects `Predicate`s that can match
  the arguments in more general ways without specifying their exact values.
  This is more powerful, but a bit wordier, than writing an expectation using
  the action `Foo`.  You must also use `Foo_` for expectations when the method
  parameters lack `Eq` or `Show` instances.

### Can I mock only some methods of a class?

Yes!

The `makeMockable` splice is the simple way to set up mocks for a class, and
delegates everything in the class to HMock to match with expectations.  However,
sometimes you either can't or don't want to delegate all of your methods to
HMock.  In that case, use the `makeMockableWithOptions` splice, instead, and set
`mockDeriveForMockT` to `False`.  This implements most of the deeper boilerplate
for HMock, but doesn't define the instance for `MockT`.  You will define that
yourself using `mockMethod` and friends.

For example:

``` haskell
class MonadFoo m where
  mockThis :: String -> m ()
  butNotThis :: Int -> m String

makeMockableWithOptions [t|MonadFoo|] def { mockDeriveForMockT = False }

instance (Monad m, Typeable m) => MonadFoo (MockT m) where
  mockThis x = mockMethod (MockThis x)
  butNotThis _ = return "fake, not mock"
```

If your class has methods that HMock cannot handle, then you **must** do this.
These include things like associated types, methods that don't run in the monad,
or methods with non-`Typeable` polymorphic return values.

### How do I mock methods with polymorphic arguments?

HMock can be used to write mocks with polymorphic arguments, but there are a few
quirks to keep in mind.

First, let's distinguish between two types of polymorphic arguments.  Consider
this class:

``` haskell
class MonadPolyArgs a m where
  foo :: a -> m ()
  bar :: b -> m ()
```

In `foo`, the argument type `a` is bound by the *instance*.  Instance-bound
arguments act just like concrete types, for the most part, but check out the
later question about multi-parameter type classes for some details.

In `bar`, the argument type `b` is bound by the *method*.  Because of this, the
`Matcher` for `bar` will be assigned the rank-n type
`(forall b. Predicate b) -> Matcher ...`.  In fact, pretty much the only
`Predicate` you could use in such a type is `anything` (which always matches, no
matter the argument value).  Since `eq` is not legal here, the corresponding
`Action` type will not get an `Expectable` instance, so you may not use it
to match an exact call to `bar`.

In order to write a more specific predicate, you'd need to add constraints to
`bar` in the original class.  Understandably, you may be reluctant to modify
your functional code for the sake of testing, but in this case there is no
alternative.  Any constraints that you add to the method can be used in
`Predicate`s in the `Matcher`.  For example, if `bar` can be modified to add a
`Typeable` constraint, then you can use a predicate like `typed @Int (lt 5)`,
which will only match calls where `b` is `Int`, and also less than 5.

### How do I mock methods with polymorphic return types?

Again, we can distinguish between type variables bound by the instance versus
the method.  Variables bound by the instance work much the same as concrete
types, but check out the question about multi-parameter type classes for some
details.

To mock a method with a polymorphic return value bound by the method itself, the
return value must have a `Typeable` constraint.  If it cannot be inferred, you
will also need to add a type annotation to the return value you set for the
method, so that GHC knows the type.  If the method will be used at different
return types, you must add separate expectations to the method for each type at
which it will be used.

### Why do I need a Default instance for `mockMethod`?

`mockMethod` uses the `Default` class from `data-default` to decide what to
return when no other response is given for an expectation.  If the method you
are mocking has no `Default` instance for its return type, you can use
`mockDefaultlessMethod` instead.  In this case, if there's no response
specified, the method will return `undefined`.

This choice is made automatically if you derive the instances for `MockT` using
Template Haskell.

### How can I change the default behavior of mocked methods?

There are a few ways to do this:

1. To just change the default behavior, use `byDefault`.  A method call must
   still be expected or it will fail, but if you leave out the return value in
   the expectation, this default will be used.
2. To also make unexpected calls to a method suceed, use `allowUnexpected`.  If
   you include a response in the argument, it will become the default in
   addition to allowing an unexpected method.
3. To set up defaults for *all* users of the mock, replace your call to
   `makeMockable` with a call to `makeMockableWithOptions` and set
   `mockEmptySetup` to `False`.  Then write an instance for `Mockable` and
   implement `setupMockable` to do whatever you like.  This setup will always
   run before the first time HMock touches your class from any test.

### How do I stop unexpected actions I don't care about from failing my tests?

* If there is a specific method that you don't care about, use `allowUnexpected`
  to ignore occurrences of that method.
* There's also a heuristic you can enable, where so-called "uninteresting"
  methods are ignored.  An uninteresting method is one for which no expectations
  are added in the entire test.  To allow these methods to succeed, use
  `setUninterestingActionCheck Ignore` or `setUninterestingActionCheck Warning`.
  This is similar to the behavior of gMock, which has a similar notion of
  uninteresting calls.
* Finally, the biggest hammer is to use `setUnexpectedActionCheck Warning`.
  This will allow any unexpected action in your test.  You almost certainly
  don't want to do this in your final test, but it can be useful during the
  course of writing the test.

### What if there are two expectations that match the same method?

By default, the most recently added expectation is matched.  Think of
expectations as being a stack, so they are first-in, first-matched.  This rule
makes HMock more compositional, since you can add and satisfy expectations in a
part of your test without worrying that expectations from a larger containing
block will interfere.

There is also an option to fail when more than one expectation matches.  To
enable this, just include `setAmiguityCheck True` as a statement in `MockT`.
From that point forward, ambiguous matches will throw errors.

### How do I mock multi-parameter type classes?

In order to mock a multi-parameter type class, the monad argument `m` must be
the last type variable.  Then just use `makeMockable [t|MonadMPTC|]`.

### How do I mock classes with functional dependencies?

We will consider classes of the form

``` haskell
class MonadMPTC a b c m | m -> a b c
```

If you try to use `makeMockable [t|MonadMPTC|]`, it will fail.  The functional
dependency requires that `a`, `b`, and `c` are determined by `m`, but we cannot
determine them for the `MockT` instance.

The recommended way to handle this case is to pass a concrete type to
`makeMockable`, like this:

``` haskell
makeMockable [t| MonadMPTC Int String Int |]
```

This will define the same `Mockable` instance for `MonadMPTC`, but the instance
for `MockT` will be defined with concrete types as required by the functional
dependency.

Note that the `MockT` instance is anti-modular, because you cannot import (even
indirectly) two different instances for `MockT` with different types in the same
module.  These instances would be *incoherent*, which Haskell doesn't typically
allow.  You can minimize the risk by using `makeMockable` in top-level test
modules which are never imported elsewhere.  If you want to share the
expectation code, you can use `makeMockableWithOptions` and set
`mockDeriveForMockT` to `False` in your library code, and then use
`makeMockable` again in your top-level test module to define the `MockT`
instance.

If you absolutely need to write multiple tests in the same module with different
type parameters, you will need to use a wrapper around the base monad for
`MockT` to disambiguate the instances.  That is a bit more involved.  Here's an
example:

``` haskell
makeMockableWithOptions [t|MonadMPTC|] def {mockDeriveForMockT = False}

newtype MyBase m a = MyBase {runMyBase :: m a}
  deriving (Functor, Applicative, Monad)

instance
  (Monad m, Typeable m) =>
  MonadMPTC String Int String (MockT (MyBase m))
  where
  foo x = mockMethod (Foo x)
```

### How do I test multithreaded code?

If your code uses `MonadUnliftIO` to create threads, you can test it directly
with HMock.  Otherwise, you can use `withMockT` to manually inject each of your
threads into the same `MockT` block.  Whichever way you do it, the expectations
are shared between threads so that an expectation added in one thread can be
fulfilled by the other.

If you don't want to share expectations, then you can use `runMockT` once per
thread to run each thread with its own set of expectations.

### How do I test code with exceptions?

You can use either the `exceptions` or `unliftio` packages to throw and catch
exceptions from code tested with `MockT`.

The behavior of `HMock` is unspecified if you continue testing after throwing
asynchronous exceptions to your threads using `throwTo`.  This concern doesn't
apply to synchronous exceptions thrown with `throwIO` or `throwM`.

### How do I get better stack traces?

HMock is compatible with stack traces using `HasCallStack`.  These can be very
convenient for finding out where your code went wrong.  However, the stack
traces are useless unless you add a `HasCallStack` constraint to the methods of
your class.  This is unfortunate, but not really avoidable with the current
state of Haskell.  You can add the constraint when troubleshooting, and remove
it again when you are done.

### What should I do about orphan instance warnings?

If you have the warning enabled, GHC will usually warn about orphan instances
when you use `makeMockable`.  We recommend disabling this warning for the
modules that use `makeMockable`, by adding the line
`{-# OPTIONS_GHC -Wno-orphans #-}` to the top of these modules.

Prohibiting orphan instances is just a *heuristic* to make it less likely that
two different instances will be defined for the same type class and parameters.
The heuristic works well for most application code.  It does **not** work so
well for HMock, because the class you are mocking is non-test code, but the
`MockableBase`, `Mockable`, and `MockT` instances should be defined in test
code.

Since the orphan heuristic doesn't work, you must take responsibility for
managing the risk of multiple instances.  The easiest way to do so is to avoid
defining these instances in libraries.  If you do define instances in libraries,
you must choose a canonical location for each instance that is consistent across
all code using the library.

### Why is my method "too complex to expect with an `Action`"?

When adding an expectation, you can only use an `Action` if the method is simple
enough.  Specifically, all arguments must have `Eq` and `Show` instances, and no
arguments may rely on type variables bound by the method.

If your method isn't simple enough, the solution is to add the expectation with
a `Matcher` instead of an `Action`.  The arguments to `Matcher` are `Predicate`s
that can inspect the value and decide whether to match.  You are now responsible
for deciding how to match the complex argument.  Some options include:

1. Using a polymorphic `Predicate` like `anything`.
2. Ensuring that a `Typeable` constraint is available, and using the `typed`
   predicate to cast the argument to a known type.
3. Using the `is` or `with` `Predicate`s and your own code that's polymorphic in
   the type and produces a monomorphic result type you can match on.

### How do I migrate from `monad-mock`?

To mock a class with the `monad-mock` package, you will have used that library's
Template Haskell splice called `makeAction`.  With HMock, you should use
`makeMockable` instead.  Unlike `makeAction`, you will use `makeMockable`
separately for each class you intend to mock.  The generated code is still
usable with any combination of other classes in the same tests.

Where you may have previously written:

``` haskell
makeAction ''MyAction [ts| MonadFilesystem, MonadDB |]
```

You will now write:

``` haskell
makeMockable [t|MonadFilesystem|]
makeMockable [t|MonadDB|]
```

To convert a test that uses `monad-mock` into a test using HMock, move
expectations from the list argument of `runMockT` into `expect` calls inside
HMock's `runMockT`.  To preserve the exact behavior of the old test, wrap your
`expect`s with `inSequence`.  You'll also need to switch from `monad-mock`'s
`:->` to HMock's `|->`, which means the same thing.

If you previously wrote (with monad-mock):

``` haskell
runMockT
  [ ReadFile "foo.txt" :-> "contents",
    WriteFile "bar.txt" "contents" :-> ()
  ]
  (copyFile "foo.txt" "bar.txt")
```

You will now write this (with HMock):

``` haskell
runMockT $ do
    inSequence
      [ expect $ ReadFile "foo.txt" |-> "contents",
        expect $ WriteFile "bar.txt" "contents" |-> ()
      ]
    copyFile "foo.txt" "bar.txt"
```

Now that your test has been migrated without changing its behavior, you may
begin to remove assertions that `monad-mock` forced you to write even though you
didn't intend to test them.  For example:

* You don't really care about the return value for `writeFile`.  HMock will
  return a default value for you if you leave out the `|->` operator.

* `inSequence` is overkill here, since the sequence is just a consequence of
  data dependencies.  (Think of it this way: if it were magically possible for
  `writeFile` to be called with the right arguments but without waiting on the
  `readFile`, it would be correct to do so!  The order is a consequence of the
  implementation, not the specification.)

Applying these two simplifications, you have a final test:

``` haskell
runMockT $ do
    expect $ ReadFile "foo.txt" |-> "contents"
    expect $ WriteFile "bar.txt" "contents"

    copyFile "foo.txt" "bar.txt"
```

### Which GHC versions are supported?

HMock is tested with GHC versions from 8.6 through 9.4.

## Case Study: Mocking Template Haskell

As a non-trivial case study in the use of HMock, consider the problem of testing
code that uses Template Haskell.  Template Haskell runs in a monad class called
`Quasi`, which provides access to actions that help build code: generating fresh
names, looking up type information, reporting errors and warnings, and so on.
While there is an `IO` instance for the `Quasi` type class, it throws errors for
most operations, making it unsuitable for testing any non-trivial uses of
Template Haskell.

As part of HMock's own test suite, the `Quasi` monad is made mockable (in
`test/QuasiMock.hs`), and then used (in `test/Classes.hs`) to test HMock's
Template Haskell-based code generation.

At first glance, this might seem unnecessary.  After all, the unit tests make
use of `makeMockable` for tests of the core HMock functionality, so surely any
problems in that code that matter would cause one of the core tests to fail, as
well.  However, writing these tests with mocks had two significant benefits:

1. Because Template Haskell runs at compile time, test coverage cannot be
   measured.  Template Haskell code at runtime generates accurate test coverage
   using `hpc`.  This, in turn, helped with writing more comprehensive tests.

2. Because Template Haskell errors would stop the tests from compiling, core
   tests can only cover *successful* uses.  Mocking `Quasi` allows tests of
   Template Haskell to check the error cases, as well.

Indeed, mock `Quasi` tests were quite valuable to HMock development.  First, the
initial tests revealed several places where tests did not exercise key logic:
mocking classes with superclasses, and mocking classes whose methods have rank-n
parameters.  When corresponding tests were added, both of these cases turned out
to be incorrect!  Next, adding tests for the error cases (which would not have
been possible to write without mocks) revealed that the code to detect mocking
classes with too many arguments was also broken, so that instead of a nice
helpful message, HMock printed something about an internal error, advising the
user to report a bug.

The implementation of the `Quasi` mock was not difficult, but there are a few
places where it was illuminating:

* Several methods of the `Quasi` type class could not be mocked by HMock because
  they have polymorphic return types without `Typeable` constraints.  This did
  not prevent using HMock, but it did make it necessary to use a hand-written
  `MockT` instance, rather than using `makeMockable` to generate everything.

* One method, `qNewName`, could have been mocked, but it wasn't the right
  choice to do so.  Template Haskell already provides an implementation in the
  `IO` monad, which was already suitable for testing.  This wasn't a problem, as
  the `MockT` instance could be written to forward to the `IO` implementation.

* Certain behaviors that did need to be mocked, such as looking up `Eq` and
  `Show` instances for common types, were useful for many different tests.  To
  help with reuse, these actions are added from `setupMockable` so they are
  automatically mocked every time the class is used.

* The mocks and setup code required less than 50 lines of very straight-forward
  code.  There was, though, a need to write more code to derive `Lift` and
  `NFData` instances for Template Haskell classes so that the correct behavior
  of the mock could be implemented and tested.

All things considered, the mock of Quasi was not difficult to implement, and
improved both the experience of HMock development and confidence in its
correctness.
