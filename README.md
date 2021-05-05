# HMock - A Flexible Mock Framework for Haskell

HMock provides a flexible mock framework for Haskell, with similar functionality
to Mockito for Java, GoogleMock for C++, and other mainstream languages.

## Quick Start

1.  Define classes for the functionality you need to mock.  To mock anything
    with HMock, it needs to be implemented using a `Monad` subclass.

    ``` haskell
    import Prelude hiding (readFile, writeFile)
    import qualified Prelude

    class MonadFilesystem m where
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
    makeMockable ''MonadFilesystem
    ```

4.  Set up expectations and run your code.

    ```haskell
    test_copyFile :: IO ()
    test_copyFile = runMockT $ do
      mock $ expect $ readFile_ "foo.txt" |-> "contents"
      mock $ expect $ writeFile_ "bar.txt" "contents" |-> ()

      copyFile "foo.txt" "bar.txt"
    ```

    * `runMockT` runs code in the `MockT` monad transformer.
    * `mock` adds an expectation or rule to the test.
    * `expect` expects a method to be called exactly once.
    * `readFile_` and `writeFile_` match the function calls.  They are defined
      by `makeMockable`.
    * `|->` separates the method call from its result.

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
with mocks.  When testing with mocks, two dangers to look out for are
over-assertion and over-stubbing.

**Over-assertion** happens when your test requires things you don't care about.
If you read two files, you usually don't care which order they are read in, so
your tests should pass with either order.  Even when your code needs to behave
a certain way, you usually don't want to check that in every single test.  Each
test should also ideally test one property.  However, a simplistic approach to
mocks may force you to over-assert just to run your code at all.

**Over-stubbing** happens when you remove too much functionality from your code,
and end up assuming part of the logic you intended to test.  This makes your
test less useful.  Again, a simplistic approach to mocks can lead you to stub
too much by not providing the right options for the behavior of your methods.

HMock is designed to help you avoid these mistakes, by offering:

### Flexible ordering

With HMock, you choose which constraints to enforce on the order of methods.
If certain methods need to happen in a fixed sequence, you can use `inSequence`
to check that.  But if you don't care about the order, you need not check it.
If you don't care about certain methods at all, `whenever` will let you set a
response without limiting when they are called.  Using `expectN`, you can make
a method optional, or limit the number of times it can occur.

These tools let you express more of the exact properties you intend to test, so
that you don't fall into the over-assertion trap.

### Flexible matchers

In HMock, you specify exactly what you care about in method parameters, by
using `Predicate`s.  A `Predicate a` is essentially `a -> Bool`, except that it
can be printed for better error messages.  If you want to match all parameters
exactly, there's a shortcut for doing so.  But you can also ignore arguments you
don't care about, or only make partial assertions about their values.  For
example, you can match a keyword in a logging message, without needing to copy
and paste the entire string into your test.

Because you need not compare every argument, HMock can be used to mock methods
whose parameters have no `Eq` instances.  You can write a mock for a method that
takes a function as an argument, for example.  You can even mock polymorphic
methods.

### Flexible responses

In HMock, you have a lot of options for what to do when a method is called.
For example:

1. You can look at the arguments.  Need to return the third argument?  No
   problem; just look at the `Action` that's passed in.
2. You can invoke other methods.  Need to forward one method to another?  Want
   to set up a lightweight fake without defining a new type and instance?  It's
   easy to do so.
3. You can add additional expectations.  Need to be sure that every opened file
   handle is closed?  The respone runs in `MockT`, so just add that expectation
   when the handle is opened.
4. You can perform actions in a base monad.  Need to modify some state for a
   complex test?  Need to keep a log of info so that you can assert a property
   at the end of the test?  Just run your test in `MockT (State Foo)` or
   `MockT (Writer [Info])`, and call `get`, `put`, and `tell` from your
   responses.

These flexible responses help you to avoid over-stubbing.  You can even set up
lightweight fakes using HMock to delegate, and not only does this avoid defining
a new type for each fake instance, but you can also easily inject errors and
other unusual behavior as exceptions to the fake implementation.

### Reusable mocks

With HMock, your mocks are independent of the specific monad stack or
combination of interfaces that your code uses.  You can write tests using any
combination of `Mockable` classes, and each part of your test code depends only
on the classes that you use directly.  This frees you to share convenience
libraries for testing, and reuse these components in different combinations
as needed.

## FAQ

Here are a few tips for making the most of HMock.

### What is the difference between `|->` and `:->`?

### What is the difference between `Foo_` and `foo_`?

### How do I mock methods with polymorphic arguments?

### How do I mock methods with polymorphic return types?

### How do I mock multi-parameter type classes?

### How do I mock classes with extra members?

### How do I mock classes with functional dependencies?

### How do I get better stack traces?

### How do I migrate from `monad-mock`?

To mock a class in monad-mock, you could use the Template Haskell `makeAction`
splice.  With HMock, you use `makeMockable` instead.  Unlike `makeAction`, you
write a separate `makeMockable` for each class you intend to mock, and the
generated code is usable with any combination of other classes in the same
tests.

So where you may have previously written:

``` haskell
makeAction ''MyAction [ts| MonadFilesystem, MonadDB |]
```

You will now write:

``` haskell
makeMockable ''MonadFilesystem
makeMockable ''MonadDB
```

To convert a test using monad-mock into a test using HMock, move expectations
from a list outside of `MockT` to a `mock` call inside `MockT`.  To preserve the
exact behavior of the old test, use `inSequence`.  You'll also need to change
your old action constructors to exact `Matcher`s, and change `:->` to `|->`.

If you previously wrote:

``` haskell
runMockT
  [ ReadFile "foo.txt" :-> "contents",
    WriteFile "bar.txt" "contents" :-> ()
  ]
  (copyFile "foo.txt" "bar.txt")
```

You will now write this:

``` haskell
runMockT $ do
    mock $ inSequence
      [ expect $ readFile_ "foo.txt" |-> "contents",
        expect $ writeFile_ "bar.txt" "contents" |-> ()
      ]
    copyFile "foo.txt" "bar.txt"
```

You may now begin to remove assertions that you aren't intending to test.  For
example, `inSequence` is overkill here, since the sequence is just a consequence
of data dependencies.  (Think of it this way: if it were magically possible for
`writeFile` to be called with the right arguments but without waiting on the
`readFile`, it would be correct to do so!  So the order is a consequence of the
implementation, not the specification.)  You can remove the `inSequence` and add
two independent expectations, instead.

``` haskell
runMockT $ do
    mock $ expect $ readFile_ "foo.txt" |-> "contents"
    mock $ expect $ writeFile_ "bar.txt" "contents" |-> ()
    copyFile "foo.txt" "bar.txt"
```

And you're done.