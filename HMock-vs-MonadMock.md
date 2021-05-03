# HMock versus monad-mock

Alexis King's monad-mock library was a major inspiration for HMock.  Here is a
summary of the differences between the two.

## Flexible ordering constraints

When you write a test with monad-mock, you must record the exact sequence of
methods that will be invoked, and any change in that sequence causes the test to
fail.  A test can fail, for example, if files are read in a different order, or
because a new logging statement was added that you don't care about.

With HMock, you choose which constraints to enforce on the sequence of methods.
If certain methods need to happen in a fixed sequence, you can use `inSequence`
to check that.  But if you don't care about the order, you need not check it.
If you don't care about certain methods at all, `whenever` will let you set a
response without limiting when they are called.  Using `expectN`, you can make
a method optional, or limit the number of times it can occur.

These tools let you express the exact properties you intend to test, so that
you don't over-assert.

## Flexible matchers

To use monad-mock, the exact parameters of a mocked method must be listed in the
specification.  Any change from those parameters leads to a test failure.  If
you're not careful, your tests can become brittle because of unimportant
details such as the exact text of logging messages.  This also limits the types
of methods that you can test with monad-mock, since you must be able to compare
the arguments for equality.

In HMock, you specify exactly what you care about in method parameters, by
using `Predicate`s.  A `Predicate a` is essentially `a -> Bool`, except that it
can be printed for better error messages.  If you want to match all parameters
exactly, there's a shortcut for doing so; but you can also ignore arguments you
don't care about, or only make partial assertions about their values.  For
example, you can match a keyword in a logging message, without needing to copy
and paste the entire string into your test.

Because you need not compare every argument, HMock can be used to mock methods
whose parameters are not `Eq` instances.  You can write a mock for a method that
takes a function as an argument, for example.  You can even mock polymorphic
methods.

## Powerful responses

The response to a method in monad-mock is just a return value.  In HMock, you
can do a lot more when a method is called.  For example:

1. You can look at the arguments.  Need to return the third argument?  No
   problem; just look at the `Action` that's passed in.
2. You can set additional expectations.  Need to be sure that every opened file
   handle is closed?  Just add that expectation when it's opened.
3. You can perform actions in a base monad.  Need to modify some state for a
   complex test?  Need to keep a log of info so that you can assert a property
   at the end of the test?  Just run your test in `MockT (State Foo)` or
   `MockT (Writer [Info])`, and call `get`, `put`, and `tell` from your
   responses.

## Reusable mocks

When using monad-mock, you gather all the various mtl-style classes you will be
using, and define a single action type that spans all of them.  Your test code
then depends on that global action type.

With HMock, each mtl-style class you use has its own `Action` type.  You can
write tests using any combination of classes, and each part of your test code
depends only on the classes that you use directly.  This means that you can
define mocks and build convenience libraries for testing specific classes or
combinations of classes, and reuse these components in different combinations
as needed.
