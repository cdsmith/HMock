# Revision history for hmock

## 0.x.0.0 -- 2021-??-??

* Dramatically simplified the Template Haskell API.
  * `makeMockable` now expects a Type instead of a Name.  Use `[t|MyClass|]`.
  * Most other variants of `makeMockable` have been removed.  Use
    `makeMockableWithOptions` instead.
  * `makeMockable` will now detect when instances already exist and won't
    redefine them.
  * `makeMockable [t|MyClass ConcreteType|]` now defines `Mockable` and
    `MockableBase` for any `MyClass` type.  Only `MockT` instances use the
    concrete type.  In some cases, you may need to add type annotations to your
    expectations.
* `MockSetup` can now add expectations.
* Added a lot more configuration for severity of faults:
  * `setAmbiguityCheck` can now set to ignore, warning, or error.
  * Added `setUninterestingActionCheck` for actions with no expectations.
  * Added `setUnexpectedActionCheck` for actions that don't match expectations.
  * Added `setUnmetExpectationCheck` for expectations that aren't met.

## 0.3.0.0 -- 2021-06-30

* Methods with polymorphic return types can now be mocked if the return type has
  a `Typeable` constraint.
* Added `whenever` to associate a side effect to a method.
* Added `WholeMethodMatcher` to match entire method args at once.
* `allowUnexpected` no longer changes the default for expected calls.

## 0.2.0.0 -- 2021-06-24

* Added ambiguity checking.
  * This is an optional feature, which is off by default.
  * To make it easier to avoid ambiguity, there is now an `allowUnexpected` that
  * causes unexpected calls to be ignored and optionally provide a response, but
    doesn't comflict with expectations that override it.  Ambiuguous uses of
    `expectAny` can often be replaced with `allowUnexpected`.
* Restricted mockable setup to defaults to avoid race conditions.
  * Setup handlers now run in the `MockSetup` monad.
  * Adding expectations from setup is no longer allowed.  However, you can use
    `allowUnexpected` to allow unexpected calls.
* Added `nestMockT` and `withNestedMockT` to the API.
* Exported smaller modules to make selective imports easier.

## 0.1.0.1 -- 2021-06-20

* Fixed a bad dependency that broke some GHC versions.

## 0.1.0.0 -- 2021-06-20

* First version. Released on an unsuspecting world.
