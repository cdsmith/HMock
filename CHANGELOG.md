# Revision history for hmock

## 0.3.0.0 -- 2021-??-??

* Added `whenever` to associate a side effect to a method.
* Added `WholeMethodMatcher` to match entire method args at once.

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
