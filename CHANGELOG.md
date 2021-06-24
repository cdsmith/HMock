# Revision history for hmock

## 0.2.0.0 -- 2021-06-22

* Exported smaller modules to make selective imports easier.
* Added ambiguity checking.
  * This is an optional feature, which is off by default.
  * To make it easier to avoid ambiguity, `byDefault` now causes a mock to allow
    unexpected calls, but doesn't comflict with expectations that override it.
    Ambiuguous Uses of `expectAny` can often be replaced with `byDefault`.
  * A new `setDefault` was added, which sets the default response but does not
    allow unexpected calls.
* Added nestMockT and withNestedMockT to the API.
* Restricted mockable setup to defaults to avoid race conditions.
  * Setup handlers now run in the `MockSetup` monad.
  * Adding expectations from setup is no longer allowed.  However, you can use
    `byDefault`, which now allows unexpected calls.

## 0.1.0.1 -- 2021-06-20

* Fixed a bad dependency that broke some GHC versions.

## 0.1.0.0 -- 2021-06-20

* First version. Released on an unsuspecting world.
