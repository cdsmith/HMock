# Revision history for hmock

## x.y.z.w -- YYYY-MM-dd

* Added optional ambiguity check.
* byDefault now makes a mock accept calls with default response.
* Added setDefault, with the old behavior of byDefault.
* Restricted mockable setup to avoid a race condition.
* Changed desugaring of multiple responses to reduce ambiguity.

## 0.1.0.1 -- 2021-06-20

* Fixed a bad dependency that broke some GHC versions.

## 0.1.0.0 -- 2021-06-20

* First version. Released on an unsuspecting world.
