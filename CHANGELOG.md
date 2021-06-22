# Revision history for hmock

## 0.2.0.0 -- 2021-06-22

* Separated modules to make selective imports easier.
* Added optional ambiguity check.
* byDefault now causes a mock to accept unexpected calls.
* Added setDefault, with the old behavior of byDefault.
* Restricted mockable setup to avoid a race condition.
* Changed desugaring of multiple responses to reduce ambiguity.

## 0.1.0.1 -- 2021-06-20

* Fixed a bad dependency that broke some GHC versions.

## 0.1.0.0 -- 2021-06-20

* First version. Released on an unsuspecting world.
