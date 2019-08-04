# v0.5.2

 * Fix a bug about tuples.

# v0.5.0

 * Large change for sum types that used `constructorTagModifier`. The generated types are now unaffected! This is a breaking change for those who used this feature.

# v0.4.2

Drop support for `aeson < 1.`
Add support for `aeson == 1.2.*`

# v0.4.1

## Bugfixes
 * Fixed support for Elm 0.18 (see issue #17)

# v0.4.0
## New features
 * Support for Elm 0.18
 * Dropped support for Elm 0.17 and Elm 0.16

# v0.3.0
## New features
 * Support for Elm 0.17

# v0.2.2

## New features
 * The Elm JSON encoders and decoders now match `aeson` more closely. In partlicular, single constructor sum types are now encoded without
   the constructor. Also, the `aeson` 0.11 option `unwrapUnaryRecords` is now supported.

## Bugfixes
 * Fixed Elm type error in encoders for types like `[Map String v]` (0.2.1.2).

# v0.2.1

## New features

 * The template Haskell derivation functions now take `aeson` `Option` type instead of a custom type.
 This change makes it easier to synchronize the Haskell and Elm code.
 * The generated Elm code can be personalized. Helpers functions assist in converting type names, and defining which type will be newtyped.

## Notes

 * The generated Elm code depends on the [bartavelle/json-helpers](http://package.elm-lang.org/packages/bartavelle/json-helpers/1.1.0/) package.
