# v0.2.1

## New features

 * The template Haskell derivation functions now take `aeson` `Option` type instead of a custom type.
 This change makes it easier to synchronize the Haskell and Elm code.
 * The generated Elm code can be personalized. Helpers functions assist in converting type names, and defining which type will be newtyped.

## Notes

 * The generated Elm code depends on the [bartavelle/json-helpers](http://package.elm-lang.org/packages/bartavelle/json-helpers/1.1.0/) package.
