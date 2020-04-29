{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "byte-codec"
, dependencies =
  [ "generics-rep", "integers", "sized-vectors" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
