# Master's Thesis Code

<!-- TODO: How to clone -->
<!-- TODO: How to run -->

## Dependencies
<!-- TODO: Update dependencies -->

- llvm-9
- ghc
- cabal-install
  - cabal packages:
    - syb
    - llvm-hs
    - megaparsec

## CMM/r (CMM with records)

- TODO: how to align

```txt
1) type Name = data { x: Int; y: Int }
2) newtype Name = data { x: Int; y: Int }
```

1) creates a type alias
2) creates a new type

## CMM/r -> llvm

## CHMMM

- NCIS ("naming convention is syntax" or "naval criminal investigative service")

If we have `Name foo` then `foo.Name` is the accessor to the record (typed `Name`) and so `foo.Name.x` is a type-safe access to a field `x`
