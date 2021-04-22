# Category Breadcrumb

[![CI](https://github.com/haskell-fun/CategoryBreadcrumb/actions/workflows/ci.yml/badge.svg)](https://github.com/haskell-fun/CategoryBreadcrumb/actions/workflows/ci.yml)

## How to build and run locally

The project uses the [Haskell tool stack](https://docs.haskellstack.org/en/stable/README/).

Assuming `stack` is installed in the system, the project can be build by running

```bash
stack build
```

To build and also run the tests, run

```bash
stack test
```

which is equivalent to

```bash
stack build --test
```

To run the executable,

```bash
stack exec category-breadcrumb-exe
```

For a faster feedback loop,

```bash
stack test --fast --file-watch
```

To run `ghci` (with a version compatible with the resolver) run

```bash
stack ghci
```

For more information, refer to the `stack` official docs.
