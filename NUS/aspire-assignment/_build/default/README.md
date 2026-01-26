# Aspire Assignment

This is a warm-up assignment to get you familiar with OCaml and Dune.

Before starting, make sure you have completed the [one-time setup
instructions](../setup/setup.md).

Note that this library uses the `sexplib` library for S-expression
serialization.  For documentation on that library, see
<<https://opam.ocaml.org/packages/sexplib/>> and
<<https://github.com/janestreet/sexplib>>.

## Task

Implement the functions in `bin/parse.ml` and `bin/eval.ml` to implement
a parser and evaluator for the Aspire language.

You can use the contents of `aspire-tests.tar.gz` as a basic test suite to test
your implementation.

Also, develop and submit your own test suite for up to 10% in bonus points.

When you submit, submit only the files `bin/parse.ml`, `bin/eval.ml`, and the
"tar ball" (i.e., extension `*.tar.gz`) containing your test suite.  We will
insert the `.ml` files into our own clean copy of the project, so make sure you
don't modify any other files.

## Setup

Install dependencies with the following (press Enter or type 'y' if prompted):

``` bash
dune build aspire-assignment.opam
opam install . --deps-only
```

## Build

Build with:

``` bash
dune build
```

## Run

Run the code with:

``` bash
dune exec -- aspire
```

Pass the `--help` flag to see usage information.

## Check for warnings

To check for warnings (submissions with warnings may be penalized), you must
first clean the build artifacts and then rebuild:

``` bash
dune clean
dune build
```

## Submit

When you are done, submit the files `bin/parse.ml`, `bin/eval.ml`, and the tar
ball of any test suite you created.
