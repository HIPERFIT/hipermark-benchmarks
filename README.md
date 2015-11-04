Hipermark benchmarks
====================

Various benchmarks and implementations based on the Hipermark infrastructure.

Trying it out
-------------

Remember do to a recursive checkout to get the `hipermark` submodule.
Then run:

    hipermark/hipermark instantiations/ 10 benchmarks/american_options haskell_sequential 8

Yes, the user interface could stand some improvement.  The above will
run the haskell_sequential implementation of the american_options
benchmark on the dataset with the name `8`, running the implementation
ten times and reporting the result.
