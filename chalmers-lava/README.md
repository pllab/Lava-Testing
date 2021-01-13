Chalmers Lava requires a few external executables to be installed on your system in order to use its verification wrappers.

## Minisat

To use `Lava.minisat`, you must have [`minisat`](https://github.com/niklasso/minisat) installed on your computer.
We've included it in a subidirectory here. If on a Mac, just do the following:

    cd minisat
    make install-bin

If on Linux, replace `-install_name` with `-soname=` in `minisat/Makefile`, then build and install as usual.

## SMV

To use `Lava.smv`, you must have [`nusmv`](https://nusmv.fbk.eu) installed on your computer.

## Chalmers Lava

We've updated the Chalmers Lava code as found in Hackage to work with `nusmv` and the required version of Haskell;
this code is found in `./chalmers-lava2000-1.6.1` and will be built automatically when you first try to run our
code in `chalmers-lava-testing`:

    cd chalmers-lava-testing
    cabal run

    # Or run the repl to experiment
    cabal repl