Chalmers Lava requires a few external executables to be installed on your system in order to use its verification wrappers.

## Minisat

To use `Lava.minisat`, you must have [`minisat`](https://github.com/niklasso/minisat) installed on your computer.
We've included it in a subidirectory here. If on a Mac, just do the following:

    cd minisat
    make install-bin

If on Linux, replace `-install_name` with `-soname=` in `minisat/Makefile`, then build and install as usual.

## SMV

To use `Lava.smv`, you must have [`nusmv`](https://nusmv.fbk.eu) installed on your computer.