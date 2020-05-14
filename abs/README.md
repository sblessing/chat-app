# ChatApp Benchmark for ABS

Note that the default scenarios might take a long time to finish; consider
using smaller scenarios.

## Compile ABS

- Install Java JDK >= 8 and Erlang >= 22
- Compile ABS:

    git clone https://github.com/abstools/abstools
    cd abstools
    ./gradlew assemble

- Verify that it’s working:

    ./bin/bash/absc --help

(`bin/bash/absc` is a shell script; the compiler is contained in `dist/absfrontend.jar` and can be called with `java -jar absfrontend.jar --help`.)

## Compile and run the benchmark

    absc --erlang chatapp.abs default.abs
    ./gen/erl/run

ABS programs currently cannot access the command line; we supply some
scenarios in the abs files in this directory.

