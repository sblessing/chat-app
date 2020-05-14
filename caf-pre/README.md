# ChatApp

The ChatApp benchmark implementation the prerelease of CAF 0.18.

## Build instructions

```  
./configure --enable-standalone-build --build-type=release
make -C build
./bin/caf-prerelease (-h for chatapp options or --long-help for CAF options)  
```  

Instead of building CAF with the benchmark you can use a local install or point to a local CAF build director with the `--caf-root-dir=` option of the `configure` script.
