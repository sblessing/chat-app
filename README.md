# chat-app
Chat App: Cross-Language Actor Benchmark

## Requirements
  * Pony
  * CAF
  * SOMns
  * ABS
  * Python >= 3.5
  * gnuplot
  * tqdm

            pip install tqdm

## Compile Benchmarks
* Pony

        cd pony
        ponyc .

* CAF

        cd caf
        ./configure
        cd build
        make

* Newspeak

        # It's Recommended to point JAVA_HOME to a JDK 8
        git clone -b exp/chatapp --depth=1 --recursive --shallow-submodules \
                  https://github.com/smarr/SOMns
        cd SOMns
        ant compile

* ABS

## Providing a runner
        Providing a new runner is easy. Simply create a new <your_language>.py file in runners/ and explain
        to the framework (a) how to run your process and (b) how to interpret the output. For this, you are
        required to implement "setup" and "output". If your implementations adheres to the parseable output
        specification, the generic OutputParser is sufficient. If you have use some custom output, you need
        to supply your own OutputParser in output_parser.py.

        If you implementation compiles to a native binary, you need to supply your binary name in the first argument
        and in the second argument the path where this library is found relative to run.py. If you benchmark
        is invoked by some other process (for example /usr/bin/java) you need to supply the name of your language
        as first argument, and a fullpath to the binary as the second argument.

        Example:

        from runners.output_parser import OutputParser

        def setup(oBenchmarkRunner, cores, phys_cores, memory):
          # array of args can be nested
          oBenchmarkRunner.configure("<binary_or_language_name>", "<path_or_executable>", memory, <array_of_args>)

        def gnuplot(cores, files, results):
          OutputParser(files).parse(cores, results)

## Run Benchmarks

        python3 run.py [-r|--run pony|caf|abs|newspeak] [-l|--hyperthreads] [-p|--plot] [-m|--memory]

        -r | --run can be combined, that is all runners can be invoked in one go.
        -m | --memory measures memory consumption but slows down execution times.
        -p | --plot can be used even after having run the benchmarks.
