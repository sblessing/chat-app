from runners.output_parser import OutputParser

def setup(oBenchmarkRunner, cores, memory):
  oBenchmarkRunner.configure("chatapp", "caf/build/bin", memory, args = ["--scheduler.max-threads=" + str(cores), "--parseable"])

def gnuplot(cores, files, results):
  OutputParser(files).parse(cores, results) 