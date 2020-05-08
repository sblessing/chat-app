from runners.output_parser import OutputParser

def setup(oBenchmarkRunner, cores, phys_cores, scenario, memory):
  print("Erlang!")

def gnuplot(cores, files, results):
  OutputParser(files).parse(cores, results)
