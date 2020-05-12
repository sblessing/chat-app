from runners.output_parser import OutputParser

def setup(oBenchmarkRunner, cores, phys_cores, scenario, memory):
  oBenchmarkRunner.configure("caf-head", "caf-head/build", memory, args = ["--parseable", "--scheduler.max-threads=" + str(cores)] + scenario)

def gnuplot(cores, files, results):
  OutputParser(files).parse(cores, results)
