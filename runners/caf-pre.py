from runners.output_parser import OutputParser

def setup(oBenchmarkRunner, cores, phys_cores, scenario, memory):
  oBenchmarkRunner.configure("caf-pre", "caf-pre/build", memory, args = ["--parseable", "--scheduler.max-threads=" + str(cores)] + scenario, exclude = ['config.status'])

def gnuplot(cores, files, results):
  OutputParser(files).parse(cores, results)
