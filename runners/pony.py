from runners.output_parser import OutputParser

def setup(oBenchmarkRunner, cores, memory):
  oBenchmarkRunner.configure("pony", "pony/", memory, ["--parseable", "--ponythreads", str(cores)])

def gnuplot(cores, files, results):
  OutputParser(files).parse(cores, results)
