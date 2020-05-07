from runners.output_parser import OutputParser

def setup(oBenchmarkRunner, cores, phys_cores, memory):
  threads = phys_cores

  if cores < phys_cores:
    threads = cores

  oBenchmarkRunner.configure("pony", "pony/", memory, ["--parseable", "--ponymaxthreads", str(threads)])

def gnuplot(cores, files, results):
  OutputParser(files).parse(cores, results)
