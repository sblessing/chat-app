from runners.output_parser import OutputParser

def setup(oBenchmarkRunner, cores, phys_cores, scenario, memory):
  if cores < phys_cores:
    threads = cores
  else:
    threads = phys_cores

  for i in range(0, len(scenario)):
    # the SOMns launcher doesn't behave well,
    # needed to use a different name for arg
    if scenario[i] == '-b':
      scenario[i] = '-be'

  # the -t needs to go before the chat.ns
  oBenchmarkRunner.configure(
    "newspeak", "SOMns/som", memory, ["newspeak/chat.ns", ["-t", str(threads)] + scenario])

def gnuplot(cores, files, results):
  OutputParser(files).parse(cores, results)
