from runners.output_parser import OutputParser

def setup(oBenchmarkRunner, cores, phys_cores, scenario, memory):
  if cores < phys_cores:
    threads = cores
  else:
    threads = phys_cores

  #no side effect on the global scenario
  som_scenario = scenario.copy()

  for i in range(0, len(som_scenario)):
    # the SOMns launcher doesn't behave well,
    # needed to use a different name for arg
    if som_scenario[i] == '-b':
      som_scenario[i] = '-be'

  # the -t needs to go before the chat.ns
  oBenchmarkRunner.configure(
    "newspeak", "SOMns/som", memory, [["-t", str(threads)], ["newspeak/chat.ns", som_scenario]])

def gnuplot(cores, files, results):
  OutputParser(files).parse(cores, results)
