class OutputParser:
  def __init__(self, files):
    self._files = files

  def parse(self, cores, results):
    for file in self._files:
      with open(file, "r") as bench:
        index = 0

        for line in bench:
          components = line.split(",")
          results[components[0]][cores - 1] = float(components[1])
          index = index + 1
