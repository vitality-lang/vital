import tables, clapfn, peg, strutils, bc_interpreter

when isMainModule:
  var parser = ArgumentParser(programName: "vital", fullName: "vital",
                              description: "the offical vitality compiler",
                              version: "0.0.0",
                              author: "Vitality <vitality.contact@proton.me>")


  parser.addRequiredArgument(name = "in_file", help = "Input file.")
  parser.addStoreArgument(shortName = "-o", longName = "--out", usageInput = "output",
                        default = "app.vbc", help = "Specify the output file.")
  let args = parser.parse()
  let entireFile = readFile(args["in_file"])
  if contains(args["in_file"], ".vit"):
    entireFile.compile args["out"]
  else:
    echo ""
