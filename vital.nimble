# Package

version       = "0.1.0"
author        = "Vitality"
description   = "The Vital Compiler"
license       = "BSD-2-Clause"
srcDir        = "src"
bin           = @["vital"]


# Dependencies

requires "nim >= 1.6.10"

requires "npeg >= 0.27.0"
requires "clapfn >= 1.0.1"
requires "parsetoml >= 0.6.0"
requires "https://github.com/disruptek/frosty"

