This is a library that allows for parsing and printing of DWARF debug information in .debug_* ELF sections.

For an introduction to the DWARF format, either read [this blog post](http://eli.thegreenplace.net/2011/02/07/how-debuggers-work-part-3-debugging-information) or refer to [the DWARF tutorial.](http://www.dwarfstd.org/doc/Debugging%20using%20DWARF.pdf)

Details concerning the binary layout of the format is specified in Section 7 - Data Representation of the DWARF 4 standard that can be found [there.](http://dwarfstd.org/doc/DWARF4.pdf)

Tested against x86_64 binaries generated with a private fork of the ocaml compiler suite with [DWARF emission](https://github.com/ocaml/ocaml/pull/574).
Modifications may be in order if the DWARF structure generation changed since and
when the DWARF emission feature hit trunk/a release.

# Dependencies

ocplib-endian, ocplib-elf

# Structure

* dwarfAbbrev.ml : Parsing and construction of .debug_abbrev abbreviations
* dwarfDIE.ml : Parsing and construction of .debug_info DIEs
* dwarfExprs.ml : DWARF expressions for location information
* dwarfLNP.ml : Parsing of Line Number Program bytecode
* dwarfLocs.ml : Parsing and construction of .debug_loc location lists
* dwarfPrinter.ml : DWARF structures printing
* dwarfTypes.ml : DWARF enumerations
* dwarfUtils.ml : Helper functions for binary data manipulation
* form.ml : Various data types that can be used to represent values in a attribute/form pair
* main_driver.ml : Entry point module
* xxd.ml : Hexadecimal view
* zipper.ml : Zipper library for DIE tree creation

# Supported debugging sections

Only sections truly relevant for debugging are handled here.
* ".debug\_info"
* ".debug\_line"
* ".debug\_abbrev"
* ".debug\_loc"

# TODO

* Handle modification of the DWARF data
* Handle writing (usually left to compilers)
* Specify which object file CU to read (by name)
* Support other remaining sections for sake of completeness (.debug\_frame, .debug\_ranges)

# Options

* "-xxd" : Output a hexadecimal view of target section data
* "-dot dot\_file cu\_num": Output DOT graph of a CU from debug\_info to dot\_file
* "-ml": Annotate OCaml parameters and variables with names, to use with debug\_loc on an OCaml CU
* "--section": Specify target section

# Examples

Show all location lists :
```
./ocp-dwarf.asm --section .debug_loc binary_name 2>&1 | less
```

Show location list of a OCaml CU in a OCaml binary :
```
./ocp-dwarf.asm -ml --section .debug_loc binary_name 2>&1 | less
```

Write a visual representation of the compilation unit's DIES as a tree :

```
./ocp-dwarf.asm --section .debug_info -dot test 0 binary_name
dot -Tpng test.dot -o test.png
