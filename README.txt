OCaml Binutils
==============

A set of libraries and tools to manipulate binary object files.

  - ocplib-elf: library to read/write ELF object files
  - ocplib-pecoff: library to read/write PE-COFF object files
  - ocplib-dwarf: library to read/write DWARF debugging info
  - ocplib-intelasm: library to assemble x86/amd64 instructions
  - ocplib-endian: library implementing string extractors for big/little-endian
  - ocplib-perffile: library to read Linux performance files (perf.data)

Expected Components:
--------------------

  - support of ELF format (Linux) [read/write]
    (started, partial read only)
  - support of PE-COFF format (Windows) [read/write]
    (started (FlexDLL), write only)
  - support of MACH-O format (Mac OS X) [read/write]
    (not started)
  - support of DWARF format [read/write]
    (started, types)

Long term objective:
--------------------

  - direct x86/amd64 backend for OCaml for Linux, Windows and Mac OS X.

Compilation
-----------

  - dependencies: ocplib-lang (typerex)
  - unarchive into typerex sources
  - use ocp-build
