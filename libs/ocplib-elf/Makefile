all:
	ocp-build ocp-elf

OBUILD=../../../_obuild/

ELF_FILES= ../endian/endianSig.ml \
  ../endian/littleEndian.ml  \
  ../endian/bigEndian.ml \
  elfTypes.ml \
  elfPrinter.ml \
  elfUtils.ml \
  elfReader.ml \
  elfWriter.ml

elf.ml: $(ELF_FILES) Makefile
	$(OBUILD)/ocp-pack/ocp-pack.asm -prefix `pwd` -o elf.ml $(ELF_FILES)
