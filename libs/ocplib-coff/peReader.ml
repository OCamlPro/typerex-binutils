(* Everything LittleEndian

  PE file:
  * MS-DOS MZ Header
    "0x5A4D" 64 bytes long
    0x3C --> 4 bytes -> position of PE FILE SIGNATURE (field "e_lfanew")
        usually 0xE8.
  * MS-DOS Real-Mode Stub Program (often 128 bytes long)
  * PE FILE SIGNATURE "PE\000\000"
  * PE FILE HEADER
  * PE OPTIONAL HEADER
  * SECTION HEADERS
  * SECTIONS

// 1 byte aligned
struct PeHeader {
	uint32_t mMagic; // PE\0\0 or 0x00004550
	uint16_t mMachine;
	uint16_t mNumberOfSections;
	uint32_t mTimeDateStamp;
	uint32_t mPointerToSymbolTable;
	uint32_t mNumberOfSymbols;
	uint16_t mSizeOfOptionalHeader;
	uint16_t mCharacteristics;
};
// 1 byte aligned
struct Pe32OptionalHeader {
	uint16_t mMagic; // 0x010B - PE32, 0x020B - PE32+ (64 bit)
	uint8_t  mMajorLinkerVersion;
	uint8_t  mMinorLinkerVersion;
	uint32_t mSizeOfCode;
	uint32_t mSizeOfInitializedData;
	uint32_t mSizeOfUninitializedData;
	uint32_t mAddressOfEntryPoint;
	uint32_t mBaseOfCode;
	uint32_t mBaseOfData;
	uint32_t mImageBase;
	uint32_t mSectionAlignment;
	uint32_t mFileAlignment;
	uint16_t mMajorOperatingSystemVersion;
	uint16_t mMinorOperatingSystemVersion;
	uint16_t mMajorImageVersion;
	uint16_t mMinorImageVersion;
	uint16_t mMajorSubsystemVersion;
	uint16_t mMinorSubsystemVersion;
	uint32_t mWin32VersionValue;
	uint32_t mSizeOfImage;
	uint32_t mSizeOfHeaders;
	uint32_t mCheckSum;
	uint16_t mSubsystem;
	uint16_t mDllCharacteristics;
	uint32_t mSizeOfStackReserve;
	uint32_t mSizeOfStackCommit;
	uint32_t mSizeOfHeapReserve;
	uint32_t mSizeOfHeapCommit;
	uint32_t mLoaderFlags;
	uint32_t mNumberOfRvaAndSizes;
};
*)
