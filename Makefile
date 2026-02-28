# Makefile for SegFX demo
BEAST_DIR = $(HOME)/src/microbeast
SJASMPLUS_DIR = $(BEAST_DIR)/sjasmplus
CPMTOOLS_DIR = $(BEAST_DIR)/cpmtools-2.23

SJASMPLUS = ${SJASMPLUS_DIR}/sjasmplus

# CP/M stuff
CPM_DISK_TYPE = -f memotech-type50
CPM_IMAGE = segfx_p25.img

MKFS = $(CPMTOOLS_DIR)/mkfs.cpm
MKFS_OPTS = $(CPM_DISK_TYPE) -b cpm22.bin 

CP = $(CPMTOOLS_DIR)/cpmcp
CP_OPTS = $(CPM_DISK_TYPE)

LS = $(CPMTOOLS_DIR)/cpmls
LS_OPTS = $(CPM_DISK_TYPE)


# NB TARGET set by OUTPUT directive in .asm file
ASM_FILES = $(wildcard *.asm)

all: demo.com

build: all

demo.com: demo.asm
	$(SJASMPLUS) --nologo --lst=demo.lst $^


clean:
	rm -f demo.com demo.lst

disk: demo.com
	$(MKFS) $(MKFSOPTS) $(CPM_IMAGE)
	$(CP) $(CP_OPTS) $(CPM_IMAGE) demo.com 0:demo.com
	$(LS) $(LS_OPTS) $(CPM_IMAGE)
	

.PHONY: all clean test disk

