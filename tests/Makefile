BSC ?= bsc

BUILDDIR = build
BDIR = $(BUILDDIR)/bdir
SIMDIR = $(BUILDDIR)/simdir

OUTPUTDIR = output

BSCFLAGS = -p +:$(CURDIR)/..
BSCFLAGS += -bdir $(BDIR)

all: sim-SourceSinkWidthShimsTest sim-VectorExtraTest

#sim-SourceSinkWidthShimsTest: SourceSinkWidthShimsTest.bsv
#	mkdir -p $(OUTPUTDIR)/$@-info $(BDIR) $(SIMDIR)
#	$(BSC) -info-dir $(OUTPUTDIR)/$@-info -simdir $(SIMDIR) $(BSCFLAGS) -sim -g top -u $<
#	$(BSC) -simdir $(SIMDIR) $(BSCFLAGS) -sim -e top -o $(OUTPUTDIR)/$@

sim-%: %.bsv
	mkdir -p $(OUTPUTDIR)/$@-info $(BDIR) $(SIMDIR)
	$(BSC) -info-dir $(OUTPUTDIR)/$@-info -simdir $(SIMDIR) $(BSCFLAGS) -sim -g top -u $<
	$(BSC) -simdir $(SIMDIR) $(BSCFLAGS) -sim -e top -o $(OUTPUTDIR)/$@
	$(OUTPUTDIR)/$@ 1> $(OUTPUTDIR)/$@.simlog 2>&1

.PHONY: clean mrproper
clean:
	rm -rf build
mrproper: clean
	rm -rf output
