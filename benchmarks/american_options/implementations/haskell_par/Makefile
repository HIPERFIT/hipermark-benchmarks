# Permit standalone compilation.
HIPERMARK_LIB_DIR ?= ../../../../lib/

include $(HIPERMARK_LIB_DIR)/setup.mk

EXECUTABLE = AmrPutPar
SOURCES = AmrPutPar.hs

GHC_FLAGS = -O3 -Wall -i$(HIPERMARK_IMPLEMENTATION_DIR) -threaded

default: $(EXECUTABLE)

$(EXECUTABLE): $(HIPERMARK_IMPLEMENTATION_DIR)/$(SOURCES)
	ghc -main-is $@ $(GHC_FLAGS) -o $@ $<

clean:
	rm -rf $(EXECUTABLE) *o

HIPERMARK_DATA_FIELDS=years
HIPERMARK_RUN_ENVIRONMENT=
include $(HIPERMARK_LIB_DIR)/flat_data_helper.mk
