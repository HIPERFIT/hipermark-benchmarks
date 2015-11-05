# Permit standalone compilation.
HIPERMARK_LIB_DIR ?= ../../../../lib/

include $(HIPERMARK_LIB_DIR)/setup.mk

EXECUTABLE = amr_put
SOURCES = amr_put.fut

FUTFLAGS = --unsafe --real-as-single

default: $(EXECUTABLE)

amr_put.bin: $(HIPERMARK_IMPLEMENTATION_DIR)/amr_put.fut
	futhark-c $(FUTFLAGS) -o $@ $<

$(EXECUTABLE): $(EXECUTABLE).bin
	echo '#!/bin/sh' > $@
	echo './'$@'.bin -t $$HIPERMARK_RUNTIME > $$HIPERMARK_RESULT' >> $@
	chmod +x $@

clean:
	rm -rf $(EXECUTABLE) *.o

HIPERMARK_DATA_FIELDS=years
HIPERMARK_RUN_ENVIRONMENT=
include $(HIPERMARK_LIB_DIR)/flat_data_helper.mk
