# Permit standalone compilation.
HIPERMARK_LIB_DIR ?= ../../../../lib/

include $(HIPERMARK_LIB_DIR)/setup.mk

EXECUTABLE = amr_put
SOURCES = amr_put.c
CC=gcc

CFLAGS = -O3 -Wall -Wextra -pedantic -I$(HIPERMARK_IMPLEMENTATION_DIR) -std=c99 -lm

default: $(EXECUTABLE)

$(EXECUTABLE): $(HIPERMARK_IMPLEMENTATION_DIR)/$(SOURCES)
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -rf $(EXECUTABLE) *.o

HIPERMARK_DATA_FIELDS=years
HIPERMARK_RUN_ENVIRONMENT=
include $(HIPERMARK_LIB_DIR)/flat_data_helper.mk
