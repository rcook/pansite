ifndef OUTPUT_DIR
$(error OUTPUT_DIR is not set)
endif

.PHONY: all
all: ${OUTPUT_DIR}/ctp.html ${OUTPUT_DIR}/pcph.html

${OUTPUT_DIR}/%.html: %.md
	pandoc -o $@ $<
