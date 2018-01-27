SRC_DIR = src
SRC_MKFILE = $(SRC_DIR)/makefile

all: $(SRC_MKFILE)
	make -C $(SRC_DIR) $@

clean: $(SRC_MKFILE)
	make -C $(SRC_DIR) $@
