CXX       = g++
LD        = g++

BIN_DIR   = bin
BUILD_DIR = bin
LIB_DIR   = .
SRC_DIR   = src

CFLAGS    = -Wall -std=c++11
LIBS      = -L$(LIB_DIR)

MODULES   = .
SRC_DIRS   = $(addprefix $(SRC_DIR)/, $(MODULES))
BUILD_DIRS = $(addprefix $(BUILD_DIR)/, $(MODULES))

SRC       = $(foreach sdir, $(SRC_DIRS), $(wildcard $(sdir)/*.cpp))
OBJ       = $(patsubst $(SRC_DIR)/%.cpp, $(BUILD_DIR)/%.o, $(SRC))
DEP       = $(OBJ:%.o=%.d)
INCLUDES  = $(addprefix -I, $(SRC_DIRS)) -Iinclude

TARGET    = au_maybe_test

DEBUG     ?= 0
ifeq ($(DEBUG), 1)
	CFLAGS += -O0 -g -pg -DDEBUG
	LDFLAGS = -O0 -g -pg -DDEBUG
else
	CFLAGS += -O3
	LDFLAGS = -O3
endif

vpath %.cpp $(SRC_DIRS)

define make-goal
$1/%.o: %.cpp
	$(CXX) $(CFLAGS) $(INCLUDES) -MMD -c $$< -o $$@
endef

.PHONY: all checkdirs clean

all: bindir builddirs $(TARGET)

-include $(DEP)

$(TARGET): $(OBJ)
	$(LD) $^ -o $(BIN_DIR)/$@ $(LDFLAGS) $(LIBS)

bindir:
	mkdir -p $(BIN_DIR)

builddirs: $(BUILD_DIRS)

$(BUILD_DIRS):
	mkdir -p $@

clean:
	rm -rf $(BIN_DIR)/$(TARGET)
	rm -rf $(BUILD_DIR)/*

$(foreach bdir, $(BUILD_DIRS), $(eval $(call make-goal, $(bdir))))