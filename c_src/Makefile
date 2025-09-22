# NIF compilation settings
ERLANG_PATH = $(shell erl -eval 'io:format("~s", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])])' -s init stop -noshell)

# Base compilation flags
CFLAGS += -std=c99 -Wall -Wmissing-prototypes -Wextra -Wformat-security
CFLAGS += -fPIC -I $(ERLANG_PATH) -I cmark-gfm -I extensions

# Production flags (default)
ifeq ($(DEBUG),)
	CFLAGS += -O3 -finline-functions -DNDEBUG
endif

# Debug and safety flags (enable with make DEBUG=1)
ifdef DEBUG
	CFLAGS += -O0 -g -DDEBUG
	CFLAGS += -fsanitize=address -fno-omit-frame-pointer -fno-common
	CFLAGS += -fsanitize=undefined -fstack-protector-strong
	LDFLAGS += -fsanitize=address -fsanitize=undefined
endif

ifeq ($(UNAME),Darwin)
	LDFLAGS += -dynamiclib -undefined dynamic_lookup
endif

# Determine platform-specific settings
UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
	LDFLAGS += -dynamiclib -undefined dynamic_lookup
else
	LDFLAGS += -shared
endif

# Source files
CMARK_SOURCES = $(wildcard cmark-gfm/*.c)
EXTENSION_SOURCES = $(wildcard extensions/*.c)
NIF_SOURCES = arizona_markdown.c

# Object files
CMARK_OBJECTS = $(CMARK_SOURCES:.c=.o)
EXTENSION_OBJECTS = $(EXTENSION_SOURCES:.c=.o)
NIF_OBJECTS = $(NIF_SOURCES:.c=.o)

# Target
PRIV_DIR = ../priv
NIF_SO = $(PRIV_DIR)/arizona_markdown.so

# Default target
all: $(NIF_SO)

# Create priv directory if it doesn't exist
$(PRIV_DIR):
	mkdir -p $(PRIV_DIR)

# Compile NIF
$(NIF_SO): $(PRIV_DIR) $(CMARK_OBJECTS) $(EXTENSION_OBJECTS) $(NIF_OBJECTS)
	$(CC) $(CMARK_OBJECTS) $(EXTENSION_OBJECTS) $(NIF_OBJECTS) $(LDFLAGS) -o $(NIF_SO)

# Compile object files
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Clean target
clean:
	rm -f $(CMARK_OBJECTS) $(EXTENSION_OBJECTS) $(NIF_OBJECTS) $(NIF_SO)

.PHONY: all clean