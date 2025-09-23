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

# Code quality targets
format-check:
	@echo "Checking C code formatting..."
	@npx clang-format --dry-run --Werror $(NIF_SOURCES) || \
		(echo "❌ Code formatting issues found. Run 'make format' to fix."; exit 1)
	@echo "✅ C code formatting is correct"

format:
	@echo "Formatting C code..."
	@npx clang-format -i $(NIF_SOURCES)
	@echo "✅ C code formatted"

lint:
	@echo "Running C code linter..."
	@if command -v clang-tidy >/dev/null 2>&1; then \
		clang-tidy $(NIF_SOURCES) -- $(CFLAGS) || \
		(echo "❌ Linting issues found. Please fix the warnings above."; exit 1); \
	else \
		echo "⚠️  clang-tidy not found, skipping C linting"; \
	fi
	@echo "✅ C code linting completed"

check: format-check lint
	@echo "✅ All C code quality checks passed"

.PHONY: all clean format-check format lint check