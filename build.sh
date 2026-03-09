#!/bin/sh
# ------------------------------------------------------------------
# build.sh - Build and install the Andy compiler 
# ------------------------------------------------------------------
# Features:
# - Checks that sbcl is installed
# - Checks that pkg-config is installed
# - Checks and optionally builds andy_runtime
# - Saves SBCL executable
# - Installs to $PREFIX/bin (default /usr/local/bin)
# ------------------------------------------------------------------

# Exit on errors and treat unset variables as error
set -eu

# -----------------------
# Configurable variables
# -----------------------
SBCL_EXEC=${SBCL_EXEC:-sbcl}              # SBCL executable
PROJECT_NAME="andy"                       # Quickload system name
MAIN_FUNCTION="andy.main:main"            # Toplevel entry point
EXE_NAME="andyc"                          # Output Lisp executable
RUNTIME_DIR="runtime/c"                   # Directory of andy_runtime source
PREFIX=${PREFIX:-/usr/local}              # Installation prefix
INSTALL_DIR="$PREFIX/bin"                 # Install location

# -----------------------
# 0. Check for sbcl
# -----------------------
if ! command -v "$SBCL_EXEC" >/dev/null 2>&1; then
    echo "Error: SBCL not found. Please install SBCL first."
    exit 1
fi

# -----------------------
# 1. Check for pkg-config
# -----------------------
if ! command -v pkg-config >/dev/null 2>&1; then
    echo "Error: pkg-config is required but not installed."
    exit 1
fi

# -----------------------
# 2. Check for andy_runtime
# -----------------------
if ! pkg-config --exists andy_runtime; then
    echo "andy_runtime not found via pkg-config."

    if [ -f "$RUNTIME_DIR/makefile" ]; then
        echo "Attempting to build andy_runtime from $RUNTIME_DIR..."
        (cd "$RUNTIME_DIR" && make)

        echo "andy_runtime built. You may need to install it system-wide:"
        echo "  Example: sudo make install"
        echo "Continuing build of Andy compiler..."
    else
        echo "No Makefile found at $RUNTIME_DIR. Please install andy_runtime manually."
        exit 1
    fi
else
    echo "Found andy_runtime via pkg-config."
fi

# -----------------------
# 3. Build the SBCL Lisp executable
# -----------------------
echo "Building SBCL standalone executable: $EXE_NAME"

"$SBCL_EXEC" --no-sysinit \
	     --eval "(load \"$HOME/quicklisp/setup.lisp\")" \
	     --eval "(ql:quickload :$PROJECT_NAME)" \
             --eval "(sb-ext:save-lisp-and-die \"$EXE_NAME\" :toplevel #'$MAIN_FUNCTION :executable t)" \
          --eval "(quit)"

# -----------------------
# 4. Install the executable
# -----------------------
echo "Trying to Install $EXE_NAME to $INSTALL_DIR"
mkdir -p "$INSTALL_DIR"
if [ ! -w "$INSTALL_DIR" ]; then
    echo "Cannot write to $INSTALL_DIR."
    echo "use sudo install -m 755 $EXE_NAME $INSTALL_DIR to install"
    exit 1
else
    install -m 755 "$EXE_NAME" "$INSTALL_DIR"
fi

echo "Build and install complete: $INSTALL_DIR/$EXE_NAME"
echo "Tip: You can override PREFIX by running: PREFIX=\$HOME/.local ./build.sh"
