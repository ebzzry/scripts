.PHONY: all cl-scripts symlinks

BINARY=$$HOME/bin/cl-scripts

cl-scripts:
	cl-launch --output $(BINARY) --dump ! --lisp sbcl --quicklisp --dispatch-system cl-scripts/touchpad --system cl-scripts

symlinks: cl-scripts
	$(BINARY) create-symlinks

all: cl-scripts symlinks
