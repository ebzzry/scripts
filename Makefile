NAME=cl-scripts
BINARY=$(HOME)/bin/$(NAME)
SCRIPT=$(PWD)/$(NAME)

.PHONY: all $(NAME) clean

all: $(NAME)

$(NAME):
	cl-launch --output $(NAME) --dump ! --lisp sbcl --quicklisp --dispatch-system $(NAME)/touchpad --system $(NAME)

install: $(NAME)
	ln -sf $(SCRIPT) $(BINARY)
	$(SCRIPT) create-symlinks

clean:
	rm -f $(NAME)

