#.PHONY: all build

NAME=cl-scripts
BINARY=$(HOME)/bin/$(NAME)
.PHONY: all $(NAME) clean

all: $(NAME)

$(NAME):
	cl-launch --output $(NAME) --dump ! --lisp sbcl --quicklisp --dispatch-system $(NAME)/touchpad --system $(NAME)

install: $(NAME)
	cp -p $(NAME) $(BINARY)
	$(BINARY) create-symlinks

clean:
	rm -f $(NAME)

