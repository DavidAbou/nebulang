##
## EPITECH PROJECT, 2023
## nebulang [WSL : Ubuntu]
## File description:
## Makefile
##

NAME	:=	nebulang

all: $(NAME)

$(NAME):
	@echo "Building $(NAME):"
	@cargo build
	@install -m 755 target/debug/$(NAME) .
	@echo "\033[0;32m✔ Done.\033[0m"

clean:
	@echo "Cleaning..."
	@cargo clean

fclean: clean
	@echo "Removing $(NAME)..."
	@rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re