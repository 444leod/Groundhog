##
## EPITECH PROJECT, 2024
## Groundhog
## File description:
## Makefile
##

NAME	=	groundhog
PROJECT		=	Groundhog

all:
	@stack build
	@cp $(shell stack path --local-install-root)/bin/$(PROJECT)-exe $(NAME)

tests_run:
	stack build --test --coverage

clean:

fclean: clean
	@rm -rf $(NAME)

re: fclean all

# weewoo was here
