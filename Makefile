##
## EPITECH PROJECT, 2024
## Groundhog
## File description:
## Makefile
##

NAME	=	groundhog
PROJECT		=	Groundhog

all:
	@TAR_OPTIONS=--no-same-owner stack setup
	@stack build
	@cp $(shell stack path --local-install-root)/bin/$(PROJECT)-exe $(NAME)

tests_run:
	stack build --test --coverage

clean:

fclean: clean
	@rm -rf $(NAME)

re: fclean all

# weewoo was here
