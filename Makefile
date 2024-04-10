##
## EPITECH PROJECT, 2024
## Groundhog
## File description:
## Makefile
##

NAME	=	groundhog
PROJECT		=	Groundhog

all:
	@stack build --allow-different-user
	@cp $(shell stack path --local-install-root)/bin/$(PROJECT)-exe $(NAME)

clean:

fclean: clean
	@rm -rf $(NAME)

re: fclean all

# weewoo was here
