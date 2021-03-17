##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## Makefile to build project
##

CC		= 	stack
EXEC	= 	funEvalExpr

all: $(EXEC)

$(EXEC):
	$(CC) build --work-dir ./.stack-work  --allow-different-user
	$(CC) install --local-bin-path .
	mv funEvalExpr-exe ./$(EXEC)

clean:
	$(CC) clean
	rm -f funEvalExpr.cabal

tests_run:
	$(CC) test --coverage

fclean: clean
	rm -f $(EXEC)
	$(CC) purge

re:		fclean all

.PHONY: 	all clean fclean re