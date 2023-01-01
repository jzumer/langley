all: main

clean:
	rm -f *.o main

main: string.c token.c ast.c rule.c parser.c compiler.c main.c
	gcc $^ -o main

debug: string.c token.c ast.c rule.c parser.c compiler.c main.c
	gcc -DDEBUG -g $^ -o main
