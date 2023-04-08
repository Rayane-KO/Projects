OBJ=main.o Logic.o GUI.o
HEADERS=Logic.h GUI.h
CC=gcc
TARGET=game

all: $(OBJ)
	$(CC) main.o Logic.o GUI.o -lSDL2 -o $(TARGET)
     
main.o: main.c $(HEADERS)
	$(CC) main.c -c -o main.o
        
Logic.o: Logic.c $(HEADERS)
	$(CC) Logic.c -c -o Logic.o
         
GUI.o: GUI.c $(HEADERS)
	$(CC) GUI.c -c -o GUI.o
