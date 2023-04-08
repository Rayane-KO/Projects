#ifndef GUI_H_
#define GUI_H_

#include <stdio.h>
#include <stdlib.h>
#include "Logic.h"

/*
 * Importeert de nodige functies uit SDL2.
 */
#include <SDL2/SDL.h>
//int draw_game(int m[6][6]);
void draw_window(int m[], int r, int c);
void initialize_window(const char *title);
void free_gui();
int user_input();
int is_relevant_event(SDL_Event *event);

/*
 * Op moderne OSX systemen moet voor Retina displays expliciet de dimensies van getekende figuren verdubbeld worden,
 * aangezien die een hogere dpi hebben.
 * Op andere besturingssystemen moet dat niet gebeuren.
 */
#ifdef __MAC_10_15
	#define DPI_SCALING 2
#else
	#define DPI_SCALING 1
#endif

/*
 * De hoogte en breedte van het venster (in pixels).
 * Deze dimensies zijn arbitrair gekozen. Deze dimensies hangen mogelijk af van de grootte van het speelveld.
 */

#define WINDOW_HEIGHT 600
#define WINDOW_WIDTH 600

#endif /* GUI_H_ */
