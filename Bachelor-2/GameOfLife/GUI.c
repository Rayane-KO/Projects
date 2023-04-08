#include "GUI.h"
#include "Logic.h"
#include <stdlib.h>

/*
 * De renderer wordt gebruikt om figuren of sprites te tekenen in het venster.
 * De renderer wordt geïnitialiseerd in de initialize_window-functie.
 */
static SDL_Renderer *renderer;

/*
 * Dit is het venster dat getoond zal worden en waarin het speelveld wordt weergegeven.
 * Dit venster wordt aangemaakt bij het initialiseren van de GUI en wordt weer afgebroken
 * wanneer het spel ten einde komt.
 */
static SDL_Window *window;
int should_continue = 1;
/*
 * Onderstaande twee lijnen maken deel uit van de minimalistische voorbeeldapplicatie:
 * ze houden de laatste positie bij waar de gebruiker geklikt heeft.
 */
int mouse_x = 0;
int mouse_y = 0;

/*
 * Geeft aan of de applicatie moet verdergaan.
 * Dit is waar zolang de gebruiker de applicatie niet wilt afsluiten door op het kruisje te klikken.
 */


/*
 * SDL genereert heel veel gebruikersevents terwijl de applicatie wordt uitgevoerd,
 * maar veel van die events zijn helemaal niet belangrijk voor de rest van de applicatie.
 * Deze functie filtert de belangrijke van de niet-belangrijke events, zodat het behandelen
 * van de niet-belangrijke events de applicatie niet vertraagt.
 * 
 * Controleert of het gegeven SDL event van belang is voor deze applicatie of niet.
 * Geeft een 1 terug als het gegeven event een muisklik-, een keypress,
 * of een quit-event (gebruiker sluit het venster af) is. Geeft een 0 terug als de
 * gegeven pointer NULL is of als het event waarnaar verwezen wordt niet belangrijk
 * is voor de applicatie.
 */
int is_relevant_event(SDL_Event *event) {
	if (event == NULL) {
		return 0;
	}
	return  (event->type == SDL_MOUSEBUTTONDOWN) ||
			(event->type == SDL_KEYDOWN) ||
			(event->type == SDL_QUIT);
}
/*
 * Vangt de input uit de GUI op. Deze functie is al deels geïmplementeerd, maar je moet die zelf
 * nog afwerken. Je mag natuurlijk alles aanpassen aan deze functie, inclusief return-type en argumenten.
 */
int user_input() {
	SDL_Event event;

	while (SDL_PollEvent(&event) || is_relevant_event(&event)) {
        switch (event.type) {
            case SDL_QUIT:
                /* De gebruiker heeft op het kruisje van het venster geklikt om de applicatie te stoppen. */
                should_continue = 0;
                break;

            case SDL_KEYDOWN:
                /* De gebruiker heeft een toets ingedrukt (hiermee wordt de simulatie gepauzeerd). */
                should_continue = -should_continue;
                break;
        }
    }
    return should_continue;
}

void clear_window() {
	/*
	 * Maakt van wit de achtergrondkleur.
	 */
	SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);
	/*
	 * Reset het venster en verandert het naar de kleur die op de lijn hierboven werd vastgelegd.
	 */
	SDL_RenderClear(renderer);
}

void draw_rectangle(int x, int y, int width, int height) {
	/*
	 * DPI_SCALING is ofwel 1, ofwel 2, afhankelijk van je platform: 2 voor moderne OSX systemen en 1 voor alle andere platformen.
	 * Op moderne OSX systemen moet voor Retina displays expliciet de dimensies van getekende figuren verdubbeld worden,
	 * aangezien die een hogere dpi hebben: figuren worden half zo klein voorgesteld zodat ze aan een hogere resolutie
	 * kunnen getoond worden. Daarom verdubbelen we de positie en dimensies van de figuren zodat ze hun oorspronkelijke
	 * positie en dimensie behouden.
	 */
	SDL_Rect rectangle = { x * DPI_SCALING, y * DPI_SCALING, width * DPI_SCALING, height * DPI_SCALING};
	/* Vraagt de renderer om rectangle te tekenen, in de kleur die werd vastgelegd via SDL_SetRenderDrawColor. */
	SDL_RenderFillRect(renderer, &rectangle);
}

void draw_window(int m[], int r, int c) { //zal de gegeven matrix tekenen
        clear_window();                   //eerst wissen we alles wat er stond
        int offset;
        for (int i = 0; i < r; i++) {
            for (int j = 0; j < c; j++) {
                offset = i*r+j;
                if (m[offset] == group_2) SDL_SetRenderDrawColor(renderer, 255, 100, 0, 255);      //groep 2 is oranje
                else if (m[offset] == group_1) SDL_SetRenderDrawColor(renderer, 0, 100, 255, 255); //groep 1 is blauw
                else SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);                         //dood is gewoon wit
                draw_rectangle(j * WINDOW_WIDTH / c, i * WINDOW_HEIGHT / r, WINDOW_WIDTH / c,WINDOW_HEIGHT / r);
            }
        }
        SDL_RenderPresent(renderer);
    }

/*
 * Initialiseert het venster en alle andere structuren die gebruikt worden door de GUI.
 */
void initialize_window(const char *title) {
	/*
	 * Code o.a. gebaseerd op http://lazyfoo.net/tutorials/SDL/02_getting_an_image_on_the_screen/index.php
	 */
	if (SDL_Init(SDL_INIT_VIDEO) < 0) {
		printf("Error while initializing SDL: %s\n", SDL_GetError());
		exit(1);
	}

	/*
	 * Maakt het venster aan met gevraagde dimensies.
	 * De titel van het venster zal overeenkomen met de waarde van de title-string.
	 */
	window = SDL_CreateWindow(title, 0, 0, WINDOW_WIDTH, WINDOW_HEIGHT, SDL_WINDOW_SHOWN | SDL_WINDOW_ALLOW_HIGHDPI);
	if (window == NULL) {
		printf("Error: %s\n", SDL_GetError());
		exit(1);
	}

	/* Initialiseert de renderer. */
	renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC);
}

/*
 * Dealloceert alle SDL structuren die dynamisch gealloceerd werden.
 */
void free_gui() {
	/* Dealloceert het venster. */
	SDL_DestroyWindow(window);
	/* Dealloceert de renderer. */
	SDL_DestroyRenderer(renderer);
	/* Stopt SDL. */
	SDL_Quit();
}