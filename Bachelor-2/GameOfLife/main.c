#include <stdlib.h>
#include <string.h>
#include "Logic.h"
#include "GUI.h"

int main(int argc, char* argv[]) {
    int *current = (int *) malloc(rows * cols * sizeof(int)); // bevat de huidige generatie
    int *next = (int *) malloc(rows * cols * sizeof(int));    // zal de volgende generatie bevatten
    int row, col;
    char *filename = "field.txt";
    if (!(strcmp("-f", argv[1]))){                               // case 1: open een file
        read_txt(&row, &col, current, argv[2]); // lees het aantal rijen en kolommen en kopieer de matrix
        filename = argv[2];                                      // verander filename naar de naam van de file die je net geopend hebt,
                                                                 // zodat het dit bestand zal updaten en niet een ander
    }
    else {                                                                     // case 2: aantal rijen en kolommen werden meegegeven
        if (!(strcmp("-w", argv[1])) && !(strcmp("-h", argv[3])) ) {           // kijk of we eerst de rijen of kolommen moeten lezen
            col = atoi(argv[2]);
            row = atoi(argv[4]);
        } else if (!(strcmp("-h", argv[1])) && !(strcmp("-w", argv[3]))) {
            row = atoi(argv[2]);
            col = atoi(argv[4]);
        }
        else {
            printf("ERROR: Wrong arguments (use -w -h)");
            return 1;
        }
        generate_random_grid(row, col, current); //genereer een willekeurige matrix met de gegeven breedte en hoogte
    }
    initialize_window("Game Of Life - Immigration");  //open de window met als titel
    while (1) {
        if (!(user_input())) break;   // als de gebruiker op 'X' druk stop het spel
        else if (user_input() == 1) { // zoniet kijk of de gebruiker het spel heeft gepauzeerd of niet
            draw_window(current, row, col);
            next_generation(current, next, row, col);
            write_txt(row, col, current, filename);
            vector_copy(row, col, next, current);
            SDL_Delay(100);
        }
    }
    free(current); //free alles zowel de matrices als de gui
    free(next);
    free_gui();
    return 0;
}
