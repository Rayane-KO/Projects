//Logic of Game Of Life (Immigration) made by Rayane Kouidane

#include "Logic.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

/*vector_copy neemt als argument 2 getallen en 2 arrays van arrays (vectoren)
 * en zal v1 kopieeren in v2 */
void vector_copy(int r, int c, int v1[], int v2[]) {
    int offset;
    for (int i = 0; i < r; i++) {
        for (int j = 0; j < c; j++) {
            offset = i*r+j;
            v2[offset] = v1[offset];
        }
    }
}
//genereert een willekeurig cijfer (0, 1, 2)
//geen rand%3 want ik wil meer dode cellen dan levenden
char random_group(){
    int r = rand() % 100;
    if (r < 40) return dead;
    else if (r < 70) return group_1;
    else return group_2;
}
//zal volgens de gegeven rijen en kolommen een willekeurige matrix genereren
//met cijfers 0, 1 en 2
int generate_random_grid(int r, int c, int grid[]){
    int offset;
    for(int i=0; i<r; i++){
        for(int j=0;j<c;j++){
            offset = i*r+j;
            grid[offset] = random_group();
        }
    }
}
//telt het aantal levende cellen rond een bepaalde cel
int count_living_cells(int m[], int cel_row, int cel_col, int r, int c, int *cells_A, int *cells_B){
    int i, j, ctr_A = 0, ctr_B = 0, offset;
    for (i = cel_row - 1; i <= cel_row + 1; i++){
        for (j = cel_col - 1; j <= cel_col + 1; j++){
            if (i == cel_row && j == cel_col) continue;          //de cel waarvan we de buren tellen moeten we niet in rekening brengen
            else if ((i >= 0 && j >= 0) && (i < r && j < c)) {   //als de posities niet buiten de grenzen zijn dat mag je ze meetellen
                offset = i * r + j;
                if (m[offset] == group_1) ctr_A++;
                else if (m[offset] == group_2) ctr_B++;
            }
        }
    }
    //voor elke cel zullen we het aantal omringende cellen van een bepaalde cel terugvinden in
    //de pointers
    *cells_A = ctr_A;
    *cells_B = ctr_B;
    return ctr_A + ctr_B; //geeft het aantal levende cellen terug
}
//gaat volgens de regels van Immigration de volgende generatie berekenen en deze in next plaatsen
int next_generation(int matrix[], int next[], int r, int c) {
    int B_cells;
    int A_cells;
    int offset;
    for (int i = 0; i < r; i++) {
        for (int j = 0; j < c; j++) {
            int living_cells = count_living_cells(matrix, i, j, r, c, &A_cells, &B_cells);
            offset = i * r + j;
            if ((matrix[offset] == dead) && (living_cells == 3)) {
                if (A_cells < B_cells) next[offset] = group_2;
                else next[offset] = group_1;
            } else if ((matrix[offset] != dead) && ((living_cells == 2) || (living_cells == 3))) {
                if (matrix[offset] == group_1) next[offset] = group_1;
                else next[offset] = group_2;
            } else if ((matrix[offset] != dead) && ((living_cells < 2) || (living_cells > 3))) {
                next[offset] = dead;
            } else next[offset] = dead;
        }
    }
}
//deze functie zorgt ervoor dat we het huidige speelveld kunnen opslaan in een txt bestand
void write_txt(int r, int c, int m[], char filename[]){
    FILE *fptr = fopen(filename, "w");      //open de file
    fprintf(fptr, "%i\n%i\n", r, c); //zet eerst het aantal rijen en kolommen in de file
    int offset;
    for(int i=0; i<r; i++){                        //kopieer daarna de matrix in de file
        for(int j=0; j<c; j++){
            offset = i*r+j;
            fprintf(fptr, "%i", m[offset]);
        }
        fprintf(fptr, "\n");
    }
    fclose(fptr);                          //sluit de file
}
//hiermee zullen we een txt bestand kunnen inlezen
void read_txt(int *r, int *c, int m[], char filename[]){
    FILE *fptr = fopen(filename, "r");
    if (fptr == NULL){                      //wanneer je een file opent die niet bestaat dan wordt een nieuwe file aangemaakt
        srand(time(0));          //met een willekeurig aantal rijen en kolommen (de start-configuratie is ook willekeurig)
        int size = rand()%rows;
        *r = size;
        *c = size;
        printf("New file created named '%s' with random grid of size %ix%i.\n", filename, *r, *c);
        generate_random_grid(*r, *c, m);
    }
    else {
        int rowz, colz, offset;
        char el;
        fscanf(fptr, "%i", &rowz);  //lees het aantal rijen
        fscanf(fptr, "%i", &colz);  //lees het aantal kolommen
        *r = rowz;
        *c = colz;
        for (int i = 0; i < rowz; i++) {               //lees de matrix en plaats deze in m
            for (int j = 0; j < colz; j++) {
                offset = i * rowz + j;
                fscanf(fptr, " %c", &el);
                m[offset] = el - '0';
            }
        }
        fclose(fptr);                     //sluit de file
    }
}