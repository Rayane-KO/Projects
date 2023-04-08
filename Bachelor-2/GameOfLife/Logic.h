//
// Created by rayane on 31/10/22.
//

#ifndef GAMEOFLIFE_LOGIC_H
#define GAMEOFLIFE_LOGIC_H
#define rows 80
#define cols 80
#define group_1 1
#define group_2 2
#define dead 0
int generate_random_grid(int r, int c, int grid[]);
int count_living_cells(int m[], int cel_row, int cel_col, int r, int c, int *cells_A, int *cells_B);
int next_generation(int matrix[], int next[], int r, int c);
void vector_copy(int r, int c, int v1[], int v2[]);
void write_txt(int r, int c, int m[], char filename[]);
void read_txt(int *r, int *c, int m[], char filename[]);
#endif //GAMEOFLIFE_LOGIC_H
