#include <M5StickCPlus.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <time.h>
#include "EEPROM.h"
#include "constants.h"

void draw_explosion(int, int);

int calculate_width(int columns){
   return SCRWIDTH / columns;
}
int calculate_height(int rows){
   return SCRHEIGHT / rows;
}

// check_configuration will check in which mode we are and add the correct elements (like bombs and virus)
void check_configuration(){
  if (timed){
    virus_timer = rand()%11+10; // timer is between 10 and 20
    timed_ctr = 1;
    virus_timed = true;
  }
  if (bomb) bomb_timer = rand()%11+5; // timer is between 5 and 15
}

/*
  STRUCT CELL
*/
typedef struct Cell{
  uint8_t row;
  uint8_t col;
  uint8_t color;      // 0 means that the cell is empty
  uint8_t group = 0; // pills will have the same group number
  uint8_t type;       // pill:1  virus:2  bomb:3  timed:4

  void add_cell(Cell** &grid){
    grid[row][col] = *this;
  }

  void delete_cell(){
    color = 0;
    group = 0;
  }

  bool check_end(Cell** &grid){
    return (row == rows-1 || grid[row+1][col].color != 0);
  }

  bool check_empty(Cell** grid, uint8_t row, uint8_t col){
    return (grid[row][col].color == 0);
  }

  void move_cell(Cell** &grid, uint8_t new_row, uint8_t new_col){
    if (new_row < rows && new_row >= 0 && new_col < cols && new_col >= 0 && check_empty(grid, new_row, new_col)){
      grid[row][col].delete_cell();
      row = new_row;
      col = new_col;
      grid[row][col] = *this;
    }
  }

  Cell() : row(0), col(0), color(0) {}

  Cell(uint8_t cell_row, uint8_t cell_col, uint8_t cell_color) : row(cell_row), col(cell_col), color(cell_color) {}
} Cell;

Cell empty_cell(uint8_t row, uint8_t col){
  return Cell(row, col, 0);
}

// grid is the playfield
Cell** grid;

// init_grid will dynamically allocate a 2d array of size rows*cols
void init_grid(){
  grid = new Cell*[rows];
  for(int i=0; i<rows; i++){
    grid[i] = new Cell[cols];
  }
}

// empty_grid will place an empty cell in every location
void empty_grid(){
  for(int i=0; i<rows; i++){
    for(int j=0; j<cols; j++){
      grid[i][j] = empty_cell(i, j);
    }
  }
}

// free_grid will deallocate the 2d array
void free_grid(){
  for(int i=0; i<rows; i++){
    delete[] grid[i];
  }
  delete[] grid;
}

//will check if there is a pair around the cell on the position cell_row, cell_col
bool check_for_pair(uint8_t cell_row, uint8_t cell_col){
  Cell &cell = grid[cell_row][cell_col];
  bool check = false;
  Cell pair1, pair2, pair3, pair4;
  if (cell_col+1 < cols) pair1 = grid[cell_row][cell_col+1];
  if (cell_col-1 >= 0) pair2 = grid[cell_row][cell_col-1];
  if (cell_row+1 < rows) pair3 = grid[cell_row+1][cell_col];
  if (cell_row-1 >= rows) pair4 = grid[cell_row-1][cell_col];
  Cell *check_pairs{ new Cell[4] {pair1, pair2, pair3, pair4}};
  for(int i=0; i<4; i++){
    if (((cell.group == check_pairs[i].group) && cell.color != 0 && check_pairs[i].color != 0)){
      check = true;
      break;
    }
  }
  delete[] check_pairs;
  return check;
}

// fall will check if there are cells that can be moved down and it will move them if it's possible
void fall(Cell** &grid, uint8_t rows, uint8_t cols){
  for (int i = 0; i < rows-1; i++){
    for (int j = 0; j < cols; j++){
      Cell &cell = grid[i][j];
      int pos = 0;
      if (cell.type == PILL){
      if (j+1 < cols){
        Cell &pair = grid[i][j+1];
        if ((cell.group == pair.group) && cell.color != 0 && pair.color != 0){
          if ((!(pair.check_end(grid))) && (!(cell.check_end(grid))) && pair.color != 0){
            cell.row++;
            pair.row++;
            grid[pair.row][pair.col] = pair;
            grid[cell.row][cell.col] = cell;
            grid[cell.row-1][cell.col].delete_cell();
            grid[pair.row-1][pair.col].delete_cell();
          }
        }
      }
      if (j-1 >= 0){
        Cell &pair = grid[i][j-1];
        if (cell.group == pair.group && cell.color != 0 && pair.color != 0){
          if ((!(pair.check_end(grid))) && (!(cell.check_end(grid)))){
            cell.row++;
            pair.row++;
            grid[pair.row][pair.col] = pair;
            grid[cell.row][cell.col] = cell;
            grid[cell.row-1][cell.col].delete_cell();
            grid[pair.row-1][pair.col].delete_cell();
          }
        }
      }
      if (i+1 < rows){
        Cell &pair = grid[i+1][j];
        if (cell.group == pair.group && cell.color != 0 && pair.color != 0){
          if ((!(pair.check_end(grid)))){
            cell.row++;
            pair.row++;
            grid[pair.row][pair.col] = pair;
            grid[cell.row][cell.col] = cell;
            grid[cell.row-1][cell.col].delete_cell();
          }
        }
      }
      if (i-1 >= 0){
      Cell &pair = grid[i-1][j];
        if (cell.group == pair.group && cell.color != 0 && pair.color != 0){
          if ((!(cell.check_end(grid)))){
            cell.row++;
            pair.row++;
            grid[pair.row][pair.col] = pair;
            grid[cell.row][cell.col] = cell;
            grid[pair.row-1][pair.col].delete_cell();
          }
        }
      }
      if (cell.group != grid[i+1][j].group && cell.group != grid[i-1][j].group && cell.group != grid[i][j+1].group && cell.group != grid[i][j-1].group){
        if(cell.type == PILL){
          if ((!(cell.check_end(grid))) && cell.color != 0){
            cell.row++;
            grid[cell.row][cell.col] = cell;
            grid[cell.row-1][cell.col].delete_cell();
          }
        }
      }
    }
  }
}
}
  
/*
  STRUCT PILL
*/
typedef struct Pill{
  Cell left = Cell(0, cols/2, rand()%3+1);
  Cell right = Cell(left.row, left.col+1, rand()%3+1);

  uint8_t rot = 0; // this represents the rotation of the pill

  void init(){ //will initialize the pill or a bomb
    if (bomb_timer == 0){
      left.type = BOMB;
      left.group = 9;
      left.color = 9;
      right.delete_cell();
      bomb_timer = rand()%11+5;
    } else {
      left.type = PILL;
      right.type = PILL;
      if (group_ctr == 0 || group_ctr == 9) group_ctr++; // so that the group number can never be 0 or 9
      left.group = group_ctr;
      right.group = group_ctr;
      group_ctr++;
      if (bomb) bomb_timer--; 
    }
  }

  void add_pill(){
    if (virus_timed) virus_timer--; // time of timed virus decrements each time a new pill falls
    if (left.type == PILL){
      left.add_cell(grid);
      right.add_cell(grid);
    } else {
      left.add_cell(grid);
    }
  }

  void explode(Cell** &grid){ //will delete the eight cells around the current and the bomb
    int width = calculate_width(cols);
    int height = calculate_height(rows);
    for(int i=left.row-1; i<= left.row+1; i++){
      for(int j=left.col-1; j<= left.col+1; j++){
        if (i>=0 && i<rows && j>=0 && j<cols){
          if (grid[i][j].type == VIRUS || grid[i][j].type == TIMED){
             score += 1;
             virus_ctr--;
             if (grid[i][j].type == TIMED) virus_timed = false; // if timed virus exploded, stop the timer
             if (!(check_for_pair(i, j))) score += 2; // bonuspoints when both parts of the pill are eliminated
          }
          grid[i][j].delete_cell();
          draw_explosion(i, j); //explosion effect
        }
      }
    }
  }

  bool check_end(){ // checks if the pill is at the end (depending on it's rotation)
    if (rot == 1) return right.check_end(grid);
    else if (rot == 3) return left.check_end(grid);
    else return (left.check_end(grid) || right.check_end(grid));
  }

  bool check_left_end(){
    return (grid[left.row+1][left.col-1].color == 0 && grid[right.row+1][right.col-1].color == 0);
  }

  bool check_right_end(){
    return (grid[left.row+1][left.col+1].color == 0 && grid[right.row+1][right.col+1].color == 0);
  }

  void move_pill(float acc_x, float acc_y){
        if (acc_x > MIN_TILT){ // depending on it's orientation it should check and move differently
          if (rot == 0){
            if (grid[left.row][left.col-1].color == 0 && check_left_end()){
              left.move_cell(grid, left.row , left.col-1);
              right.move_cell(grid, right.row, right.col-1);
            }
          }
          else if (rot == 2){
            if (grid[right.row][right.col-1].color == 0 && check_left_end()){
            right.move_cell(grid, right.row, right.col-1);
            left.move_cell(grid, left.row , left.col-1);
            }
          }
           else {
            if (grid[left.row][left.col-1].color == 0 && grid[right.row][right.col-1].color == 0 && check_left_end()){
              left.move_cell(grid, left.row , left.col-1);
              right.move_cell(grid, right.row, right.col-1);
            }
          }
        }
        else if (acc_x < -MIN_TILT){
          if (rot == 0){
            if (grid[right.row][right.col+1].color == 0 && check_right_end()){
              right.move_cell(grid, right.row, right.col+1);
              left.move_cell(grid, left.row , left.col+1);
            }
          }
          else if (rot == 2){
            if (grid[left.row][left.col+1].color == 0 && check_right_end()){
              left.move_cell(grid, left.row , left.col+1);
              right.move_cell(grid, right.row, right.col+1);
            }
          } else {
            if (grid[left.row][left.col+1].color == 0 && grid[right.row][right.col+1].color == 0 && check_right_end()){
              left.move_cell(grid, left.row , left.col+1);
              right.move_cell(grid, right.row, right.col+1);
            }
          }
        }
      if (acc_y > MIN_TILT) speed = max(MIN_SPEED, speed-50*level_ctr);
      if (acc_y < -MIN_TILT) speed = min(MAX_SPEED, speed+50*level_ctr);
      if (rot == 1){
        right.move_cell(grid, right.row + 1, right.col);
        left.move_cell(grid, left.row + 1, left.col);
      } else{
        left.move_cell(grid, left.row + 1, left.col);
        if (left.type == PILL) right.move_cell(grid, right.row + 1, right.col);
      }
  }

  void rotate_pill(){
    if (left.type == PILL){ // you can only rotate if it's a pill (not a bomb)
    uint8_t row = right.row, col = right.col;
    switch (rot){
      case 0:
        right.move_cell(grid, row+1, col-1);
        break;
      case 1:
        right.move_cell(grid, row-1, col-1);
        break;
      case 2:
        right.move_cell(grid, row-1, col+1);
        break;
      case 3:
        right.move_cell(grid, row+1, col+1);
        break;
    }
    rot = (rot+1)%4;
    }
  }
} Pill;

/*
  STRUCT VIRUS
*/
typedef struct Virus{
  Cell virus = Cell(0, 0, rand()%3+1);

  void add_virus(){
    if (virus_timed && timed_ctr != 0){
      virus.type = TIMED;
      virus.add_cell(grid);
      timed_ctr--;
    } else{   
    virus.type = VIRUS;
    virus.add_cell(grid);
    }
  }
} Virus;

//place the virus on the grid (at random positions)
void init_virus(Cell** &grid){
  uint8_t counter = virus_ctr;
  for(int i=rows/2; i<rows; i++){
    for(int j=0; j<cols; j++){
        if (rand()%2 == 0 && counter != 0){
          Virus new_virus;
          new_virus.virus.row = i;
          new_virus.virus.col = j;
          new_virus.add_virus();
          counter--;
        }
    }
  }
}

void check_cells(Cell** &grid, uint8_t row, uint8_t col, bool row_col){ // checks if it can eliminate a row or a column of 4 or more cells/virus of the same color
  int left_count = 0, right_count = 0, i, pos = 0, end, cell_color, cell_type;
  bool check_pair;
  if (row_col){
    i = row - 1;
    end = rows;
  } else {
    i = col - 1;
    end = cols;
  }
  int *array = new int[end];
  for(int j=0; j<end; j++){
    array[j] = -1;
  } 
  if ((grid[row][col]).color != 0){
    while (i >= 0){
      if (row_col) cell_color = (grid[i][col]).color;
      else cell_color = (grid[row][i]).color;
      if (cell_color == (grid[row][col]).color){
        left_count++;
        array[pos] = i;
        pos++;
        i--;
      }
      else break;
    }
    if (row_col) i = row + 1;
    else i = col + 1;
    while (i < end){
      if (row_col) cell_color = (grid[i][col]).color;
      else cell_color = (grid[row][i]).color;
      if (cell_color == (grid[row][col]).color){
        right_count++;
        array[pos] = i;
        pos++;
        i++;
      }
      else break;
    }
    if (left_count + right_count + 1 >= 4){
      for (int j = 0; j < end; j++){
        if (array[j] != -1){
          if (row_col) cell_type = (grid[array[j]][col]).type;
          else cell_type = (grid[row][array[j]]).type;
          if (cell_type == VIRUS){
             score += 1;
             virus_ctr--;
          }
          else if (cell_type == TIMED){
            virus_timed = false;
            score += 1;
            virus_ctr--;
          } else {
            if (row_col) check_pair = (check_for_pair(array[j], col));
            else check_pair = (check_for_pair(row, array[j]));
            if (!check_pair){
              score += 2;
            }
          }
          if (row_col) (grid[array[j]][col]).delete_cell();
          else (grid[row][array[j]]).delete_cell();
        }
      }
      cell_type = grid[row][col].type;
        if (cell_type == VIRUS){
          score += 1;
          virus_ctr--;
        }
        else if (cell_type == TIMED){
          virus_timed = false;
          score += 1;
          virus_ctr--;
        }
        check_pair = (check_for_pair(row, col));
        if (!check_pair) score += 2;
        grid[row][col].delete_cell();
    }
  }
  delete[] array;
}

bool check_win(){
  return (virus_ctr <= 0);
}

bool check_lost(Pill pill){
  return ((virus_timed && virus_timer == 0) || ((pill.left.row == 1 || pill.right.row == 1) && pill.check_end()));
}
uint8_t encode_config(){ // will encode the current configuration (bomb and timed virus)
  uint8_t shifted_bomb, shifted_timed, shifted_vtimed, encoded;
  shifted_bomb = bomb << 7;
  shifted_timed = timed << 6;
  shifted_vtimed = virus_timed << 5;
  encoded = shifted_bomb | shifted_timed | shifted_vtimed | virus_timer;
  return encoded;
}

uint32_t encode_cell(Cell cell){
  uint32_t shifted_row, shifted_col, shifted_color, shifted_group, encoded;
  
  shifted_row = cell.row << 24;
  shifted_col = cell.col << 16;
  shifted_group = cell.group << 8;
  shifted_color = cell.color << 4;
  
  encoded = shifted_row | shifted_col | shifted_group | shifted_color | cell.type;
  return encoded;
}

void decode_config(uint8_t encoded){
  bomb = encoded & 0b10000000;
  timed = encoded & 0b01000000;
  virus_timed = encoded & 0b00100000;
  virus_timer = encoded & 0b00011111;
}

Cell decode_cell(uint32_t encoded){
  uint32_t bitmask_row, bitmask_col, bitmask_group, bitmask_colortype;
  uint32_t shifted_row, shifted_col, shifted_group, shifted_colortype;
  uint8_t row, col, group, colortype;

  bitmask_colortype = 0b00000000000000000000000011111111;
  colortype = encoded & bitmask_colortype;

  bitmask_group = 0b00000000000000001111111100000000;
  shifted_group = encoded & bitmask_group;
  group = shifted_group >> 8;

  bitmask_col = 0b00000000111111110000000000000000;
  shifted_col = encoded & bitmask_col;
  col = shifted_col >> 16;

  bitmask_row = 0b11111111000000000000000000000000;
  shifted_row = encoded & bitmask_row;
  row = shifted_row >> 24;
  
  Cell decoded = Cell(row, col, 0);
  decoded.group = group;
  decoded.color = colortype >> 4;
  decoded.type = colortype & 0b00000000000000000000000000001111;
  
return decoded;
}

void save_game(Pill current){
  int save = 1;
  uint8_t dimension;
  uint8_t encoded_config = encode_config();
  for(int i=0; i<rows; i++){
    for(int j=0; j<cols; j++){
      if (grid[i][j].color != 0 || grid[i][j].group == current.left.group || grid[i][j].group == current.right.group){
      uint32_t encoded = encode_cell(grid[i][j]);
      for (int k=0; k<4; k++){
        EEPROM.put(save, (encoded >> (8*k)) & 0xFF);
        save++;
      }
      }
    }
  }
  EEPROM.put(0, save);             // how many cells to read
  EEPROM.put(save, cols);          // only save the cols because rows is always cols*2
  EEPROM.put(save+SCOREADRR, score);
  EEPROM.put(save+RIGHTADRR, current.right.color);
  EEPROM.put(save+LEFTADRR, current.left.color);
  EEPROM.put(save+CONFIGADRR, encoded_config); 
  EEPROM.put(save+VCTRADRR, virus_ctr);    // how many virus left
  EEPROM.put(save+LEVELADRR, level_ctr);    // for the speed of the level
  EEPROM.commit();
}

void load_game(Pill &current){
  int address = 1;
  int save;
  uint8_t lgroup, lcolor, rcolor, s;
  uint8_t encoded_config;
  bool bool1, bool2;
  EEPROM.get(0, save);
  EEPROM.get(save, cols);
  EEPROM.get(save+SCOREADRR, s);
  EEPROM.get(save+RIGHTADRR, rcolor);
  EEPROM.get(save+LEFTADRR, lcolor);
  EEPROM.get(save+CONFIGADRR, encoded_config);
  EEPROM.get(save+VCTRADRR, virus_ctr);
  EEPROM.get(save+LEVELADRR, level_ctr);
  uint32_t encoded = 0;
  score = s;
  decode_config(encoded_config);
  uint8_t info;
  free_grid();
  rows = 2*cols;
  init_grid();
  empty_grid();

  while(address < save){
    for(int i=0; i<4; i++){
      EEPROM.get(address, info);
      encoded = encoded | (info << (8*i));
      address++;
    }
    Cell decoded = decode_cell(encoded); 
    grid[decoded.row][decoded.col] = decoded;
    encoded = 0;
  }
  current.left.color = lcolor;
  current.right.color = rcolor;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////// DRAW FUNCTIONS ////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

void print_s(int pos_x, int pos_y, char txt[], int size, int color){
  M5.Lcd.setCursor(pos_x, pos_y);
  M5.Lcd.setTextSize(size);
  M5.Lcd.setTextColor(color);
  M5.Lcd.printf(txt);

}

void draw_explosion(int i, int j){
  int width = calculate_width(cols);
  int height = calculate_height(rows);
  M5.Lcd.fillRoundRect(j * width, i * height, width, height, 10, ORANGE);
}

void draw_cell(uint8_t r, uint8_t c, int color, int width, int height){
  M5.Lcd.fillRoundRect(c * width, r * height, width, height, 4, color);
  M5.Lcd.drawRoundRect(c * width, r * height, width, height, 4, BLACK);
  return;
}

void draw_timed(uint8_t r, uint8_t c, int color, int width, int height){
  M5.Lcd.drawTriangle(c * width+(width/2), r * height, c * width, r * height+height-2, c * width+width-1, r * height+height-2, color);
  return;
}

void draw_virus(uint8_t r, uint8_t c, int color, int width, int height){
  M5.Lcd.fillTriangle(c * width+(width/2), r * height, c * width, r * height+height-2, c * width+width-1, r * height+height-2, color);
  return;
}

void draw_grid(Cell** &grid, uint8_t rows, uint8_t cols){
  int width = calculate_width(cols);
  int height = calculate_height(rows);
  int color;
  for (int i = 1; i < rows; i++){
    for (int j = 0; j < cols; j++){
      Cell &cell = grid[i][j];
      if (cell.color != 0 && cell.type == BOMB){
        M5.Lcd.fillRoundRect(j * width, i * height, width, height, 10, DARKGREY);
      }
      else if (cell.color != 0){
        if (cell.color == 1) color = RED;
        else if (cell.color == 2) color = BLUE;
        else if (cell.color == 3) color = YELLOW;
        if (cell.type == PILL) draw_cell(i, j, color, width, height);
        else if (cell.type == TIMED) draw_timed(i, j, color, width, height);
        else draw_virus(i, j, color, width, height);
      }
      else if (cell.color == 0) M5.Lcd.fillRect(j * width, i * height, width, height, BLACK);
    }
  }
}

int convert_color(uint8_t color){
  switch (color){
  case 1:
    return RED;
    break;
  case 2:
    return BLUE;
    break;
  case 3:
    return YELLOW;
    break;
  case 9:
    return DARKGREY;
    break;
  default:
    return BLACK;
    break;
  }
}

void draw_upper(Pill &pill){
  int width = 15;
  int height = 15;
  M5.Lcd.fillRect(0, 0, SCRWIDTH, 30, BLACK);
  if (pill.left.color == 9) M5.Lcd.fillRoundRect(SCRWIDTH-2*width, 0, width, height, 10, convert_color(pill.left.color));
  else {
    M5.Lcd.fillRoundRect(SCRWIDTH-2*width, 0, width, height, 4, convert_color(pill.left.color));
    M5.Lcd.fillRoundRect(SCRWIDTH-2*width+width, 0, width, height, 4, convert_color(pill.right.color));
  }
  char next[] = "NEXT: ";
  print_s(SCRWIDTH-4*width, 0, next, 1, WHITE);
  M5.Lcd.setCursor(5, 0);
  M5.Lcd.setTextSize(1);
  M5.Lcd.printf("%i", score);
  if (virus_timed){
    M5.Lcd.setCursor(45, 0);
    M5.Lcd.printf("%i", virus_timer);
  }
}

void draw_level(){
  M5.Lcd.fillScreen(black_color);
  M5.Lcd.setCursor(SCRWIDTH/4, SCRHEIGHT/2);
  M5.Lcd.setTextSize(2);
  M5.Lcd.printf("LEVEL %i", level_ctr);
  level_ctr++;
}

void draw_lost(){
  delay(100);
  char lost[] = "YOU LOST!";
  char scoretxt[] = "SCORE";
  M5.Lcd.fillScreen(black_color);
  print_s(SCRWIDTH/6, SCRHEIGHT/3, lost, 2, WHITE);
  print_s(SCRWIDTH/4, SCRHEIGHT/2, scoretxt, 2, WHITE);
  M5.Lcd.setCursor(SCRWIDTH/3, SCRHEIGHT/2+30);
  M5.Lcd.printf("%i", score);
}

void draw_next_level(){
  delay(100);
  M5.Lcd.fillScreen(black_color);
  M5.Lcd.setCursor(SCRWIDTH/4, SCRHEIGHT/2);
  M5.Lcd.setTextSize(2);
  M5.Lcd.printf("LEVEL %i", level_ctr);
}

void draw_loading(){
  M5.Lcd.fillScreen(black_color);
  char loading[] = "Loading saved game...";
  print_s(2, SCRHEIGHT/2, loading, 1, WHITE);
  delay(2000);
  M5.Lcd.fillScreen(black_color);
}

uint8_t choice = 0, previous_choice = 1;
void draw_menu(){
  while(true){
    M5.update();
    if (M5.BtnA.wasPressed()){
      choice = (choice+1)%4;
    } else if (M5.BtnB.wasPressed()){
      break;
    }
    if (choice != previous_choice){
      M5.Lcd.fillScreen(black_color);
      char mario[] = "Dr.MARIO";
      char option[] = "Choose an option:";
      char nones[] = "> None";
      char bombs[] = "> Bomb";
      char tvrius[] = "> Timed virus";
      char bandts[] = "> Bomb & Virus";
      int color = WHITE;
      print_s(SCRWIDTH/6, SCRHEIGHT/4, mario, 2, WHITE);
      print_s(SCRWIDTH/5-5, SCRHEIGHT/3+20, option, 1, WHITE);
      if (choice == 0){
        color = YELLOW;
        previous_choice = 0;
      } else color = WHITE;
      print_s(SCRWIDTH/5-5, SCRHEIGHT/3+40, nones, 1, color);
      if (choice == 1){
        color = YELLOW;
        previous_choice = 1;
      } else color = WHITE;
      print_s(SCRWIDTH/5-5, SCRHEIGHT/3+60, bombs, 1, color);
      if (choice == 2){
        color = YELLOW;
        previous_choice = 2;
      } else color = WHITE;
      print_s(SCRWIDTH/5-5, SCRHEIGHT/3+80, tvrius, 1, color);
      if (choice == 3){
        color = YELLOW;
        previous_choice = 3;
      } else color = WHITE;
      print_s(SCRWIDTH/5-5, SCRHEIGHT/3+100, bandts, 1, color);
    }
  }
  M5.Lcd.fillScreen(black_color);
  switch (choice){
  case 0:
    break;
  case 1:
    bomb = true;
    break;
  case 2:
    timed = true;
    break;
  case 3:
    bomb = true;
    timed = true;
    break;
  }
}

////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////// GAME ////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////

Pill pill;
Pill next_pill;
Pill current = pill;
Pill next = next_pill;

void setup_game(){
  srand(time(NULL));
  check_configuration();
  init_grid();
  empty_grid();
  current.init();
  next.init();
  current.add_pill();
  draw_level();
  delay(200);
  M5.Lcd.fillScreen(black_color);
  virus_ctr = 5;
  virus_timer = rand()%11+10;;
  init_virus(grid);
  draw_upper(next);
}

float acc_x, acc_y, acc_z;
void setup(){
  M5.begin();
  Serial.begin(115200);
  Serial.flush();
  EEPROM.begin(512);
  draw_menu();
  delay(100);
  M5.IMU.Init();
  setup_game();
}
void loop(){
  M5.update();
  delay(speed);
  if (!check_win() && !check_lost(current)){
  if (M5.BtnA.isPressed() && M5.BtnB.isPressed()){
    draw_loading();
    free_grid();
    setup_game();
    load_game(current);
    draw_upper(current);
    draw_grid(grid, rows, cols);
  } else if (M5.BtnB.isPressed()){
      save_game(current);
  }
  draw_grid(grid, rows, cols);
  finish = true;
  if (current.check_end()){
    if (current.left.type == BOMB){
      current.explode(grid);
    }
    delay(100);
    for (int i = rows - 1; i >= 0; i--){
      for (int j = cols - 1; j >= 0; j--){
        check_cells(grid, i, j, false);
        check_cells(grid, i, j, true);
      }
    }
    draw_grid(grid, rows, cols);
    fall(grid, rows, cols);
    draw_grid(grid, rows, cols);
    Pill new_pill;
    current = next;
    next = new_pill;
    next.init();
    current.add_pill();
    speed = 50*level_ctr;
    draw_upper(next);
  }
  if (M5.BtnA.isPressed()) current.rotate_pill();
  acc_x = 0, acc_y = 0;
  M5.Imu.getAccelData(&acc_x, &acc_y, &acc_z);
  current.move_pill(acc_x, acc_y);
} else if (check_win()){
    if (finish){
      draw_next_level();
      finish = false;
      Pill pill;
      Pill next_pill;
      current = pill;
      next = next_pill;
      free_grid();
      cols = rand()%7+6;
      rows = 2*cols;
      setup_game();
      }
    }
  else if(check_lost(current)){
    if (finish){
      draw_lost();
      finish = false;
    }
  }
}