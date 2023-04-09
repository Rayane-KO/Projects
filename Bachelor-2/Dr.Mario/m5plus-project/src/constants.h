#include <M5StickCPlus.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#ifndef CONSTANTS_H
#define CONSTANTS_H

#define MIN_TILT 0.15
#define MAX_SPEED 400
#define MIN_SPEED 100
#define INVALID -1
#define EMPTY 0
#define PILL 1
#define VIRUS 2
#define BOMB 3
#define TIMED 4
#define SCRWIDTH M5.Lcd.width()
#define SCRHEIGHT M5.Lcd.height()
#define SCOREADRR 1
#define RIGHTADRR 2
#define LEFTADRR 3
#define CONFIGADRR 4
#define VCTRADRR  5
#define LEVELADRR 6

uint32_t black_color = M5.Lcd.color565(0, 0, 0);
uint32_t red_color = M5.Lcd.color565(255, 0, 0);
//configuration
uint8_t cols = 8;
uint8_t rows = 2*cols; 
bool bomb = false;
bool timed = false;
// counters
int8_t virus_ctr = 5;
bool virus_timed = false;
uint8_t timed_ctr = 0;
uint8_t level_ctr = 1;
// game vars
uint16_t group_ctr = 1;
int speed = 50 + 50*level_ctr;
int score = 0;
// timers
int8_t bomb_timer = INVALID;  
int8_t virus_timer = INVALID;
// draw vars
bool finish = true;

#endif