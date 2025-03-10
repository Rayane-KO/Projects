;;;; RaRa GPIO
;;;; Raspberry Pi GPIO binding for Racket
;;;; Version 0.1/jun16
;;;; Copyright © David H. Christensen <me@davidh.info>, 2016
;;;; Licensed under the MIT license
;;;;
;;;; Requires wiringPi library installation. If you're using Raspbian,
;;;; this can be accomplished by entering
;;;;      sudo apt-get install wiringpi
;;;; in your favorite shell.
;;;; WiringPi is the property of Drogon at wiringpi.com. This is only a
;;;; binding for the world's favorite LISP dialect, with a few extensions.
;;;;
;;;; To make development more "natural" and LISP-y, several helper functions
;;;; are supplied.

#lang racket
(require ffi/unsafe)
(define wiringpi-lib (ffi-lib "/usr/lib/libwiringPi"))

(provide (except-out (all-defined-out) wiringpi-lib))

;;; General-purpose IO

;; Setup
(define gpio-setup
  (get-ffi-obj "wiringPiSetup" wiringpi-lib (_fun -> _int32)
               (lambda () (error 'wiringpi-lib "WiringPI does not provide wiringPiSetup"))))

(define gpio-pin-mode-native
  (get-ffi-obj "pinMode" wiringpi-lib (_fun _int32 _int32 -> _void)
               (lambda () (error 'wiringpi-lib "pinMode missing"))))

(define gpio-pull-up-down-control
  (get-ffi-obj "pullUpDnControl" wiringpi-lib (_fun _int32 _int32 -> _void)
               (lambda () (error 'wiringpi-lib "pullUpDnControl missing"))))

;; Digital interfacing
(define gpio-digital-write
  (get-ffi-obj "digitalWrite" wiringpi-lib (_fun _int32 _int32 -> _void)
               (lambda () (error 'wiringpi-lib "digitalWrite missing"))))

(define gpio-digital-read
  (get-ffi-obj "digitalRead" wiringpi-lib (_fun _int32 -> _int32)
               (lambda () (error 'wiringpi-lib "digitalRead missing"))))

;; PWM 
(define gpio-pwm-write
  (get-ffi-obj "pwmWrite" wiringpi-lib (_fun _int32 _int32 -> _void)
               (lambda () (error 'wiringpi-lib "pwmWrite missing"))))

;; Analog interfacing
(define gpio-analog-write
  (get-ffi-obj "analogWrite" wiringpi-lib (_fun _int32 _int32 -> _void)
               (lambda () (error 'wiringpi-lib "digitalWrite missing"))))

(define gpio-analog-read
  (get-ffi-obj "analogRead" wiringpi-lib (_fun _int32 -> _int32)
               (lambda () (error 'wiringpi-lib "digitalRead missing"))))

;;; Timing
(define gpio-elapsed-ms
  (get-ffi-obj "millis" wiringpi-lib (_fun -> _uint32)
               (lambda () (error 'wiringpi-lib "millis missing"))))

(define gpio-elapsed-µs
  (get-ffi-obj "micros" wiringpi-lib (_fun -> _uint32)
               (lambda () (error 'wiringpi-lib "micros missing"))))

(define gpio-delay-ms
  (get-ffi-obj "delay" wiringpi-lib (_fun _uint32 -> _void)
               (lambda () (error 'wiringpi-lib "delay missing"))))

(define gpio-delay-µs
  (get-ffi-obj "delayMicroseconds" wiringpi-lib (_fun _uint32 -> _void)
               (lambda () (error 'wiringpi-lib "delayMicroseconds missing"))))


;; Pin mode defines
(define π-gpio-pin-input 0)
(define π-gpio-pin-output 1)
(define π-gpio-pin-pwm 2)
(define π-gpio-pin-clock 3)
(define π-gpio-pin-soft-pwm 4)
(define π-gpio-pin-soft-tone 5)
(define π-gpio-pin-pwm-tone 6)

;; Pullup/down modes
(define π-gpio-pull-up 2)
(define π-gpio-pull-down 1)
(define π-gpio-pull-off 0)



;;; Custom helpers
;; More "native" pin mode setter
(define gpio-set-pin-mode
  (lambda (pin mode)
    (cond
      [(eq? mode 'input) (gpio-pin-mode-native pin π-gpio-pin-input)]
      [(eq? mode 'output) (gpio-pin-mode-native pin π-gpio-pin-output)]
      [(eq? mode 'pwm) (gpio-pin-mode-native pin π-gpio-pin-pwm)]
      [(eq? mode 'clock) (gpio-pin-mode-native pin π-gpio-pin-clock)]
      [(eq? mode 'soft-pwm) (gpio-pin-mode-native pin π-gpio-pin-soft-pwm)]
      [(eq? mode 'soft-tone) (gpio-pin-mode-native pin π-gpio-pin-soft-tone)]
      [(eq? mode 'pwm-tone) (gpio-pin-mode-native pin π-gpio-pin-pwm-tone)]
      [else (error 'gpio-set-pin-mode (string-join "Erroneous pin mode passed: " (symbol->string mode) ". Valid: 'input 'output 'pwm 'clock 'soft-pwm 'soft-tone 'pwm-tone"))])))

(define gpio-set-pull-resistor
  (lambda (pin mode)
    (cond
      [(eq? mode 'up) (gpio-pull-up-down-control pin π-gpio-pull-up)]
      [(eq? mode 'down) (gpio-pull-up-down-control pin π-gpio-pull-down)]
      [(eq? mode 'off) (gpio-pull-up-down-control pin π-gpio-pull-off)]
      [else (error 'gpio-set-pull-resistor (string-join "Erroneous pullup/down mode passed: " (symbol->string mode) ". Valid: 'up, 'down, 'off'"))])))
                            

;; GPIO delay
(define gpio-delay-seconds
  (lambda (seconds)
    (gpio-delay-µs (exact-floor (* seconds 1000000)))))

;; Elapsed time in seconds
(define gpio-elapsed-seconds
  (lambda ()
    (exact->inexact (/ (gpio-elapsed-ms) 1000))))
      
;; Write a byte of data sequentially to specified pin
(define gpio-write-serial-byte
  (lambda (pin byte bit-duration)
    (letrec ([gpio-write-byte-op
           (lambda (offset)
             (if (<= offset 7)
                 (begin
                   (if (bitwise-bit-set? byte offset) (gpio-digital-write pin 1) (gpio-digital-write pin 0))
                   (gpio-delay-seconds bit-duration)
                   (gpio-write-byte-op (+ offset 1)))
                 (#t)))]) (gpio-write-byte-op 0))))


;; Software PWM

(define gpio-soft-pwm-create
  (get-ffi-obj "softPwmCreate" wiringpi-lib (_fun _int32 _int32 _int32 -> _void)
               (lambda () (error 'wiringpi-lib "softPwmCreate missing"))))

(define gpio-soft-pwm-write
  (get-ffi-obj "softPwmWrite" wiringpi-lib (_fun _int32 _int32 -> _void)
               (lambda () (error 'wiringpi-lib "softPwmWrite missing"))))

;; SPI

(define gpio-spi-setup
  (get-ffi-obj "wiringPiSPISetup" wiringpi-lib (_fun _int32 _int32 -> _int32)
               (lambda () (error 'wiringpi-lib "wiringPiSPISetup missing"))))
           
(define gpio-spi-data-rw
  (get-ffi-obj "wiringPiSPIDataRW" wiringpi-lib (_fun _int32 _pointer _int32 -> _int32)
               (lambda () (error 'wiringpi-lib "wiringPiSPIDataRW missing"))))


;; Extension: I2C MCP23008 / MCP23017 i2c input/output expander
;; http://wiringpi.com/extensions/i2c-mcp23008-mcp23017/

(define gpio-mcp23017-setup
  (get-ffi-obj "mcp23017Setup" wiringpi-lib (_fun _int32 _int32 -> _void)
               (lambda () (error 'wiringpi-lib "mcp23017Setup missing"))))

(define gpio-mcp23008-setup
  (get-ffi-obj "mcp23008Setup" wiringpi-lib (_fun _int32 _int32 -> _void)
    (lambda () (error 'wiringpi-lib "mcp23008Setup missing"))))


;; Extension: MCP3008
;; http://shaunsbennett.com/piblog/?p=266

(define (gpio-mcp3008-setup spi-channel)
    (gpio-spi-setup spi-channel 1000000))
    
(define (gpio-mcp3008-analog-read spi-channel channel-config analog-channel)
    (if (or (< analog-channel 0) (> analog-channel 7))
        -1
        (let ([buffer (bytes 1 0 0)])
            (bytes-set! buffer 1 (arithmetic-shift (+ channel-config analog-channel) 4))
            (gpio-spi-data-rw spi-channel buffer 3)
        (+ (arithmetic-shift (bitwise-and (bytes-ref buffer 1) 3) 8) (bytes-ref buffer 2)))))
        
(define (gpio-mcp3008-simple-read analog-channel)
    (define spi-channel 0)
    (define channel-config-single 8)
    (define channel-config-diff 0)
    (define channel-config channel-config-single)
    (gpio-mcp3008-setup spi-channel)
    (gpio-mcp3008-analog-read spi-channel channel-config analog-channel))


