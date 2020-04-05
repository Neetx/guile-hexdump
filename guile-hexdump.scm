#!/usr/bin/guile  -s
!#

; Copyright (C) 2020 Neetx
; This file is part of guile-hexdump.
;
; guile-hexdump is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; guile-hexdump is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with guile-hexdump.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (ice-9 binary-ports))
(use-modules (ice-9 format))
(use-modules (rnrs bytevectors))
(use-modules (rnrs base))

;-----------------------------------------------------------------------------------------------------------------------------
(define bytevector-to-hex-list			   ;INPUT:bytevector -> OUTPUT: List of string, each one is a 16 bytes hex
    (lambda (v)
        (map 
            (lambda(x)
                (format #f "~2,,,'0@a" (number->string x 16))
            )
            (bytevector->u8-list v))))

(define bytevector-to-ascii-list  			;INPUT:bytevector -> OUTPUT: List of string, each one is an ascii char
    (lambda (v)
        (map
            (lambda(x)
                (if (and (> x 32) (<= x 126))
                    (string (integer->char x))
                    "."))
                (bytevector->u8-list v))))
;-------------------------------------------------------------------------------------------------------------------------------

(define bytevector-to-hex-string   			;INPUT: bytevector -> OUTPUT: one string of hex  eg."FF FF FF FF"
    (lambda (v)
        (string-join (bytevector-to-hex-list v))))

(define bytevector-to-ascii-string 			;INPUT; bytevector -> OUTPUT: one string of ascii char eg. "q w e r t y"
    (lambda (v)
        (string-join (bytevector-to-ascii-list v))))
;--------------------------------------------------------------------------------------------------------------------------------

(define printhexascii
    (lambda (file l)
        (define i (get-bytevector-n file 16))
        (if (eof-object? i)
            1
            (begin
                (display (format #f "~8,,,'0@a" (number->string l 16)))
                (display "  ")
                (display (bytevector-to-hex-string i))
                (if (< (bytevector-length i) 16) (display (format #f "~v{~A~:*~}" (*(- 16 (bytevector-length i)) 3) '(" "))))  ;last raw format handling
                (display "  |")
                (display (string-delete #\space (bytevector-to-ascii-string i)))
                (display "|")
                (newline)
                (printhexascii file (+ l 1))))))

(define help
    (lambda ()
        (display "Usage: hexdump <filename>\n")))

(define-syntax try
    (syntax-rules (catch)
        ((_ body (catch catcher))
            (call-with-current-continuation
                (lambda (exit)
                    (with-exception-handler
                        (lambda (condition)
                            catcher
                            (exit condition))
                        (lambda () body)))))))

(define input (program-arguments)) ;TODO: Argparse

(if (<= (length input) 1)
    (begin
        (display "ERROR: Please insert a file name.\n")
        (help)))

(define file (list-ref input 1))

(try 
    (begin 
        (define binaryfile (open-file file "rb"))
        (printhexascii binaryfile 0))
    (catch
        (begin
            (display "ERROR: File not found\n")
            (help))))