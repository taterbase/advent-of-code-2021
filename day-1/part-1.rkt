#lang racket

(define (solve-for input)
  (define (to-num str)
    (string->number (string-trim str)))

  (define (iter incs last-num next-line)
    (cond ((eof-object? next-line) incs)
          ((> (to-num next-line) last-num)
           (iter (+ incs 1) (to-num next-line) (read-line input)))
          (else (iter incs (to-num next-line) (read-line input)))))

  (iter 0 (to-num (read-line input)) (read-line input)))

(solve-for (open-input-file "input.txt"))