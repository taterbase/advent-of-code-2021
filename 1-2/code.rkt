#lang racket

(define (solve-for input)
  (define (to-num str)
    (string->number (string-trim str)))

  (define (iter incs last-sum first-num second-num next-line)
    (if (not (eof-object? next-line))
        (let* ([next-num (to-num next-line)]
              [new-sum (+ first-num second-num next-num)])

          (if (> new-sum last-sum)
              (iter (+ incs 1) new-sum second-num next-num (read-line input))
              (iter incs new-sum second-num next-num (read-line input))))
        incs))

  (let ([a (to-num (read-line input))]
        [b (to-num (read-line input))]
        [c (to-num (read-line input))])
    (iter 0 (+ a b c) b c (read-line input))))

(solve-for (open-input-file "input.txt"))