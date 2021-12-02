#lang racket

(define (to-num str)
  (string->number (string-trim str)))

(define (move-sub input)
  
  (define (iter aim horiz depth next-line)
    (if (eof-object? next-line)
        (begin (close-input-port input)
               (* horiz depth))

        (let* ([cmd (string-split next-line)]
              [dir (car cmd)]
              [amt (to-num (car (cdr cmd)))]
              [new-line (read-line input)])
          (cond ((string=? dir "forward")
                 (iter aim (+ horiz amt) (+ depth (* amt aim)) new-line))
                ((string=? dir "down")
                 (iter (+ aim amt) horiz depth new-line))
                (else
                 (iter (- aim amt) horiz depth new-line))))))
    
  (iter 0 0 0 (read-line input)))

(move-sub (open-input-file "input.txt"))