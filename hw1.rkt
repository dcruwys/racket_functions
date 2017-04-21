#lang racket
(define theList '((1 2) (3 4) (5 6)))
;;;Ascending HW1.1
(define (ascending? listOfNum)
  (cond
    [(= (length listOfNum) 1) true]
    [(> (first listOfNum) (second listOfNum)) false]
    [else (ascending? (rest listOfNum))]))

;;;Reverse-Pairs HW1.2
(define (reverse-pairs listOfPair)
  (cond
    [(null? listOfPair) listOfPair]
    [else (cons (reverse (car listOfPair)) (reverse-pairs (rest listOfPair)))]))

;;;Translate Single Digits HW1.3
(define (translate-digits listOfNum)
  (cond
    [(empty? listOfNum) listOfNum]
    [(equal? (car listOfNum) 0) (cons 'Zero (translate-digits (rest listOfNum)))]
    [(equal? (car listOfNum) 1) (cons 'One (translate-digits (rest listOfNum)))]
    [(equal? (car listOfNum) 2) (cons 'Two (translate-digits (rest listOfNum)))]
    [(equal? (car listOfNum) 3) (cons 'Three (translate-digits (rest listOfNum)))]
    [(equal? (car listOfNum) 4) (cons 'Four (translate-digits (rest listOfNum)))]
    [(equal? (car listOfNum) 5) (cons 'Five (translate-digits (rest listOfNum)))]
    [(equal? (car listOfNum) 6) (cons 'Six (translate-digits (rest listOfNum)))]
    [(equal? (car listOfNum) 7) (cons 'Seven (translate-digits (rest listOfNum)))]
    [(equal? (car listOfNum) 8) (cons 'Eight (translate-digits (rest listOfNum)))]
    [(equal? (car listOfNum) 9) (cons 'Nine (translate-digits (rest listOfNum)))]
    [else (print "Error incorrect value")]))

;;; Duplicate HW 1.4
(define (duplicate n listOfDups)
  (if (= n 1) (cons listOfDups null) (cons listOfDups (duplicate (- n 1) listOfDups))))
  
;;;Count Symbol HW 1.5
;;;Explained by John Zouzounis, I couldn't find a better solution so I just messed around with his. LMK if you have any questions.
(define (count-symbol s List)
  (cond [(null? List) (+ 0)]
    [(list? (first List)) (+ (count-symbol s (first List)) (count-symbol s (rest List)))] 
    [(equal? s (first List)) (+ 1 (count-symbol s (rest List)))]
    [else (count-symbol s (rest List))]))

;;; Examples 
(require test-engine/racket-tests)
(check-expect (ascending? '(1 2 3 4 5)) #t)
(check-expect (ascending? '(1 2 6 8 10)) #t)
(check-expect (ascending? '(3 2 1 4 5)) #f)

(check-expect (reverse-pairs '((1 2)(3 4)(5 6))) '((2 1) (4 3) (6 5)))
(check-expect (reverse-pairs '((2 1)(4 3)(6 5))) '((1 2) (3 4) (5 6)))
(check-expect (reverse-pairs '((2 1)(6 7)(6 5))) '((1 2) (7 6) (5 6)))

(check-expect (translate-digits '(0 1 2)) '(Zero One Two))
(check-expect (translate-digits '(0 2 3 4)) '(Zero Two Three Four))
(check-expect (translate-digits '(0 1 2 3)) '(Zero One Two Three))

(check-expect (duplicate 2 'Hello) '(Hello Hello))
(check-expect (duplicate 3 '(1 2 3)) '((1 2 3) (1 2 3) (1 2 3)))
(check-expect (duplicate 2 '(1 (2 3))) '((1 (2 3)) (1 (2 3))))

(check-expect (count-symbol 'Hello '(Hello Hello)) 2)
(check-expect (count-symbol 1 '(1 2 1)) 2)
(check-expect (count-symbol 1 '(1 (1 3))) 2)


(test)

