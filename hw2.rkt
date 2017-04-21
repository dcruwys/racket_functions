#lang racket
;;;HW2.1 wraps parentheses around each top-level ele- ment of a list and returns the new list.
(define (demote l)
  (cond [(null? l) empty]
        [(cons (list(car l)) (demote (cdr l)))]))

;;;HW2.2 removes one pair of parentheses from around each top-level element of a list.
(define (promote l)
  (cond [(null? l) empty]
        [(list? (car l)) (append (car l) (promote (cdr l)))]
        [else (cons (car l) (promote (cdr l)))]))

;;;HW2.3 returns a list of the symbols in the order in which they occur.
(define (flatten l)
  (cond [(null? l) empty]
        [(list? (car l)) (append (flatten (car l)) (flatten (cdr l)))]
        [else (cons (car l) (flatten (cdr l)))]))

;;;Examples 
(require test-engine/racket-tests)
(check-expect (demote '(1 2 3)) '((1) (2) (3)))
(check-expect (demote '((a) (good) (day))) '(((a)) ((good)) ((day))))
(check-expect (demote '(a (more (complicated)) example)) '((a) ((more (complicated))) (example)))

(check-expect (promote '((1 2)(3 4))) '(1 2 3 4))
(check-expect (promote '((x (y)) z)) '(x (y) z))
(check-expect (promote '(a (more (complicated)) example)) '(a more (complicated) example))

(check-expect (flatten '((((((1 2 3 4))))))) '(1 2 3 4))
(check-expect (flatten '(1 (2 3) (4 (5)))) '(1 2 3 4 5))
(check-expect (flatten '(1 2 (a b c))) '(1 2 a b c))


(test)

        
