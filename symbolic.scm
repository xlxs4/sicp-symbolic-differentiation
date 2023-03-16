(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation base expn)
  (cond ((=number? base 1) 1)
        ((=number? expn 0) 1)
        ((=number? expn 1) base)
        ((and (number? base) (number? expn))
         (expt base expn))
        (else (list '** base expn))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (let ((m1 (multiplier exp))
               (m2 (multiplicand exp)))
           (make-sum
            (make-product m1 (deriv m2 var))
            (make-product (deriv m1 var) m2))))
        ((exponentiation? exp)
         (let ((b (base exp))
               (e (exponent exp)))
           (make-product
            (make-product
             e
             (make-exponentiation b
                                  (make-sum e -1)))
            (deriv b var))))
        (else (error "unknown expression
                      type: DERIV" exp))))

;; (deriv '(+ x 3) 'x)
;; (deriv '(* x y) 'x)
;; (deriv '(** x 2) 'x)
