
(define (countpositive lis)
  (cond
    ((null? lis) 0)
    ((not (list? (car lis))) ; if not a list == true
      (cond
        ((<= (car lis) 0) (countpositive (cdr lis)))
        (else (+ 1 (countpositive (cdr lis))))
      )
    )
    (else ; if first element is list (optimization for sublist in list)
      (+ (countpositive (car lis)) (countpositive (cdr lis)))
    )
  )
)


(define (x lis)
  (cond
    ((null? lis) 0)
    ((not (list? (car lis)))
      (cond
        ((< (car lis) 0) (x (cdr lis)))
        (else (+ (car lis) (x (cdr lis))))
      )
    )
    (else (+ (x (car lis)) (x (cdr lis))))
  )
)
;(countpositive '(0 -1 2 -3 4 (-5 6) -7 8 (-9 10)) ; positive: {2,4,6,8,10} (5)
;(countpositive '(7 -6 0 7 -5 0 4 0 -7 9))
;(countpositive '((-1 2) (-3 4) ((-5 6 (-7 8)))))
;(x '(0))

(define (countAtoms l1)
  (cond
    ((null? l1) 0)
    (else (+ 1 (countAtoms (cdr l1)))
    )
  )
)

(define (xor a b)
  (cond
    ((and a b) #f)
    ((or a b) #t)
    (else #f)
  )
)

(define (struc_compare l1 l2)
  (cond
    ((xor (null? l1) (null? l2)) #f)
    ((and (null? l1) (null? l2)) #t)
    (else
      (cond
        ((= (countAtoms l1) (countAtoms l2))
          (cond
            ((and (list? (car l1)) (list? (car l2)))
              (struc_compare (car l1) (car l2))
            )
            (else #f)

          ) ; end of cond 3
          (struc_compare (cdr l1) (cdr l2))
        )
        (else #f)
      ) ; end of cond 2
    )
  ) ; end of cond 1
)



;(struc_compare '(1 2 3) '(4 5 6)) ; expected output = true
;(struc_compare '((1 2) 3 4) '((a b) c)) ; expected output = false
;(struc_compare '((1 2) 3 4) '((a b) c d)) ; expected output = true
(struc_compare '((1 2) (3 (4))) '((a b) (c (d)))) ; expected output = true
