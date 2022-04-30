(define (x lis)
  (cond
    ((null? lis) 0) ; base case if(list.empty)
    ((not (list? (car lis))) ;if(!firstElem.isList) first element is not a a list
      (cond ;
        ((< (car lis) 0) (x (cdr lis))) ; if(firstElement < 0, ignore negative number recall x, passing the list removing first element)
        (else (+ (car lis) (x (cdr lis)))) ; if(firstElement > -1 add the number info the result, recall x, passing the list removing first element)
      )
    )
    (else (+ (x (car lis)) (x (cdr lis)))) ; if(firstElem.isList) recall the function passing the sublist to get the sum of the sublist
  )
)

;(x '())
(x '((2 4) 6))
