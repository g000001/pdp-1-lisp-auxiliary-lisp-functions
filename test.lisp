(absval (minus 4))


(and t t t) 
(and t nil t)


(caar (quote ((1))))
(caar (quote ((1 2))))
(caar (quote ((1 .2))))


(cadr (quote (1 2)))


(cdar (quote ((1 2 3 4))))


(cddr (quote (1 2 3 4)))


(equal 1 1)
(equal (quote (1 2 3 4))
       (quote (1 2 3 4)))


(assoc 1 (quote ((2.2) (3.3) (1.1))))

(assoc 2
       (cons (cons 2 2)
             (cons (cons 3 3)
                   (cons (cons 1 1) nil))))



(cset (quote qqq) 1)
(eval qqq ())


(difflist 1 (quote (2 3 1 4 5)))
