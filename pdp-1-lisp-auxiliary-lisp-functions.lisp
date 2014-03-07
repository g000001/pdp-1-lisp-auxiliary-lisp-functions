(quote (AUXILIARY LISP FUNCTIONS)) 


(rplacd (quote absval) 
        (quote (expr (lambda (x) 
                       (cond ((greaterp 0 x) (minus x))
                             (t x))))))  

 
(rplacd (quote and)
        (quote (fexpr (lambda (x a)
                        (prog ()
                           n  (cond ((null x) (return t))
                                    ((null (eval (car x) a))
                                     (return nil)))
                              (setq x (cdr x))
                              (go n))))))

 
(rplacd (quote caar)
        (quote (expr (lambda (x) (car (car x))))))


(rplacd (quote cadr)
        (quote (expr (lambda (x) (car (cdr x))))))


(rplacd (quote cdar)
        (quote (expr (lambda (x) (cdr (car x))))))


(rplacd (quote cddr)
        (quote (expr (lambda (x) (cdr (cdr x))))))


(rplacd (quote equal)
        (quote (expr (lambda (x y)
                       (cond ((atom x) (eq x y))
                             ((atom y) nil)
                             ((equal (car x) (car y))
                              (equal (cdr x) (cdr y)))
                             (t nil))))))


(rplacd (quote assoc)
        (quote (expr (lambda (x y)
                       (cond ((equal (caar y) x)
                              (car y))
                             (t (assoc x (cdr y))))))))


(rplacd (quote cset)
        (quote (expr (lambda (x y)
                       (rplacd x (list (quote apval) y))))))


(rplacd (quote csetq)
        (quote (fexpr (lambda (x a)
                        (rplacd (car x)
                                (cons (quote expr) (cdr x)))))))


(rplacd (quote dex)
        (quote (fexpr (lambda (x a)
                        (rplacd (car x)
                                (cond (quote expr) (cdr x)))))))


(rplacd (quote dfx)
        (quote (fexpr (lambda (x a)
                        (rplacd (car x)
                                (cond (quote fexpr) (cdr x)))))))


(rplacd (quote difflist)
        (quote (expr (lambda (a x)
                       (cond ((null x) nil)
                             ((equal a (car x))
                              (difflist a (cdr x)))
                             (t (cons (car x)
                                      (difflist a (cdr x)))))))))


(rplacd (quote double)
        (quote (expr (lambda (x) (plus x x)))))


(rplacd (quote rem)
        (quote (expr (lambda (y x)
                       (cond ((equal y x) 0)
                             ((greaterp x y) y)
                             (t (rem (plus y (minus x))
                                     x)))))))


(rplacd (quote zerop)
        (quote (expr (lambda (x)
                       (cond ((equal x 0) t)
                             (t nil))))))


(rplacd (quote gcd)
        (quote (expr (lambda (x y)
                       (cond ((greaterp x y)
                              (gcd y x))
                             ((zerop (rem y x)) x)
                             (t (gcd (rem y x) x)))))))


(rplacd (quote last)
        (quote (expr (lambda (l)
                       (cond ((null l) nil)
                             ((null (cdr l)) (car l))
                             (t (last (cdr l))))))))


(rplacd (quote length)
        (quote (expr (lambda (l)
                       (prog (u v)
                             (setq v 0)
                             (setq u l)
                          a  (cond ((null u)
                                    (return v)))
                             (setq u (cdr u))
                             (setq v (plus 1 v))
                             (go a))))))


(rplacd (quote lengthr)
        (quote (expr (lambda (l)
                       (cond ((null l) 0)
                             (t (plus 1 (lengthr (cdr l)))))))))


(rplacd (quote maplist)
        (quote (expr (lambda (x a)
                       (cond ((null x) nil)
                             (t (cons (a x)
                                      (maplist (cdr x) a))))))))


(rplacd (quote maplist)
        (quote (fexpr (lambda (x a)
                        (prog (v m r)
                              (setq r (setq m (list (eval (cadr x) a))))
                              (setq v (eval (car x) a))
                           p  (cond ((null v)
                                     (return (cdr r))))
                              (setq m (cdr (rplacd m (list (eval (list (car r)
                                                                       (list (quote quote)
                                                                             v))
                                                                 a)))))
                              (setq v (cdr v))
                              (go p))))))


(rplacd (quote member)
        (quote (expr (lambda (a x)
                       (cond ((null x) nil)
                             ((eq a (car x)) t)
                             (t (member a (cdr x))))))))


(rplacd (quote smaller)
        (quote (expr (lambda (x y)
                       (cond ((greaterp x y) y)
                             (t x))))))


(rplacd (quote min)
        (quote (expr (lambda (l)
                       (cond ((null l) nil)
                             ((null (cdr l))
                              (car l))
                             (t (smaller (car l) (min (cdr l)))))))))


(rplacd (quote not)
        (quote (expr null)))


(rplacd (quote or)
        (quote (fexpr (lambda (x a)
                        (prog nil
                           n  (cond ((null x)
                                        (return nil))
                                       ((eval (car x) a)
                                        (return t)))
                              (setq x (cdr x))
                              (go n))))))


(rplacd (quote pairlis)
        (quote (expr (lambda (x y a)
                       (cond ((null x) a)
                             (t (cons (cons (car x) (car y))
                                      (pairlis (cdr x) (cdr y) a))))))))


(quote (PDEF (print and punch definition)))
(rplacd (quote pdef)
        (quote (fexpr (lambda (x a)
                        (list (quote rplacd)
                              (list (quote quote (car x))
                                    (list (quote quote)
                                          (cdr (car x)))))))))


(rplacd (quote quotient)
        (quote (expr (lambda (q d)
                       (prog (u v)
                             (setq v 0)
                             (setq u q)
                          a  (cond ((greaterp d u)
                                    (return v)))
                             (setq u (plus u (minus d)))
                             (setq v (plus 1 v))
                             (go a))))))


(rplacd (quote quotientr)
        (quote (expr (lambda (y x)
                       (cond ((greaterp x y) 0)
                             ((eq x y) 1)
                             ((greaterp y x)
                              (plus 1 (quotientr (plus y (minus x))
                                                 x))))))))


(rplacd (quote r1)
        (quote (expr (lambda (m l)
                       (cond ((null l) m)
                             (t (r1 (cons (car l) m)
                                    (cdr l))))))))
(rplacd (quote reverse)
        (quote (expr (lambda (l) (r1 nil l)))))


(rplacd (quote reverse)
        (quote (expr (lambda (m)
                       (prog (u v)
                             (setq u m)
                          k  (cond ((null u) (return v)))
                             (setq v (cons (car u) v))
                             (setq u (cdr u))
                             (go k))))))


(rplacd (quote sequence)
        (quote (expr (lambda (l)
                       (prog (u v w)
                             (setq u l)
                             (setq v (min l))
                             (setq w nil)
                          a  (cond ((null u) (return w)))
                             (setq v (min u))
                             (setq u (difflist v u))
                             (setq w (append w (list v)))
                             (go a))))))


(rplacd (quote sub2)
        (quote (expr (lambda (a z)
                       (cond ((null a) z)
                             ((eq (caar a) z)
                              (cdar a))
                             (t (sub2 (cdr a) z)))))))


(rplacd (quote sublis)
        (quote (expr (lambda (a y)
                       (cond ((atom y)
                              (sub2 a y))
                             (t (cons (sublis a (car y))
                                      (sublis a (cdr y)))))))))


(rplacd (quote subst)
        (quote (expr (lambda (x y z)
                       (cond ((equal y z) x)
                             ((atom z) z)
                             (t (cons (subst x y (car z))
                                      (subst x y (cdr z)))))))))


(rplacd (quote times)
        (quote (expr (lambda (n m)
                       (cond ((equal n 1) m)
                             (t (plus m (times m (plus n (minus 1))))))))))


(rplacd (quote times)
        (quote (expr (lambda (x n)
                       (prog (u v)
                             (setq v 0)
                             (setq u 0)
                          a  (cond ((eq v n) (return u)))
                             (setq u (plus x u))
                             (setq v (plus v 1))
                             (go a))))))


(rplacd (quote union)
        (quote (expr (lambda (x y)
                       (cond ((null x) y)
                             ((member (car x) y)
                              (union (cdr x) y))
                             (t (cons (car x)
                                      (union (cdr x) y))))))))


(quote (Some Additional Functions for Basic PDP-1 LISP))


(rplacd (quote xsy)
        (quote (expr (lambda (x)
                       (prog (y)
                             (setq y oblist)
                          a  (cond ((null (cdr y)) (return nil))
                                   ((eq x (car (cdr y)))
                                    (return (rplacd y (cdr (cdr y))))))
                             (setq y (cdr y))
                             (go a))))))


(rplacd (quote remove)
        (quote (fexpr (lambda (x y)
                        (prog nil
                           a  (cond ((null x) (return oblist)))
                              (xsy (car x))
                              (setq x (cdr x))
                              (go a))))))


(rplacd (quote deposit)
        (quote (expr (lambda (x y)
                       (prog nil
                          a  (cond ((null x) (return y)))
                             (xeq (plus 240000 y) (car x) 0)
                             (setq x (cdr x))
                             (setq y (plus 1 y))
                             (go a))))))


(rplacd (quote putsubr)
        (quote (expr (lambda (n x y)
                       (prog nil
                             (rplacd n (list (quote subr)
                                             (plus 160000 y)))
                             (return (deposit x y)))))))


(rplacd (quote defsubr)
        (quote (expr (lambda (n x)
                       (rplacd n (list (quote subr)
                                       (plus 160000 x)))))))


(quote *EOF*)


