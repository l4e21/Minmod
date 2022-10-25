(defpackage minmod/tests/main
  (:use :cl :minmod :fiveam)
  (:export #:debug! #:run! #:main-suite #:test-main-suite))

(in-package :minmod/tests/main)

(def-suite main-suite)
(in-suite main-suite)

(defun test-main-suite ()
  (debug! 'main-suite))

(test sanity
  (is (= (+ 1 1) 2)))

(test var-test
  (is (var-p '?x)))

(test var-opt-test
  (is (var-opt-p '&x)))

(test or-test
  (is (or-p '?or)))

(test not-test
  (is (not-p '?not)))

(test add-bindings
  (is (equal (add-binding '?x 3 nil)
             '((?x . 3)))))

(test match-atoms
  (is (match 'hi 'hi)))

(test match-variable
  (is (equal (match '(hi there) '(hi ?x))
             '((?x . there) (t . t)))))

(test match-simple-list
  (is (not (match '(hi there) '(hi thonk)))))

(test match-fail-list
  (is (not (match '(hi there) '(hi thonk)))))

(test match-variable-on-list
  (is (equal (match '(hi (there you)) '(hi ?x))
             '((?x . (there you)) (t . t)))))

(test match-&-variable
  (is (equal '((&x . (hi (there you))) (t . t))
             (match '(hi (there you)) '(&x)))))

(test match-or
  (is (equal '((?x . (there you)) (t . t))
             (match '(hi (there you)) '(hi (?or a ?x)))))
  (is (equal '((t . t))
             (match '(hi (there you)) '(hi (?or (there you) ?x))))))

(test subst-bindings
  (is (equal '(hi there)
             (subst-bindings
              '(?x there) '((?x . hi) (t . t))))))

(test dispatching
  (is (equal
       '(hi thar)
       (dispatch '(hi there) '(((?x there) (?x thar)))))))

(test proving
  (is (prove '((6 * 6) = (6 ** 2))
             '(((?x * ?x) = (?x ** 2))))))

(test proving-2
  (is (prove '((6 ** 6) = (6 * 6)) 
             '(((?x * ?x) = (?x ** 2)) 
               ((?x ** 3) = (?x ** 2)) 
               ((?x ** 4) = (?x ** 3)) 
               ((?x ** 4) = (?x ** 5)) 
               ((?x ** 6) = (?x ** 5))))))

(test proving-3
  (is (prove '((d (y / x)) = (d y / d x))
             '(((d ?y / d ?x) = (d (?y / ?x)))))))

(test proving-4
  (is (prove '((2 * (6 * 3)) = 36)
             '(((6 * 3) = 18) ((2 * 18) = 36)))))

(test proving-commutativity
  (is (prove '((2 * 3) = (3 * 2))
             '(((?x * ?y) = (?y * ?x))))))

(test proving-commutativity-2
  (is (prove '((2 * (4 * 3)) = (4 * (3 * 2)))
             '(((?x * ?y) = (?y * ?x))
               ((?x * (?y * ?z)) = (?y * (?x * ?z)))))))

(test stress-test
  (is (prove '((2 * 5 * (4 * 3)) = (4 * 5 * (3 * 2)))
             '(((?x * ?y) = (?y * ?x))
               ((?x * (?y * ?z)) = (?y * (?x * ?z)))
               ((?x * (?y * ?z)) = (?x * ?y * ?z))))))

(test derivatives
  (is (prove
       '((d ((3 * x) ** 5) x) = (15 * (x ** 4))) 
       '(((d ((?a * ?x) ** ?n) ?x) = ((?a * ?n) * (?x ** (?n - 1))))
         ((3 * 5) = 15)
         ((5 - 1) = 4)
         ((?x * ?y) = (?y * ?x))))))

;; (test stressier (is (prove
;;                      '((x * (y * (d * (z * (a * (b * c)))))) = (a * (b * (c * d * (z * (y * x))))))
;;                      '(((?x * ?y) = (?y * ?x))
;;                        ((?x * (?y * ?z)) = (?y * (?x * ?z)))))))
