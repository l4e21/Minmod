(defpackage minmod
  (:use :cl :serapeum)
  (:export #:add-binding
           #:var-p
           #:var-opt-p
           #:or-p
           #:not-p
           #:match
           #:subst-bindings
           #:dispatch
           #:a-dispatch
           #:dispatch-fn
           #:match-fact
           #:transform-expression
           #:find-subexpressions
           #:find-transformations
           #:equivalence-transformations
           #:identity-statement-p
           #:direct-equivalences
           #:print-proof-trace
           #:add-proof-trace
           #:remove-traced-equivalences
           #:aux
           #:prove))

(in-package :minmod)

(-> add-binding (symbol t list) list)
(defun add-binding (key val bindings)
  "add a key-val pair to the association list of bindings"
  (cons (cons key val) bindings))

(-> var-p (t) boolean)
(defun var-p (sym)
  "?x or ?y are examples of bindings. Any sym starting with ?"
  (and (symbolp sym)
       (string= "?" (subseq (symbol-name sym) 0 1))))

(-> var-opt-p (t) boolean)
(defun var-opt-p (sym)
  "?x or ?y are examples of bindings. Any sym starting with ?"
  (and (symbolp sym)
       (string= "&" (subseq (symbol-name sym) 0 1))))

(-> or-p (t) boolean)
(defun or-p (sym)
  (and (symbolp sym)
       (string= "?OR" (symbol-name sym))))

(-> not-p (t) boolean)
(defun not-p (sym)
  (and (symbolp sym)
       (string= "?NOT" (symbol-name sym))))

(-> consistent-with-bindings (t t list) boolean)
(defun consistent-with-bindings (from to bindings)
  "Either no binding yet, or same as the desired"
  (let ((binding (assoc to bindings)))
    (or (not binding) (equal from (cdr binding)))))

(-> match (t t list) list)
(defun match (from to &optional (bindings '((t . t))))
  (assure list
    (cond
      ((not bindings)
       nil)

      ((equal from to)
       bindings)

      ((and (consp to))
       (cond
         ((var-opt-p (first to))
          (if (consistent-with-bindings from (first to) bindings)
              (add-binding (first to) from bindings)))
         
         ((or-p (first to))
          (reduce
           (lambda (acc to-match)
             (or acc (match from to-match bindings)))
           (rest to)
           :initial-value nil))
         
         ((consp from)
          (match (rest from) (rest to)
            (match (first from) (first to) bindings)))
         (t nil)))

      ((var-p to)
       (if (and from (consistent-with-bindings from to bindings))
           (add-binding to from bindings)
           nil))

      ((var-opt-p to)
       (if (and from (consistent-with-bindings from to bindings))
           (add-binding to from bindings)
           bindings))

      (t nil))))

(-> subst-bindings (t list) t)
(defun subst-bindings (expression bindings)
  (reduce (lambda (acc binding)
            (subst (cdr binding) (car binding) acc))
          bindings
          :initial-value expression))

(-> dispatch (t list) t)
(defun dispatch (expression pattern-list)
  (reduce (lambda (acc pair)
            (or acc
                (destructuring-bind (pattern dispatch-body)
                    pair
                  (let ((results (match expression pattern '((t . t)))))
                    (if results
                        (subst-bindings dispatch-body results)
                        nil)))))
          pattern-list
          :initial-value nil))

(defmacro a-dispatch (expression &body body)
  "Anaphoric pattern matching (bindings provided directly)"
  `(cond
     ,@(mapcar
        (lambda (clause)
          (destructuring-bind (pattern dispatch-body)
              clause
            (let ((results
                    (match expression `,pattern '((t . t)))))
              `(',results
                (let ((bindings ',results)
                      ,@(mapcar (lambda (binding)
                                  `(,(car binding) ',(cdr binding)))
                                (remove-if (lambda (result) (equal (car result) t)) results)))
                  
                  bindings
                  ,@(mapcar #'car results)
                  ,dispatch-body)))))
        body)))


(-> dispatch-fn (t list) t)
(defun dispatch-fn (expression pattern-list)
  (reduce (lambda (acc pair)
            (or acc
                (destructuring-bind (pattern dispatch-body)
                    pair
                  (let ((results (match expression pattern '((t . t)))))
                    (if results
                        (funcall dispatch-body results)
                        nil)))))
          pattern-list
          :initial-value nil))

(-> match-fact (t) t)
(defun match-fact (fact)
  (dispatch fact
   (list (list '(?left = ?right)
               '(?left ?right)))))


(-> transform-expression (t t) t)
(defun transform-expression (expr fact)
  (destructuring-bind (&optional fact-l fact-r)
      (match-fact fact)
    (dispatch-fn expr
                 (list
                  (list fact-l
                        (lambda (bindings)
                          (subst-bindings fact-r bindings)))
                  (list fact-r
                        (lambda (bindings)
                          (subst-bindings fact-l bindings)))))))

(defun find-subexpressions (expr)
  "Here is where you will need to provide operators that can be subexpressions for now"
  (dispatch-fn expr
               (list
                (list '(?x = ?y)
                      (lambda (bindings)
                        (append
                         (list (assocdr '?x bindings)
                               (assocdr '?y bindings))
                         (find-subexpressions (assocdr '?x bindings))
                         (find-subexpressions (assocdr '?y bindings)))))
                (list '(d ?x ?y)
                      (lambda (bindings)
                        (append
                         (list (assocdr '?x bindings)
                               (assocdr '?y bindings))
                         (find-subexpressions (assocdr '?x bindings))
                         (find-subexpressions (assocdr '?y bindings)))))
                (list '(?x - ?y)
                      (lambda (bindings)
                        (append
                         (list (assocdr '?x bindings)
                               (assocdr '?y bindings))
                         (find-subexpressions (assocdr '?x bindings))
                         (find-subexpressions (assocdr '?y bindings)))))
                (list '(?x + ?y)
                      (lambda (bindings)
                        (append
                         (list (assocdr '?x bindings)
                               (assocdr '?y bindings))
                         (find-subexpressions (assocdr '?x bindings))
                         (find-subexpressions (assocdr '?y bindings)))))

                (list '(?x * ?y)
                      (lambda (bindings)
                        (append
                         (list (assocdr '?x bindings)
                               (assocdr '?y bindings))
                         (find-subexpressions (assocdr '?x bindings))
                         (find-subexpressions (assocdr '?y bindings)))))
                (list '(?x ** ?y)
                      (lambda (bindings)
                        (append
                         (list (assocdr '?x bindings)
                               (assocdr '?y bindings))
                         (find-subexpressions (assocdr '?x bindings))
                         (find-subexpressions (assocdr '?y bindings))))))))


(-> find-transformations (t list) list)
(defun find-transformations (expr facts)
  (remove-if (lambda (transformation) (not (second transformation)))
          (mapcan 
           (lambda (subexpr)
             (mapcar (lambda (fact)
                       (list subexpr (transform-expression subexpr fact)))
                     facts))
           (append (list expr) (find-subexpressions expr)))))

(-> equivalence-transformations (t list) list)
(defun equivalence-transformations (eq-expr facts)
  (mapcan (lambda (expr)
            (find-transformations expr facts))
          (match-fact eq-expr)))

(-> identity-statement-p (t) boolean)
(defun identity-statement-p (eq-expr)
  (destructuring-bind (left right)
      (match-fact eq-expr)
    (equal left right)))

(-> direct-equivalences (t list) list)
(defun direct-equivalences (eq-expr equivalences)
  (destructuring-bind (left right)
      (match-fact eq-expr)
    (remove-if-not (lambda (equivalence)
                     (and (find left equivalence :test #'equal)
                          (find right equivalence :test #'equal)))
                   equivalences)))

(-> print-proof-trace (t) t)
(defun print-proof-trace (acc)
  (mapcar (lambda (trace) (pprint trace))
          acc)
  (first (last acc)))

(-> add-proof-trace (t list) list)
(defun add-proof-trace (expression equivalence acc)
  (append acc (list
               (list :expression expression
                     :from (first equivalence)
                     :to (second equivalence)
                     :rule 'todo))))

(-> aux (t list &optional list) list)
(defun aux (expr facts &optional (acc nil))
  (if (identity-statement-p expr)
      (add-proof-trace expr '(done done) acc)
      (reduce
       (lambda (acc2 equivalence)
         (let ((substitution (subst (second equivalence) (first equivalence) expr)))
           (if (or
                acc2
                (find (list :expression substitution
                            :from (second equivalence)
                            :to (first equivalence)
                            :rule 'todo)
                      acc :test #'equal)
                (find (list :expression expr
                            :from (first equivalence)
                            :to (second equivalence)
                            :rule 'todo)
                      acc :test #'equal))
               acc2
               
               (aux
                substitution
                facts
                (add-proof-trace expr equivalence acc)))))
       
       (equivalence-transformations expr facts)
       :initial-value nil)))

(-> prove (t list) list)
(defun prove (expr facts)
  (let ((result (aux expr facts)))
    (if result
        (~> result print-proof-trace))))

;; Ideas
;; Term re-writing
