; Interpret an FL expression with respect to an FL program.
(defun fl-interp (expr program)
    (if (atom expr)
      expr
      (fl-eval (car expr) (cdr expr) program)))


; Interpret a list of FL expressions with respect to an FL program;
(defun fl-interp-all (expressions program)
  (if (null expressions)
    nil
    (cons
      (fl-interp (car expressions) program)
      (fl-interp-all (cdr expressions) program))))


; Attempt to apply a function named `fname` with argument list `args`.
;
; The function may be built in to the language, and it may be defined by the
; user in `program`.
;
; If neither, the expression is not a function application, and the original
; expression, (cons fname args) is returned.
(defun fl-eval (fname args program)
  (cond
    ;handle built in functions
    ((eq fname 'if) (fl-if args program))
    ((eq fname 'first) (fl-first args program))
    ((eq fname 'rest) (fl-rest args program))
    ((eq fname 'cons) (fl-cons args program))
    ((eq fname 'null) (fl-null args program))
    ((eq fname 'atom) (fl-atom args program))
    ((eq fname 'number) (fl-number args program))
    ((eq fname 'eq) (fl-eq args program))
    ((eq fname 'equal) (fl-equal args program))
    ((eq fname '+) (fl-+ args program))
    ((eq fname '-) (fl-- args program))
    ((eq fname '*) (fl-* args program))
    ((eq fname '>) (fl-> args program))
    ((eq fname '<) (fl-< args program))
    ((eq fname '=) (fl-= args program))
    ((eq fname 'and) (fl-and args program))
    ((eq fname 'or) (fl-or args program))
    ((eq fname 'not) (fl-not args program))

    ;handle user defined functions
    ((function-defined fname args program) (fl-interp
                                             (apply-function
                                               fname
                                               (fl-interp-all args program)
                                               program)
                                             program))

    ;fname is not a function - return expression reconstructed from function
    ;name and evaluated argument list
    (T (cons fname (fl-interp-all args program)))))


#|
|
| BUILT IN FUNCTIONS
|
||||||||#

(defun fl-if (args program)
  (if (fl-interp (car args) program)
    (fl-interp (cadr args) program)
    (fl-interp (caddr args) program)))

(defun fl-first (args program)
  (car (fl-interp (car args) program)))

(defun fl-rest (args program)
  (cdr (fl-interp (car args) program)))

(defun fl-cons (args program)
  (cons
    (fl-interp (car args) program)
    (fl-interp (cadr args) program)))

(defun fl-null (args program)
  (null (fl-interp (car args) program)))

(defun fl-atom (args program)
  (atom (fl-interp (car args) program)))

(defun fl-number (args program)
  (numberp (fl-interp (car args) program)))  

(defun fl-eq (args program)
  (eq 
    (fl-interp (car args) program)
    (fl-interp (cadr args) program)))

(defun fl-equal (args program)
  (equal
    (fl-interp (car args) program)
    (fl-interp (cadr args) program)))  

(defun fl-+ (args program)
  (+
    (fl-interp (car args) program)
    (fl-interp (cadr args) program)))  

(defun fl-- (args program)
  (-
    (fl-interp (car args) program)
    (fl-interp (cadr args) program)))  

(defun fl-* (args program)
  (*
    (fl-interp (car args) program)
    (fl-interp (cadr args) program)))  

(defun fl-> (args program)
  (>
    (fl-interp (car args) program)
    (fl-interp (cadr args) program)))  

(defun fl-< (args program)
  (<
    (fl-interp (car args) program)
    (fl-interp (cadr args) program)))  

(defun fl-= (args program)
  (=
    (fl-interp (car args) program)
    (fl-interp (cadr args) program)))  

(defun fl-and (args program)
  (if (fl-interp (car args) program)
    (if (fl-interp (cadr args) program)
      T
      nil)
    nil))  

(defun fl-or (args program)
  (if (fl-interp (car args) program)
    T
    (if (fl-interp (cadr args) program)
      T
      nil)))

(defun fl-not (args program)
  (not (fl-interp (car args) program)))


#|
|
| UTILITY FUNCTIONS
|
||||||||#


; Return true if the function exists in `program`.  The function is identified
; by name `fname` and argument list `args`.
(defun function-defined (fname args program)
  (cond
    ((null program) nil)
    ((signature-equal fname args (car program)) T)
    (T (function-defined fname args (cdr program)))))


; Return the function definition if the function exists in `program`.  The
; function is identified by name `fname` and argument list `args`.  The returned
; definition is in FL syntax.
(defun locate-definition (fname args program)
  (cond
    ((null program) nil)
    ((signature-equal fname args (car program)) (car program))
    (T (locate-definition fname args (cdr program)))))


; Return the result of applying the arguments in `args` to the function named
; `fname`, which is identified in the list of definitions in `program`.
(defun apply-function (fname args program)
    (substitute-args
      args
      (locate-definition fname args program)))


; Within the supplied definition, substitute each argument from the `args` list
; for the respective parameter in the definition.
(defun substitute-args (args definition)
    (applicative-reduce
      args
      (parse-params definition)
      (parse-body definition)))


; For each parameter in `params`, replace the parameter in `body` with its
; respective  argument from `args`.
(defun applicative-reduce (args params body)
  (if (null args)
    body
    (recursive-replace
      (car params)
      (car args)
      (applicative-reduce
        (cdr args) 
        (cdr params)
        body))))


; Recursively traverse the list `body` at all levels of nesting, replacing
; all occurences of `name` with `value`.
(defun recursive-replace (name value body)
  (cond
    ((null body) nil)
    ((eql name body) value)
    ((atom body) body)
    (T (cons
         (recursive-replace name value (car body))
         (recursive-replace name value (cdr body))))))


#|
|
| PARSING FUNCTIONS
|
||||||||#


; Return true iff the function identified by name `fname` and argument list
; `args` is the same as the one in the FL definition.
(defun signature-equal (fname args definition)
  (and 
    (eq fname (parse-fname definition))
    (= (size args) (size (parse-params definition)))))


; From the FL function definition, return the function's name.
(defun parse-fname (definition)
  (car definition))


; From the FL function definition, return the function's parameter list.
(defun parse-params (definition)
  (elements-before-= (cdr definition)))


; From the FL function definition, return the function's body expression.
(defun parse-body (definition)
  (car (elements-after-= definition)))


; From a list, return all elements that occur after the = symbol.
(defun elements-after-= (query-list)
  (reverse (elements-before-= (reverse query-list))))


; From a list, return all elements that occur before the = symbol.
(defun elements-before-= (query-list)
  (cond
    ((null query-list) nil)
    ((eq '= (car query-list)) nil)
    (T (cons (car query-list) (elements-before-= (cdr query-list))))))


; Return the number of elements in a list.
(defun size (query-list)
  (if (null query-list) 0 (+ 1 (size (cdr query-list)))))