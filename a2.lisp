#|  ASSIGNMENT TWO
    NAME: SONOLA SOWEMIMO (FEMI)
    CLASS: CMPUT 325
    ID: 1293430
|#

(defun countNum (args)
  ;  (cond
   ;     ((null args) 0)
    ;    (t (+ 1 (countNum (cdr args))))
   ; )

    (if (null args) 0
        (+ 1 (countNum(cdr args)))
    )
)

(defun findVariables (E values)
    (cond
        ((null values) E)
        ((equal E (caar values)) (cdar values))
        (t (findVariables E (cdr values)))
    )
)

(defun getValues (vars nums values)
;    (cond
;        ((null nums) values)
;        (t (cons (cons (car vars) (car nums)) (getValues (cdr vars) (cdr nums) values)))
 ;   )

    (if (null nums) values 
        (cons (cons (car vars) (car nums)) (getValues (cdr vars) (cdr nums) values))
    )
)

(defun findFunc (E P values)
    (cond
        ((null P) (findVariables E values))
        ((equal (caar P) E) (list (userDefinedArgs (cdar P)) (userDefinedBody (car P))))
        (t (findFunc E (cdr P) values))
    )
)

(defun userDefined (f E P)
    (cond
        ((null P) nil)
        ((and (equal (caar P) f) 
              (equal E (countNum (userDefinedArgs (cdar P))))) (list (userDefinedArgs (cdar P)) (userDefinedBody (car P))))
        (t (userDefined f E (cdr P)))
    )
)

(defun userDefinedBody (L)
   ; (cond
    ;    ((eq (car L) '=) (cadr L))
     ;   (t (userDefinedBody (cdr L)))
    ;)
    
    (if (eq (car L) '=) (cadr L)
        (userDefinedBody (cdr L))
    )

)

(defun userDefinedArgs (L)
;    (cond
 ;       ((eq (car L) '=) nil)
 ;       (t (cons (car L) (userDefinedArgs (cdr L))))
 ;   )

    (if (eq (car L) '=) nil
        (cons (car L) (userDefinedArgs (cdr L)))

    )
)

(defun evalArgs (args P values)
;    (cond
;        ((null args) nil)
;        (t (cons (interp (car args) P values) (evalArgs (cdr args) P values)))
;    )

    (if (null args) nil
        (cons (interp (car args) P values) (evalArgs (cdr args) P values))
    )
)

(defun interp (E P values)
    (cond
        ((atom E) (findFunc E P values))  ; this includes the case where expr is nil
        (t
            (let ((f (car E)) (arg (cdr E)))
                (cond
                    ; handle built-in functions
                    ((eq f 'first)  
                        (car (interp (car arg) P values))
                    )
                    ((eq f 'rest)  
                        (cdr (interp (car arg) P values))
                    )
                    ((eq f 'atom)  
                        (atom (interp (car arg) P values))
                    )
                    ((eq f 'null)  
                        (null (interp (car arg) P values))
                    )
                    ((eq f 'number)  
                        (numberp (interp (car arg) P values))
                    )
                    ((eq f 'abs)  
                        (abs (interp (car arg) P values))
                    )
                    ((eq f 'eq)  
                        (eq (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq f 'equal)  
                        (equal (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq f 'append) 
                        (append (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq f 'cons) 
                        (cons (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                   ; ((eq f 'quote) 
                    ;    (quote (interp (car arg) P values) (interp (cadr arg) P values))
                    ;)
                    ((eq f 'mapcar) 
                        (mapcar (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq f 'reduce) 
                        (reduce (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq f 'if) 
                        (if (interp (car arg) P values) (interp (cadr arg) P values) (interp (caddr arg) P values))
                    )
                    ((eq f '+)  
                        (+ (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq f '-)  
                        (- (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq f '*)  
                        (* (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq f '>)  
                        (> (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq f '/)  
                        (/ (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq f '<)  
                        (< (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq f '=)  
                        (= (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq f 'and)  
                        (and (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq f 'or) 
                        (or (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq f 'not)  
                        (not (interp (car arg) P values))
                    )
                    ;((eq f 'or)  (not (null (or (interp (car arg) P values) (interp (cadr arg) P values)))))
                   ; ((eq f 'not)  (not (interp (car arg) P values)))
                    ((eq f 'if) 
                        (if (interp (car arg) P values) (interp (cadr arg) P values) (interp (caddr arg) P values))
                    )

                    (t
                        (let
                            ((evalArgsVar (evalArgs arg P values))
                                (closure (userDefined f (countNum arg) P)))
                            (if closure
                                (let
                                    ((newValues (getValues (car closure) evalArgsVar values))
                                        (body (cadr closure)))
                                    (interp body P newValues) ; valueshange to a function-specific thing?
                                )
                                E
                            )
                        )
                    )
                )
            )
        )
    )
)

(defun fl-interp (E P)
    (interp E P NIL)
)