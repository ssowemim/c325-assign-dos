#|  ASSIGNMENT TWO
    NAME: SONOLA SOWEMIMO (FEMI)
    CLASS: CMPUT 325
    ID: 1293430
|#

#|
    This simple functions is an adding function,
    main primary is to count the number of arguments.
|#
(defun countNum (args)

    (if (null args) 
        ;first argument
        0
        ;second argument
        (+ 1 (countNum(cdr args)))
    )
)

#|
    This function is in charge of parsing through E
    with possible values.
|#
(defun findVariables (E values)
    (cond
        ((null values) 
            E
        )
        ((equal E (car (car values))) 
            (cdr (car values))
        )
        (t 
            (findVariables E (cdr values))
        )
    )
)

#|
    process of assigning values to variables
|#
(defun getValues (vars nums values)
    (if (null nums) 
        ; first argument
        values 
        ; second argument
        (cons (cons (car vars) (car nums)) (getValues (cdr vars) (cdr nums) values))
    )
)

(defun findFunc (E P values)
    (cond
        ((null P) 
            (findVariables E values)
        )
        ((equal (car (car P)) E) 
            (list (userDefinedArgs (cdar P)) (userDefinedBody (car P)))
        )
        (t 
            (findFunc E (cdr P) values)
        )
    )
)

#|
    Handling the user defined function body
|#
(defun userDefinedBody (L)
    
    (if (eq (car L) '=) 
        ;first argument
        nil
        ;second argument
        (car (cdr (L)) (userDefinedBody (cdr L)))
    )
)

#|
    Handling the user defined function arguments
|#
(defun userDefinedArgs (L)

    (if (eq (car L) '=) 
        ;first argument
        nil
        ;second argument
        (cons (car L) (userDefinedArgs (cdr L)))

    )
)

(defun userDefined (func E P)
    (cond
        ((null P) 
            nil
        )
        ((and 
            (equal (car (car P)) func) 
            (equal E (countNum (userDefinedArgs (cdr (car P)))))
        ) 
            (list (userDefinedArgs (cdr (car P))) (userDefinedBody (car P)))
        )
        (t 
            (userDefined func E (cdr P))
        )
    )
)


(defun evalArgs (args P values)

    (if (null args) 
        ;first argument
        nil
        ;second argument
        (cons (interp (car args) P values) (evalArgs (cdr args) P values))
    )
)

#|
    Arguably the main function, handles all the primitive functions - functions that are 
    already implemented by lisp already and also those functions that are defined by users.

    That function then ends up calling the other functions that aid in solving the function.
|#

(defun interp (E P values)
    (cond
        ((atom E) (findFunc E P values))
        (t
            (let ((func (car E)) (arg (cdr E)))
                (cond
                    
                    ((eq func 'first)  
                        (car (interp (car arg) P values))
                    )
                    ((eq func 'rest)  
                        (cdr (interp (car arg) P values))
                    )
                    ((eq func 'atom)  
                        (atom (interp (car arg) P values))
                    )
                    ((eq func 'null)  
                        (null (interp (car arg) P values))
                    )
                    ((eq func 'number)  
                        (numberp (interp (car arg) P values))
                    )
                    ((eq func 'abs)  
                        (abs (interp (car arg) P values))
                    )
                    ((eq func 'eq)  
                        (eq (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq func 'equal)  
                        (equal (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq func 'append) 
                        (append (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq func 'cons) 
                        (cons (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq func 'mapcar) 
                        (mapcar (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq func 'reduce) 
                        (reduce (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq func 'if) 
                        (if (interp (car arg) P values) (interp (cadr arg) P values) (interp (caddr arg) P values))
                    )
                    ((eq func '+)  
                        (+ (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq func '-)  
                        (- (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq func '*)  
                        (* (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq func '>)  
                        (> (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq func '/)  
                        (/ (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq func '<)  
                        (< (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq func '=)  
                        (= (interp (car arg) P values) (interp (cadr arg) P values))
                    )
                    ((eq func 'and)  
                        (not
                            (null
                                (and (interp (car arg) P values) (interp (cad (car arg)) P values))
                            )
                        )
                    )
                    ((eq func 'or) 
                        (not 
                            (null
                                (or (interp (car arg) P values) (interp (cad (car arg)) P values))
                            )
                        )
                    )
                    ((eq func 'not)  
                        (not (interp (car arg) P values))
                    )

                    ((eq func 'if) 
                        (if (interp (car arg) P values) (interp (cadr arg) P values) (interp (caddr arg) P values))
                    )

                    (t
                        (let
                            ((variableX (evalArgs arg P values))
                                (closure (userDefined func (countNum arg) P)))
                            (if closure
                                (let
                                    ((variableY (getValues (car closure) variableX values))
                                        (body (cad (car closure))))
                                    (interp body P variableY)
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

#|
    The main function that is called, this then in turn calls the secondary function that handles
    all the command arguments. Also passing in the values arguments. Initially this is nil.
|#
(defun fl-interp (E P)
    (interp E P NIL)
)