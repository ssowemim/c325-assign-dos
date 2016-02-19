(defun find_var_in_context (E C)
    (cond
        ((null C) E)
        ((equal E (caar C)) (cdar C))
        (t (find_var_in_context E (cdr C)))
    )
)

(defun find_func_in_program (E P C)
    (cond
        ((null P) (find_var_in_context E C))
        ((equal (caar P) E) (list (get_fnargs (cdar P)) (get_fnbody (car P))))
        (t (find_func_in_program E (cdr P) C))
    )
)

(defun get_func (f arity P)
    (cond
        ((null P) nil)
        ((and (equal (caar P) f) (equal arity (get_arity (get_fnargs (cdar P))))) (list (get_fnargs (cdar P)) (get_fnbody (car P))))
        (t (get_func f arity (cdr P)))
    )
)

(defun get_fnbody (L)
    (cond
        ((eq (car L) '=) (cadr L))
        (t (get_fnbody (cdr L)))
    )
)

(defun get_fnargs (L)
    (cond
        ((eq (car L) '=) nil)
        (t (cons (car L) (get_fnargs (cdr L))))
    )
)

(defun fl-interp (E P)
    (_fl-interp E P NIL)
)

(defun _fl-interp (E P C)
    (cond
        ((atom E) (find_func_in_program E P C))  ; this includes the case where expr is nil
        (t
            (let ((f (car E)) (arg (cdr E)))
                (cond
                    ; handle built-in functions
                    ((eq f 'if) (if (_fl-interp (car arg) P C) (_fl-interp (cadr arg) P C) (_fl-interp (caddr arg) P C)))
                    ((eq f 'null)  (null (_fl-interp (car arg) P C)))
                    ((eq f 'atom)  (atom (_fl-interp (car arg) P C)))
                    ((eq f 'eq)  (eq (_fl-interp (car arg) P C) (_fl-interp (cadr arg) P C)))
                    ((eq f 'first)  (car (_fl-interp (car arg) P C)))
                    ((eq f 'rest)  (cdr (_fl-interp (car arg) P C)))
                    ((eq f 'cons) (cons (_fl-interp (car arg) P C) (_fl-interp (cadr arg) P C)))
                    ((eq f 'equal)  (equal (_fl-interp (car arg) P C) (_fl-interp (cadr arg) P C)))
                    ((eq f 'number)  (numberp (_fl-interp (car arg) P C)))
                    ((eq f '+)  (+ (_fl-interp (car arg) P C) (_fl-interp (cadr arg) P C)))
                    ((eq f '-)  (- (_fl-interp (car arg) P C) (_fl-interp (cadr arg) P C)))
                    ((eq f '*)  (* (_fl-interp (car arg) P C) (_fl-interp (cadr arg) P C)))
                    ((eq f '>)  (> (_fl-interp (car arg) P C) (_fl-interp (cadr arg) P C)))
                    ((eq f '<)  (< (_fl-interp (car arg) P C) (_fl-interp (cadr arg) P C)))
                    ((eq f '=)  (= (_fl-interp (car arg) P C) (_fl-interp (cadr arg) P C)))
                    ((eq f 'and)  (not (null (and (_fl-interp (car arg) P C) (_fl-interp (cadr arg) P C)))))
                    ((eq f 'or)  (not (null (or (_fl-interp (car arg) P C) (_fl-interp (cadr arg) P C)))))
                    ((eq f 'not)  (not (_fl-interp (car arg) P C)))

                    (t
                        (let
                            ((ev_args (eval_args arg P C))
                                (closure (get_func f (get_arity arg) P)))
                            (if closure
                                (let
                                    ((new_context (get_context (car closure) ev_args C))
                                        (body (cadr closure)))
                                    (_fl-interp body P new_context) ; Change to a function-specific thing?
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

(defun get_arity (args)
    (cond
        ((null args) 0)
        (t (+ 1 (get_arity (cdr args))))
    )
)

(defun get_context (vars nums C)
    (cond
        ((null nums) C)
        (t (cons (cons (car vars) (car nums)) (get_context (cdr vars) (cdr nums) C)))
    )
)

(defun eval_args (args P C)
    (cond
        ((null args) nil)
        (t (cons (_fl-interp (car args) P C) (eval_args (cdr args) P C)))
    )
)