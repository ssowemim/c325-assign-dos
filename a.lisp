;Making initial contribution to assignment two in C325

(defun countNum (args) ;get_arity
;	(if (null args)
;		0
;		(+ 1 (countNum (cdr args)))
;	)

	(cond 
		((null args) 0)
		(t (+ 1 (countNum (cdr args))))
	)
)

(defun getVarsOfFunc (E P)
	(if (null P)
		nil
		(if (and (eq (car E) (car (car P)))
				(eq (countNum (car(cdr(car P))))
					(countNum (cdr E))
				)
			)
			(car (cdr (car P)))
			(getVarsOfFunc E (cdr P))
		)
	)
)

(defun userDefined (f E P)
	(if (null P)
		nil
		(if (and (equal f (car (car P)))
				 (equal (countNum (car (cdr (car P)))) 
					(countNum (cdr E))
				 )
			)
			(car (cdr (cdr (cdr (car P)))))
			(userDefined f E (cdr P))
		)
	)
)

(defun replaceVars (E vars values)
	(if (null vars) E
		(if (equal E (car vars))
			(car values)
			(replaceVars E (cdr vars) (cdr values))
		)
	)

;(cond 
;	((null vars) (getVarsOfFunc E values))
;	((equal (caar P) E) (list ())
;)

)

(defun interp (E P vars values)
	(cond 
		((atom E) (replaceVars E vars values))   ;this includes the case where expr is nil
	   	(t
	        (let ( (f (car E))  (arg (cdr E)) )
			    (cond 
		            ;handle built-in functions
		            ;prmitive functions
		            ((eq f 'first)  
		           		(car (interp (car arg) P vars values))
		            )
	                ((eq f 'rest)
	                	(cdr (interp (car arg) P vars values))
	                )
	                ((eq f 'atom)	
	                	(atom (interp (car arg) P vars values))
	                )
	                ((eq f 'null)	
						(null (interp (car arg) P vars values))
					)
	                ((eq f 'number)	
	                	(numberp (interp (car arg) P vars values))
	                )
	                ((eq f 'abs)	
	                	(abs (interp (car arg) P vars values))
	                )
	                ((eq f 'eq)	
	                	(eq (interp (car arg) P vars values) (interp (car(cdr arg)) P vars values))
	                )
	                ((eq f 'equal)
	                	(equal (interp (car arg) P vars values) (interp (car(cdr arg)) P vars values))
	                )
	                ((eq f 'append)
	                	(append (interp (car arg) P vars values) (interp (car(cdr arg)) P vars values))
	                )
	                ((eq f 'cons) 
	                	(append (interp (car arg) P vars values) (interp (car(cdr arg)) P vars values))
	                )
	                ((eq f 'quote)
	                	(quote (interp (car arg) P vars values))
	                )
	                ((eq f 'mapcar)
	                	(mapcar (interp (car arg) P vars values) (interp (car(cdr arg)) P vars values))
	                )
	                ((eq f 'reduce)
	                	(reduce (interp (car arg) P vars values) (interp (car(cdr arg)) P vars values))
	                )
	                ((eq f '+)
	                	(+ (interp (car arg) P vars values) (interp (car(cdr arg)) P vars values))
	                )
	                ((eq f '-)
	                	(- (interp (car arg) P vars values) (interp (car(cdr arg)) P vars values))
	                )
	 				((eq f '*)
	                	(* (interp (car arg) P vars values) (interp (car(cdr arg)) P vars values))
	                )
	                ((eq f '/)
	                	(/ (interp (car arg) P vars values) (interp (car(cdr arg)) P vars values))
	                )
	                ((eq f '<)
	                	(< (interp (car arg) P vars values) (interp (car(cdr arg)) P vars values))
	                )
	                ((eq f '>)
	                	(> (interp (car arg) P vars values) (interp (car(cdr arg)) P vars values))
	                )
	                ((eq f '=)
	                	(= (interp (car arg) P vars values) (interp (car(cdr arg)) P vars values))
	                )
	                ((eq f 'and)
	                	(and (interp (car arg) P vars values) (interp (car(cdr arg)) P vars values))
	                )
	                ((eq f 'or)
	                	(or (interp (car arg) P vars values) (interp (car(cdr arg)) P vars values))
	                )
	               	((eq f 'not)
	                	(not (interp (car arg) P vars values))
	                )
	                ((eq f 'if) 
	                	(if (interp (car arg) P vars values) (interp (car (cdr arg)) P vars values)
	                		(interp (car (cdr(cdr arg))) P vars values)
	                	)
	                )

		        	; if f is a user-defined function,
	                ;    then evaluate the arguments 
	                ;         and apply f to the evaluated arguments 
	                ;             (applicative order reduction) 


	                ; otherwise f is undefined; in this case,
	                ; E is returned as if it is quoted in lisp

	                ;handles the situation for a user defined function
	                ((userDefined f E P)
	                	(interp (userDefined f E P) 
	                		P 
	                		;evaluating the function and applying f to args
	                		;done in an applicative order reduction order
	                		(append (getVarsOfFunc E P) vars)
	                		(append (evalArgs arg P vars values) values)
	             		)
	                )(t E)
	           	)
			)
	    )
	)
)

(defun evalArgs (arg P vars values) ;eval_args
	(if (null arg)
		nil
		(cons (interp (car arg) P vars values) (evalArgs (cdr arg) P vars values))
	)
)

(defun fl-interp (E P)

	(interp E P nil nil)
)
