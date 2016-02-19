;Making initial contribution to assignment two in C325


(defun fl-interp (E P vars values)
   (interp E P nil nil)
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
	                	(car (fl-interp (car arg) P vars values))
	                )
	                ((eq f 'rest)
	                	(cdr (fl-interp (car arg) P vars values))
	                )
	                ((eq f 'atom)	
	                	(atom (fl-interp (car arg) P vars values))
	                )
	                ((eq f 'null)	
						(null (fl-interp (car arg) P vars values))
					)
	                ((eq f 'number)	
	                	(numberp (fl-interp (car arg) P vars values))
	                )
	                ((eq f 'abs)	
	                	(abs (fl-interp (car arg) P vars values))
	                )
	                ((eq f 'eq)	
	                	(eq (fl-interp (car arg) P vars values) (fl-interp (car(cdr arg) P vars values)))
	                )
	                ((eq f 'equal)
	                	(equal (fl-interp (car arg) P vars values) (fl-interp (car(cdr arg) P vars values)))
	                )
	                ((eq f 'append)
	                	(append (fl-interp (car arg) P vars values) (fl-interp (car(cdr arg) P vars values)))
	                )
	                ((eq f 'cons) 
	                	(append (fl-interp (car arg) P vars values) (fl-interp (car(cdr arg) P vars values)))
	                )
	                ((eq f 'quote)
	                	(quote (fl-interp (car arg) P vars values))
	                )
	                ((eq f 'mapcar)
	                	(mapcar (fl-interp (car arg) P vars values) (fl-interp (car(cdr arg) P vars values)))
	                )
	                ((eq f 'reduce)
	                	(reduce (fl-interp (car arg) P vars values) (fl-interp (car(cdr arg) P vars values)))
	                )
	                ((eq f '+)
	                	(+ (fl-interp (car arg) P vars values) (fl-interp (car(cdr arg) P vars values)))
	                )
	                ((eq f '-)
	                	(- (fl-interp (car arg) P vars values) (fl-interp (car(cdr arg) P vars values)))
	                )
	 				((eq f '*)
	                	(* (fl-interp (car arg) P vars values) (fl-interp (car(cdr arg) P vars values)))
	                )
	                ((eq f '/)
	                	(/ (fl-interp (car arg) P vars values) (fl-interp (car(cdr arg) P vars values)))
	                )
	                ((eq f '<)
	                	(< (fl-interp (car arg) P vars values) (fl-interp (car(cdr arg) P vars values)))
	                )
	                ((eq f '>)
	                	(> (fl-interp (car arg) P vars values) (fl-interp (car(cdr arg) P vars values)))
	                )
	                ((eq f '=)
	                	(= (fl-interp (car arg) P vars values) (fl-interp (car(cdr arg) P vars values)))
	                )
	                ((eq f 'and)
	                	(and (fl-interp (car arg) P vars values) (fl-interp (car(cdr arg) P vars values)))
	                )
	                ((eq f 'or)
	                	(or (fl-interp (car arg) P vars values) (fl-interp (car(cdr arg) P vars values)))
	                )
	               	((eq f 'not)
	                	(not (fl-interp (car arg) P vars values))
	                )

		        ; if f is a user-defined function,
	                ;    then evaluate the arguments 
	                ;         and apply f to the evaluated arguments 
	                ;             (applicative order reduction) 


	                ; otherwise f is undefined; in this case,
	                ; E is returned as if it is quoted in lisp

	                ;handles the situation for a user defined function
	                ((userDefined E P)
	                	(fl-interp (userDefined E P) 
	                		P 
	                		;evaluating the function and applying f to args
	                		;done in an applicative order reduction order
	                		(append (getVarsOfFunc E P) vars)
	                		(append (evalArgs arg P vars values) values))
	                	)

	                (T E)
		    	)
	        )
	    ) 
	)
)

(defun replaceVars (E vars values)
	(if (null vars) E
		(if (eq E (car vars))
			(car values)
			(replaceVars E (cdr vars) (cdr values))
		)
	)
)

(defun getVarsOfFunc (E P)
	(if (null P)
		nil
		(if (and (eq (car f) (car (car P)))
				(eq (countNum (car(cdr(car ))))
					(countNum (cdr E))
				)
			)
			(car (cdr (car P)))
			(getVarsOfFunc E (cdr P))
		)
	)
)

(defun userDefined (E P)
	(if (null P)
		nil
		(if (and (eq (car E) (car (car P)))
				(eq (countNum (car (cdr (car P)))) 
					(countNum (cdr E))
				)
			)
			(car (cdr (cdr (cdr (car P)))))
			(userDefined E (cdr P))
		)
	)
)

(defun evalArgs (L P vars values)
	(if (null L)
		nil
		(cons (interp (car L) P vars values) (evalArgs (cdr L) P vars values))
	)
)