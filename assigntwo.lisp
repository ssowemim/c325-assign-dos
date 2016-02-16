; Making initial contribution to assignment two in C325

(defun fl-interp (E P)
  (cond 
	((atom E) E)   %this includes the case where expr is nil
        (t
           (let ( (f (car E))  (arg (cdr E)) )
	      	(cond 
                ; handle built-in functions
                ((eq f 'first)  
                	(car (fl-interp (car arg) P))
                )
                ((eq f 'rest)
                	(cdr (fl-interp (car arg) P))
                )
                ((eq f 'atom)	
                	(atom (fl-interp (car arg) P))
                )
                ((eq f 'null)	
					(null (fl-interp (car arg) P))
				);j/9i
                ((eq f 'number)	
                	(number (fl-interp (car arg) P))
                )
                ((eq f 'abs)	
                	(abs (fl-interp (car arg) P))
                )
                ((eq f 'eq)	
                	(eq (fl-interp (car arg) P) (fl-interp (car(cdr arg) P)))
                )
                ((eq f 'equal)
                	(equal (fl-interp (car arg) P) (fl-interp (car(cdr arg) P)))
                )
                ((eq f 'append)
                	(append (fl-interp (car arg) P) (fl-interp (car(cdr arg) P)))
                )
                ((eq f 'cons) 
                	(append (fl-interp (car arg) P) (fl-interp (car(cdr arg) P)))
                )
                ((eq f 'quote)
                	(quote (fl-interp (car arg) P))
                )
                ((eq f 'mapcar)
                	(mapcar (fl-interp (car arg) P) (fl-interp (car(cdr arg) P)))
                )
                ((eq f reduce)
                	(reduce (fl-interp (car arg) P) (fl-interp (car(cdr arg) P)))
                )
                ((eq f ())


	        ..... 

	        ; if f is a user-defined function,
                ;    then evaluate the arguments 
                ;         and apply f to the evaluated arguments 
                ;             (applicative order reduction) 
                .....

                ; otherwise f is undefined; in this case,
                ; E is returned as if it is quoted in lisp
		
         	)

	    )

        )
    ) 
)
