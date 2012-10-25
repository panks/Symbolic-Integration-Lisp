(load 'diff.lisp)
    
(defun integrate( block )
    (
        if(eq (listp block) nil) (integrate (list block))
        (
			if(eq (isNumber block) 4) (NumberI block)
			(
				if(eq (isSingle block) 1) (SingleI block)
				(
					if(eq (isPower block) 2) (PowerI block)
					(
						if(eq (isMult block) 3) (MultI block)
						(
							if(eq (isInverse block) 5) (InverseI block)
							(
								if(eq (isExp block) 6) (ExpI block)
								(
									print "error"
								)
							)
						)
					)
				)
			)
        )
    ))

(defun isSimple( block )
    (
        
        if(eq (isNumber block) 4) 1
        (
            if(eq (isSingle block) 1) 1
            (
                if(eq (isPower block) 2) 1
                (
                    if(eq (isMult block) 3) 1
                    (
                        if(eq (isInverse block) 5) 1
                        (
                            if(eq (isExp block) 6) 1
                            0
                        )
                    )
                )
            )
        )
    ))

; trying to do UV rule and substitution
#|
(defun integrateRules ( seg num)
    (
		;(setf left 0)
        ;assuming only 2 elements in the product..
        if(eq num 100) nil
        (
            if(eq (isSimple seg) 1) 0
            (
				if(null (last ( )) )
            )
        )
    )) 


(defun pathFinder (seg def start)
	(
		if(null seg) (print 'No integral')
		(
			if(eq (car seg) 1) ( append ( list '* (integrate (tillN1 def)) (last def)) (list '* (* -1 start ) ( parhFinder (cdr seg) (diff  ) (* -1 start))))
		)
	))
	
|#	
(defun tillN1 (lis)
	(
		if(<= (length lis) 2) (car lis)
		( append (list (car lis)) (list (tillN1 (cdr lis))))
	))
	
;TODO e^x and ln x are not working
(defun starFun( str )
    (
        if(eq (car str) '+) 
        (
            cons '+ (iterate (cdr str))
        )
        ( integrate  str)
    ))

(defun iterate(block)
    (
        if(eq (length block) 1)  (list (integrate (car block)))
        (;print block);
        cons (integrate (car block)) (iterate (cdr block)))
    ))

(defun isNumber( unit )
    (
        if(numberp (car unit)) 4            
        (
            print "error"    
        )
    ))
    
(defun NumberI( unit )
    (
        list '* (car unit) 'x
    ))    

    
(defun isSingle( unit )
    (
        if(eq (car unit) 'x) 1            
        10
    ))    

(defun SingleI ( unit )
    (
        list '* 0.5 (list '^ 'x 2)
    ))
    
( defun isPower( unit )
    (
        if(eq (car unit) '^)
            ( if(eq (second unit) 'x)
                (   if(numberp (third unit)) 2
                    (print "error"))
            
               ;later expansion
               20
            )
        (;later expansion
        )
    ))

( defun PowerI (unit)
    (
        list '* (list '/ 1 (+ (third unit) 1) ) (list '^ 'x (+ (third unit) 1)) 
    ))
    
( defun isMult( unit )  
    (
        if(eq (car unit) '*)
            ( if(numberp (second unit))
                (   if(eq (third unit) 'x) 3
                    30
                    )
            (
               ;later expansion
            ))
        (;later expansion
        )
    )) 
    
(defun MultI (unit )
    (
         if(listp (third unit))  (list (first unit) (second unit) (integrate (third unit)))
         (list (first unit) (second unit) (integrate (list (third unit))))
    )) 
    
( defun isInverse( unit )
    (
        if(eq (car unit) '/)
            ( if(numberp (second unit))
                (   if(eq (third unit) 'x) 5
                    50)
            (
               ;later expansion
            ))
        (;later expansion
        )
    ))

( defun InverseI (unit)
    (
        list '* (second unit) 'log 'x
    ))
    
( defun isExp( unit )
    (
        if(eq (car unit) '^)
            ( if(eq (second unit) 'e)
                (   if(eq (third unit) 'x) 6
                    60)
            (
               ;later expansion
            ))
        (;later expansion
        )
    ))

( defun ExpI (unit)
    (
        list '^ 'exp 'x
    ))    


        