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
				if(eq (isLn block) 7) (LnI block)
				(
					if(eq (isSin block) 8) (SinI block)
					(
						if(eq (isCos block) 9) (CosI block)
						(
							if(eq (isTan block) 10) (TanI block)
							(
								if(eq (isCot block) 11) (CotI block)
								
								(
								  if(eq (isSec block) 12) (SecI block)
								
								  nil
								)
							)
						)
					)
				)
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

(defun xpresent( seg )
	(
		if(null seg) nil
		(
			if(listp (car seg)) 
				(
					if(eq (xpresent (car seg)) nil) (xpresent (cdr seg))
					T
				)
			(
				if(eq (car seg) 'x) T
				(
					xpresent (cdr seg)
				)
			)
		)
	))
	
	
(defun isNilPresent (lis)
	(
		if(null lis) nil
		(
			if(null (caar lis)) T
			(isNilPresent (cdr lis))
		)
	))

(defun tillN1 (lis)
	(
		if(<= (length lis) 2) (car lis)
		( append (list (car lis)) (list (tillN1 (cdr lis))))
	))
	
;TODO e^x and ln x are not working
(defun starFun( str coun)
    (
		if(= coun 100) nil
		(
			if(eq (car str) '+) 
				(
					cons '+ (iterate (cdr str))
				)
			( 
				if(and (eq (car str) '*) (listp (second str)) (listp (third str)) )
					(
						if(and (eq t (xpresent (third str)) ) (null (xpresent (second str))) ) (list '* (second str) (integrate (third str) ))
						(
							if(and (eq t (xpresent (second str)) ) (null (xpresent (third str))) ) (list '* (third str) (integrate (second str) ))
							(
								if(and (null (xpresent (third str)) ) (null (xpresent (second str))) ) str
								(
									;UV rule!!
									;modify global parameter here
									if(eq (isNilPresent (starFun (list '* (starFun (tillN1 (cdr lis))) (diff (last list))) (+ 1 coun) ) ) T)
										(
											if(eq (isNilPresent (starFun (list '* (starFun (last lis)) (diff (tillN1 (cdr lis)))) (+ 1 coun) ) ) T) nil
											(
												starFun (list '* (starFun (last lis)) (diff (tillN1 (cdr lis)))) (+ 1 coun)
											)
										)
									(
										starFun (list '* (starFun (tillN1 (cdr lis))) (diff (last list))) (+ 1 coun)
									)
								)
							)
						)
					)
				(
				integrate  str
				)
			)
		)    
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
        -1
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
                    -1)
            
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

;====================================
        
(defun isLn( unit )
	(	
	if( eq (car unit) 'ln)
		( if( eq (second unit) 'x) 7
		-1
		)
		(
		;later expansion
		)
	)
)

(defun LnI(unit)
	(list '-  '* (second unit) 'ln (second unit) (second unit))
)


(defun isSin(unit)
	(	
	if(or (eq (car unit) 'sin) (eq (car unit) 'Sin))
		( if( eq (second unit) 'x) 
			      8
		(	
		  if(and (and (eq (second unit) '*) (eq (isNumber (list (third unit))) 4)) (eq (fourth unit) 'x)) 8
		  -1
		)
		)

		(
		;later expansion
		)

	)
)

(defun SinI(unit)
     (if (eq (length unit) 2)
	(list '-Cos (second unit))
	(list '* (third unit) '-Cos (fourth unit))
	)
)



(defun isCos(unit)
	(	
	if(or (eq (car unit) 'cos) (eq (car unit) 'Cos))
		( if( eq (second unit) 'x) 
		9
		(	
		  if(and (and (eq (second unit) '*) (eq (isNumber (list (third unit))) 4)) (eq (fourth unit) 'x)) 9
		  -1
		)
		)

		(
		;later expansion
		)

	)
)


(defun CosI(unit)
     (if (eq (length unit) 2)
	(list 'Sin (second unit))
	(list '* (third unit) 'Sin (fourth unit))
      )
)



(defun isTan(unit)
	(	
	if(or (eq (car unit) 'tan) (eq (car unit) 'Tan))
		( if( eq (second unit) 'x) 
		10
		(	
		  if(and (and (eq (second unit) '*) (eq (isNumber (list (third unit))) 4)) (eq (fourth unit) 'x)) 10
		  -1
		)
		)

		(
		;later expansion
		)

	)
)



(defun TanI(unit)
     (if (eq (length unit) 2)
	(list 'ln 'Sec (second unit) )	
	(list '* (third unit) 'ln 'Sec (fourth unit))
      )
)


(defun isCot(unit)
	(	
	if(or (eq (car unit) 'cot) (eq (car unit) 'Cot))
		( if( eq (second unit) 'x) 
		11
		(	
		  if(and (and (eq (second unit) '*) (eq (isNumber (list (third unit))) 4)) (eq (fourth unit) 'x)) 11
		  -1
		)
		)

		(
		;later expansion
		)

	)
)


(defun CotI(unit)
     (if (eq (length unit) 2)
	(list 'ln 'Sin (second unit))	
	(list '* (third unit) 'ln 'Sin (fourth unit))
      )
)




(defun isSec(unit)
	(	
	if(or (eq (car unit) 'sec) (eq (car unit) 'Sec))
		( if( eq (second unit) 'x) 
		12
		(	
		  if(and (and (eq (second unit) '*) (eq (isNumber (list (third unit))) 4)) (eq (fourth unit) 'x)) 12
		  -1
		)
		)

		(
		;later expansion
		)

	)
)


(defun SecI(unit)
     (if (eq (length unit) 2)
	(list 'ln '+ 'Sec (second unit) 'Tan (second unit) )	
	(list '* (third unit) 'ln '+ 'Sec (fourth unit) 'Tan (fourth unit) )
      )
)
