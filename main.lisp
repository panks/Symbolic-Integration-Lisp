(load 'diff.lisp)

(defun ^(a b)
    (
        if(<= b 0) 1
        (
            * a (^ a (- b 1))
        )
    ))

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
								
								    (
								      if(eq (isCosec block) 13) (CosecI block)
								
				(
					if(eq (isSinh block) 14) (SinhI block)
					(
						if(eq (isCosh block) 15) (CoshI block)
						(
							if(eq (isTanh block) 16) (TanhI block)
							(
								if(eq (isCoth block) 17) (CothI block)
								
								(
								  if(eq (isSech block) 18) (SechI block)
								
								    (
								     ; if(eq (isCosech block) 19) (CosechI block)
									 ; (
									;	  if(eq (isExpow block) 20) (ExpowI block)
										  nil
									 ; )		
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

(defun valpresent( seg val)
	(
		if(null seg) nil
		(
			if(listp (car seg)) 
				(
					if(eq (valpresent (car seg) val) nil) (valpresent (cdr seg) val)
					T
				)
			(
				if(eq (car seg) val) T
				(
					valpresent (cdr seg) val
				)
			)
		)
	))
	
(defun xpresent (seg)
    (
        valpresent seg 'x
    ))
	
(defun isNilPresent (seg)
	( ;not working!!
	#|
		if(null lis) nil
		(
			if(null (caar lis)) T
			(isNilPresent (cdr lis))
		)
		|#
		
		if(null seg) nil
		(
			if(listp (car seg)) 
				(
				    if(eq (car seg) nil) T
				    (
					    if(eq (isNilPresent (car seg)) nil) (isNilPresent (cdr seg))
					    T
					)
				)
			(
				if(eq (car seg) nil) T
				(
					isNilPresent (cdr seg)
				)
			)
		)
		
		;valpresent lis nil
	))

(defun tillN1 (lis)
	(
		if(<= (length lis) 2) (list (car lis))
		( append (list (car lis))  (tillN1 (cdr lis)))
	))
	
(defun integratetop (func)
	(setf intVal (cons 'C (list (starFun func 0))))
    (
		;print intVal
		if( isNilPresent intVal) nil
        (cons '+ intVal)
    ))

(defun searchpattern ( func deri)
	(
		if(null func) nil
		(
			if(equal func deri) T
			(
				if(listp (car func)) 
					(
						if(null (searchpattern (car func) deri)) 
							(
								if(equal (car func) deri) T
								(
									searchpattern (cdr func) deri
								)
							)
							T
					)
				(
					searchpattern (cdr func) deri
				)
			)
		)
	))
    
(defun searchAndReplace ( func deri val)
	;(setf state 0)
	(
		if(null func) func
		(
			if(equal func deri) 
				(
					if(listp deri)
						(
							if(listp val) val
							(list val)
						)
					val	
				)
			(
				if(listp func)
				(	
					if(listp (searchAndReplace (car func) deri val))
					(	
						if(eq (length (searchAndReplace (car func) deri val)) 1)
							(
								cons (car (searchAndReplace (car func) deri val)) (searchAndReplace (cdr func) deri val)
							)
						(cons (searchAndReplace (car func) deri val) (searchAndReplace (cdr func) deri val))
					)
					(cons (searchAndReplace (car func) deri val) (searchAndReplace (cdr func) deri val))
				)
				func
			)
		)
	))

(defun substitution	(func deri)
	(setf intd (starFun deri 0))
	(
		if(null (searchpattern func intd)) nil
		(
			if(null (xpresent (searchAndReplace func intd 'z)))
				(
					searchAndReplace (starFun (searchAndReplace (searchAndReplace func intd 'z) 'z 'x) 0) 'x intd
				)
			nil	
		)
	))
	
(defun starFun( str coun)
    (
        if(eq (listp str) nil) (starFun  (list str) coun)
        (
		    if(= coun 20) nil
		    (
			    if(eq (car str) '+) 
				    (
					     iterate (cdr str)
				    )
			    ( 
				    if(and (eq (car str) '*) (listp (second str)) (listp (third str)) );use mapcar for checking on all
					    (
						    if(and (eq t (xpresent (third str)) ) (null (xpresent (second str))) ) (list '* (second str) (starFun (third str) 0) )
						    (
						
							    if(and (eq t (xpresent (second str)) ) (null (xpresent (third str))) ) (list '* (third str) (starFun (second str) 0))
							    (
								    if(and (null (xpresent (third str)) ) (null (xpresent (second str))) ) (list '* str 'x)
								    (
										; adding the rules for substitution here
										if(null (substitution (third str) (second str)))
											(
												if(null (substitution (second str) (third str)))
												(
											
													;UV rule!!
													;modify global parameter here
													if(eq (isNilPresent (starFun (list '* (starFun (car (tillN1 (cdr str))) (+ 1 coun)) (diff  (car(last str)))) (+ 1 coun) ) ) T)
														( ;print "2"
															if(eq (isNilPresent (starFun (list '* (starFun (car (last str)) (+ 1 coun)) (diff (car (tillN1 (cdr str))) )) (+ 1 coun) ) ) T) nil
															(
																list '+ (list '* (starFun (car (last str)) (+ 1 coun)) (car (tillN1  (cdr str) )) )  (list '* (list (* -1 (^ -1 (mod coun 2)))) ( starFun (list '* (starFun (car (last str )) (+ 1 coun))  (diff (car (tillN1 (cdr str))))) (+ 1 coun) ))
															)
														
														)
													( ;print "1"
														;list '* (starFun (tillN1  str) 0) (car (last str) ) 
														list '+  (list '* (starFun (car (tillN1  (cdr str) ) ) (+ 1 coun)) (car (last str) ) ) (list '* (list (* -1 (^ -1 (mod coun 2)))) (starFun (list '* (starFun (car (tillN1  (cdr str))) (+ 1 coun)) (diff (car (last str))) ) (+ 1 coun)))
													)
												)
												(substitution (second str) (third str))
											)
											(substitution (third str) (second str))
								    )
							    )
						    )
					    )
				    (
				    integrate  str
				    )
			    )
		    ) 
	    ) 
    ))

(defun iterate(block)
    (
        if(eq (length block) 1)   (list (starFun (car block) 0))
        (
        cons (starFun (car block) 0) (iterate (cdr block)))
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
	    if (
	    and 
	    (eq (car unit) '*) 
	    (listp (second unit)) 
	    (numberp (car (second unit))) 
	    (eq (first (third unit)) '^) 
	    (eq (second (third unit)) 'x) 
	    (numberp (third (third unit)))
	    ) 2
	    -1	  
        )
    ))

( defun PowerI (unit)
    (if (listp (third unit))
    
    (
	list '* (list '/ (car (second unit)) (+ (third (third unit)) 1) ) (list '^ 'x (+ (third (third unit)) 1))
    )
    (
        list '* (list '/ 1 (+ (third unit) 1) ) (list '^ 'x (+ (third unit) 1)) 
    )
    
    )
    
    )
    
( defun isMult( unit )  
    (
        if(eq (car unit) '*)
            ( if(or (numberp (second unit)) (and (eq (length (second unit)) 1) (numberp (car (second unit))) ))
                (   if(eq (third unit) 'x) 3
                    (
                        if(listp (third unit)) 3
                        -1
                        ;list '* (second unit) (top (car (cddr unit)))
                    )
                )
            (
               ;later
            ))
        (;later expansion
        )
    )) 
    
(defun MultI (unit )
    (
         if(listp (third unit))  (list (first unit) (second unit) (starFun (third unit) 0))
         (list (first unit) (second unit)  (starFun (third unit) 0))
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
        list '^ 'e 'x
    ))    

(defun isExpow (unit)
	(
		if(eq (car unit) '^)
            ( if(null (xpresent (second unit)))
                (   if(eq (third unit) 'x) 20
                    60)
            (
               ;later expansion
            ))
        (;later expansion
        )
	))

(defun ExpowI (unit)
	(
		list '/ (list '^ (list (second unit)) x) (list (second unit))
	))	
	
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
	(list '-  (list '* (list (second unit)) (list 'ln (second unit))) (list (second unit)))
)


(defun isSin(unit)
	(	
	if(or (eq (car unit) 'sin) (eq (car unit) 'Sin))
		( if( eq (second unit) 'x) 
			      8
		(	
		 ; if(and (eq (second unit) '*) (eq (isNumber (list (third unit))) 4) (eq (fourth unit) 'x)) 8
		  if (and
		  (listp (second  unit))
		  (eq (first (second unit)) '*)
		  (numberp (second (second unit)))
		  (eq (third (second unit)) 'x)
		  )8
		  -1
		)
		)

		(
		;later expansion
		)

	)
)

(defun SinI(unit)
     (if (listp (second unit))
	  (list '* (list '/ -1 (second (second unit))) (list 'Cos (list '* (second (second unit)) (third (second unit)))) )
	(list '*  (list -1) (list 'Cos (second unit)) )
	
	)
)



(defun isCos(unit)
	(	
	if(or (eq (car unit) 'cos) (eq (car unit) 'Cos))
		( if( eq (second unit) 'x) 
		9
		(
		
		  if (and
		  (listp (second  unit))
		  (eq (first (second unit)) '*)
		  (numberp (second (second unit)))
		  (eq (third (second unit)) 'x)
		  )9
		  -1
		)
		)

		(
		;later expansion
		)

	)
)


(defun CosI(unit)
     (if (listp (second unit))
	(list '* (list '/ 1 (second (second unit))) (list 'sin (list '* (second (second unit)) (third (second unit)))) )
	(list 'Sin (second unit))
	
      )
)



(defun isTan(unit)
	(	
	if(or (eq (car unit) 'tan) (eq (car unit) 'Tan))
		( if( eq (second unit) 'x) 
		10
		(	
		  if (and
		  (listp (second  unit))
		  (eq (first (second unit)) '*)
		  (numberp (second (second unit)))
		  (eq (third (second unit)) 'x)
		  )10
		  -1
		)
		)

		(
		;later expansion
		)

	)
)



(defun TanI(unit)
     (if (listp (second unit))
	(list '* (list '/ 1 (second (second unit))) (list 'ln 'Sec (list '* (second (second unit)) (third (second unit)))) )
	(list 'ln (list 'Sec (second unit) ))
	
      )
)


(defun isCot(unit)
	(	
	if(or (eq (car unit) 'cot) (eq (car unit) 'Cot))
		( if( eq (second unit) 'x) 
		11
		(	
		  if (and
		  (listp (second  unit))
		  (eq (first (second unit)) '*)
		  (numberp (second (second unit)))
		  (eq (third (second unit)) 'x)
		  )11
		  -1
		)
		)

		(
		;later expansion
		)

	)
)


(defun CotI(unit)
     (if (listp (second unit))
	(list '* (list '/ 1 (second (second unit))) (list 'ln 'Sin (list '* (second (second unit)) (third (second unit)))) )
	(list 'ln (list 'Sin (second unit) ))	

      )
)




(defun isSec(unit)
	(	
	if(or (eq (car unit) 'sec) (eq (car unit) 'Sec))
		( if( eq (second unit) 'x) 
		12
		(	
		  if (and
		  (listp (second  unit))
		  (eq (first (second unit)) '*)
		  (numberp (second (second unit)))
		  (eq (third (second unit)) 'x)
		  )12
		  -1
		)
		)

		(
		;later expansion
		)

	)
)


(defun SecI(unit)
     (if (listp (second unit))
	(list '* (list '/ 1 (second (second unit))) (list 'ln  (list '+ 'Sec (list '* (second (second unit)) (third (second unit))) 'Tan (list '* (second (second unit)) (third (second unit)))) ) )
	(list 'ln  (list '+ 'Sec (second unit) 'Tan (second unit)) )	

      )
)


(defun isCosec(unit)
	(	
	if(or (eq (car unit) 'cosec) (eq (car unit) 'Cosec))
		( if( eq (second unit) 'x) 
		13
		(	
		  if (and
		  (listp (second  unit))
		  (eq (first (second unit)) '*)
		  (numberp (second (second unit)))
		  (eq (third (second unit)) 'x)
		  )13
		  -1
		)
		)

		(
		;later expansion
		)

	)
)


(defun CosecI(unit)
     (if (listp (second unit))
	(list '* (list '/ -1 (second (second unit))) (list 'ln  (list '+ 'cosec (list '* (second (second unit)) (third (second unit))) 'cot (list '* (second (second unit)) (third (second unit)))) ) )
	(list '* (list -1) (list 'ln  (list '+ 'cosec (second unit) 'cot (second unit)) ))	

      )
)


;;================================================ trigo hyperbolic ============================================================



(defun isSinh(unit)
	(	
	if(or (eq (car unit) 'sinh) (eq (car unit) 'Sinh))
		( if( eq (second unit) 'x) 
			      14
		(	
		 ; if(and (eq (second unit) '*) (eq (isNumber (list (third unit))) 4) (eq (fourth unit) 'x)) 8
		  if (and
		  (listp (second  unit))
		  (eq (first (second unit)) '*)
		  (numberp (second (second unit)))
		  (eq (third (second unit)) 'x)
		  )14
		  -1
		)
		)

		(
		;later expansion
		)

	)
)

(defun SinhI(unit)
     (if (listp (second unit))
	 (list '* (list '/ 1 (second (second unit))) (list 'Cosh (list '* (second (second unit)) (third (second unit))) ))
	 (list 'Cosh (second unit) )
	
	)
)



(defun isCosh(unit)
	(	
	if(or (eq (car unit) 'cosh) (eq (car unit) 'Cosh))
		( if( eq (second unit) 'x) 
		15
		(
		
		  if (and
		  (listp (second  unit))
		  (eq (first (second unit)) '*)
		  (numberp (second (second unit)))
		  (eq (third (second unit)) 'x)
		  )15
		  -1
		)
		)

		(
		;later expansion
		)

	)
)


(defun CoshI(unit)
     (if (listp (second unit))
	 (list '* (list '/ 1 (second (second unit))) (list 'sinh (list '* (second (second unit)) (third (second unit))) ))
	(list 'Sinh (second unit))
	
      )
)



(defun isTanh(unit)
	(	
	if(or (eq (car unit) 'tanh) (eq (car unit) 'Tanh))
		( if( eq (second unit) 'x) 
		16
		(	
		  if (and
		  (listp (second  unit))
		  (eq (first (second unit)) '*)
		  (numberp (second (second unit)))
		  (eq (third (second unit)) 'x)
		  )16
		  -1
		)
		)

		(
		;later expansion
		)

	)
)



(defun TanhI(unit)
     (if (listp (second unit))
	(list '* (list '/ 1 (second (second unit))) (list 'ln 'cosh (list '* (second (second unit)) (third (second unit)))) )
	(list 'ln (list 'cosh (second unit) )	)
	
      )
)


(defun isCoth(unit)
	(	
	if(or (eq (car unit) 'coth) (eq (car unit) 'Coth))
		( if( eq (second unit) 'x) 
		17
		(	
		  if (and
		  (listp (second  unit))
		  (eq (first (second unit)) '*)
		  (numberp (second (second unit)))
		  (eq (third (second unit)) 'x)
		  )17
		  -1
		)
		)

		(
		;later expansion
		)

	)
)


(defun CothI(unit)
     (if (listp (second unit))
	(list '* (list '/ 1 (second (second unit))) (list 'ln 'sinh (list '* (second (second unit)) (third (second unit)))) )
	(list 'ln 'Sinh (second unit))	

      )
)




(defun isSech(unit)
	(	
	if(or (eq (car unit) 'sech) (eq (car unit) 'Sech))
		( if( eq (second unit) 'x) 
		18
		(	
		  if (and
		  (listp (second  unit))
		  (eq (first (second unit)) '*)
		  (numberp (second (second unit)))
		  (eq (third (second unit)) 'x)
		  )18
		  -1
		)
		)

		(
		;later expansion
		)

	)
)


(defun SechI(unit)
     (if (listp (second unit))
	(list '* (list '/ 1 (second (second unit))) (list 'arctan  (list 'Sinh ( list '* (second (second unit)) (third (second unit)))) ) )
	(list 'acrtan  (list 'Sinh (second unit)) )	

      )
)


(defun isCosech(unit)
	(	
	if(or (eq (car unit) 'cosech) (eq (car unit) 'cosech))
		( if( eq (second unit) 'x) 
		19
		(	
		  if (and
		  (listp (second  unit))
		  (eq (first (second unit)) '*)
		  (numberp (second (second unit)))
		  (eq (third (second unit)) 'x)
		  )19
		  -1
		)
		)

		(
		;later expansion
		)

	)
)


(defun CosechI(unit)
	(if (listp (second unit))
	(list '* (list '/ 1 (second (second unit))) (list 'ln 'tanh (list '* (list '/ (second (second unit)) 2) (third (second unit))) ) )
	(list 'ln 'tanh (list '/ (second unit) 2) )	
	)
)


