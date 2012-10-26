; differentator
(defun mainDiff ( block )
    (
        ;; TODO add selection rules 
        ;XD lis
        if(eq (listp block) nil) (mainDiff (list block))
        (
			if(eq (isNumberD block) 4) (list 0)
			(
				if(eq (isSingleD block) 1) (XD block)
				(
					if(eq (isPowerD block) 2) (PowD block)
					(
						if(eq (isMultD block) 3) (MultD block)
						(
							if(eq (isInverseD block) 5) (PowD block)
							(
								if(eq (isExpD block) 6) (ExpD block)
								;nil
								(
			    if(eq (isLnD block) 7) (LogD block)
				(
					if(eq (isSinD block) 8) (SinD block)
					(
						if(eq (isCosD block) 9) (CosD block)
						(
							if(eq (isTanD block) 10) (TanD block)
							(
								if(eq (isCotD block) 11) (CotD block)
								
								(
								  if(eq (isSecD block) 12) (SecD block)
								
								    (
								      if(eq (isCosecD block) 13) (CosecD block)
								
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
        )
    ))

(defun diff( str )
    (
        if(eq (car str) '+) 
        (
            cons '+ (loopDiff (cdr str))
        )
        ( mainDiff  str)
    ))

(defun loopDiff(block)
    (
        if(eq (length block) 1)  (list (mainDiff (car block)))
        (;print block);
        cons (mainDiff (car block)) (loopDiff (cdr block)))
    ))
    
    
(defun PowD ( unit )
    (
        if(listp (second unit)) 
        (
            if(eq (third unit) 0)
                (list 0)      
            (list '* (list (third unit)) (list '^ (second unit) (- (third unit) 1)) (diff (second unit)))
            
        )
        (    if(eq (third unit) 0) (list 0) 
            (list '* (list (third unit)) (list '^ (second unit) (- (third unit) 1)) )
        )
    ))
    
(defun XD (unit )
    (
        list 1
    ))
    
( defun MultD( unit )
    (
        if(listp (third unit)) (list '* (second unit) (diff (third unit)))
         (second unit)
    ))   

( defun ExpD( unit);;; REDO XXX
    (
        if(listp (third unit)) (list '* (list '^ 'e (third unit)) (diff (third unit)))
        (list '^ 'e (third unit))
    )) 
    
(defun LogD( unit)
    (
        if(listp (second unit)) (list '* (list '/ 1 (second unit)) (diff (second unit)))
         (list '/ 1 (second unit)) 
    ))
    
(defun SinD( unit)
    (
        if(listp (second unit)) (list '* (list 'cos (second unit)) (diff (second unit)))
        (list 'cos (second unit)) 
    ))
    
(defun CosD( unit)
    (
        if(listp (second unit)) (list '* (list -1) (list 'sin (second unit)) (list (diff (second unit))))
        (list '* (list -1) (list 'sin (second unit))  )
    ))        

(defun TanD( unit)
    (
        if(listp (second unit)) (list '* (list 'sec (second unit)) (list 'sec (second unit)) (diff (second unit)))
        (list '* (list 'sec (second unit)) (list 'sec (second unit)))
    ))

(defun cotD( unit)
    (
        if(listp (second unit)) (list '* -1 (list 'cosec (second unit)) (list 'cosec (second unit)) (diff (second unit)))
        (list '* -1 (list 'cosec (second unit)) (list 'cosec (second unit)))
    ))

(defun SecD( unit)
    (
        if(listp (second unit)) (list '* (list 'sec (second unit)) (list 'tan (second unit)) (diff (second unit)))
        (list '* (list 'sec (second unit)) (list 'tan (second unit)))
    ))    
      
(defun cosecD( unit)
    (
        if(listp (second unit)) (list '* -1 (list 'cot (second unit)) (list 'cosec (second unit)) (diff (second unit)))
        (list '* -1 (list 'cot (second unit)) (list 'cosec (second unit)))
    ))      
        
( defun isExpD( unit )
    (
        if(eq (car unit) '^)
            ( if(eq (second unit) 'e)
                   
                    6    
                    ;if(eq (third unit) 'x) 6
                    ;(print "error")
                
            (
               ;later expansion
            ))
        (;later expansion
        )
    ))
    
( defun isInverseD( unit )
    (
        if(eq (car unit) '/)
            ( if(numberp (second unit)) 5
                
            (
               ;later expansion
            ))
        (;later expansion
        )
    ))

( defun isMultD( unit )  
    (
        if(eq (car unit) '*)
            ( if(or (numberp (second unit)) (and (eq (length (second unit)) 1) (numberp (car (second unit))) )) 3
                
            (
               ;later expansion
            ))
        (;later expansion
        )
    ))
    
( defun isPowerD( unit )
    (
        if(eq (car unit) '^)
            ( if(eq (second unit) 'x) 2
                
            (
               ;later expansion
            ))
        (;later expansion
        )
    )) 
    
(defun isSingleD( unit )
    (
        if(eq (car unit) 'x) 1            
        nil
    ))
    
(defun isNumberD( unit )
    (
        if(numberp (car unit)) 4            
        nil
    ))
    
(defun isSinD(unit)
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


(defun isCosD(unit)
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


(defun isTanD(unit)
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


(defun isCotD(unit)
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


(defun isSecD(unit)
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


(defun isCosecD(unit)
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
       
(defun isLnD( unit )
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
