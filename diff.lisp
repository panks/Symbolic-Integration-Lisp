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
        if(listp (second unit)) (list '* (third unit) (list '^ (second unit) (- (third unit) 1)) (diff (second unit)))
        (list '* (third unit) (list '^ (second unit) (- (third unit) 1)) )
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
        if(listp (second unit)) (list '* -1 (list 'sin (second unit)) (diff (second unit)))
        (list '* -1 (list 'sin (second unit))  )
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
            ( if(numberp (second unit)) 3
                
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
        (
            print "error"    
        )
    ))
    
(defun isNumberD( unit )
    (
        if(numberp (car unit)) 4            
        (
            print "error"    
        )
    ))