; differentator
(defun diff ( block )
    (
        ;; TODO add selection rules 
        ;XD lis
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
        
    ))
    
(defun PowD ( unit )
    (
        if(listp (second unit)) (list '* (third unit) (list '^ (second unit) (- (third unit) 1)) (diff (second unit)))
        (list '* (third unit) (list '^ (second unit) (- (third unit) 1)) (diff (list (second unit))))
    ))
    
(defun XD (unit )
    (
        list 1
    ))
    
( defun MultD( unit )
    (
        if(listp (third unit)) (list '* (second unit) (diff (third unit)))
        (list '* (second unit) (diff (list (third unit))))
    ))   

( defun ExpD( unit);;; REDO XXX
    (
        if(listp (third unit)) (list '* (list '^ 'e (second unit)) (diff (third unit)))
        (list '* (list '^ 'e (second unit)) (diff (list (third unit))))
    )) 
    
(defun LogD( unit)
    (
        if(listp (second unit)) (list '* (list '/ 1 (second unit)) (diff (second unit)))
        (list '* (list '/ 1 (second unit)) (diff (list (second unit))))
    ))
    
(defun SinD( unit)
    (
        if(listp (second unit)) (list '* (list 'cos (second unit)) (diff (second unit)))
        (list '* (list 'cos (second unit)) (diff (list (second unit))))
    ))
    
(defun CosD( unit)
    (
        if(listp (second unit)) (list '* -1 (list 'sin (second unit)) (diff (second unit)))
        (list '* -1 (list 'sin (second unit)) (diff (list (second unit))))
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
