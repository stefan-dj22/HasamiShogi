(defun graniceS (i j table)   
              (if (or (< i 0)(< j 0)) nil 
               (if (or (> i (- dim 1))(> j (- dim 1))) '() t)))
(defun prazno (i j table)
                (if (equal(getElm table i j) '-)nil t)) 
				
(defun istaX (play i j table)
                  (if (equal play 'X) 
                      (if(equal (getElm table i j) 'X) 
                               t)
                     )) 
(defun istaO (play i j table)
                  (if (equal play 'O) 
                      (if(equal (getElm table i j) 'O) 
                               t)
                     )) 
(defun ko (play)
                (if (equal play 'X) t)) 
					 
 (defun sen (v k play table)
                (if (or (equal (graniceS (- v 1) k table) nil) 
                       (equal (prazno (- v 1) k table) nil)    
                       (if(equal (ko play) nil)(equal(istaO play (- v 1) k table) t)(equal(istaX play (- v 1) k table) t))) 0 
                      (1+ (sen (- v 1) k play table))   
                   )) 
				   

			   

(defun sendole (v k play table)
                (if (or (equal (graniceS (+ v 1) k table) nil) 
                       (equal (prazno (+ v 1) k table) nil)    
                       (if(equal (ko play) nil)(equal(istaO play (+ v 1) k table) t)(equal(istaX play (+ v 1) k table) t))) 0 
                      (1+ (sendole (+ v 1) k play table))   
                   ))


			   
(defun senlevo (v k play table)
                (if (or (equal (graniceS v (- k 1) table) nil) 
                       (equal (prazno v (- k 1) table) nil)    
                       (if(equal (ko play) nil)(equal(istaO play v (- k 1) table) t)(equal(istaX play v (- k 1) table) t))) 0 
                      (1+ (senlevo v (- k 1)  play table))   
                   ))

				   

			   

(defun sendesno (v k play table)
                (if (or (equal (graniceS v (+ k 1) table) nil) 
                       (equal (prazno v (+ k 1) table) nil)    
                       (if(equal (ko play) nil)(equal(istaO play v (+ k 1) table) t)(equal(istaX play v (+ k 1) table) t))) 0 
                      (1+ (sendesno v (+ k 1) play table))   
                   ))


;(if (sendesno v k play) (if ) '())
				  
(defun naplis (v k play table)
                (if(or(equal (granices (- v 1) k table) nil)(if (equal(getElm table v k)'x)(equal(getElm table (- v 1) k) 'x)(equal(getElm table (- v 1) k) 'o)) (equal(getElm table (- v 1) k) '-)) nil
        (if (equal(getElm table ( - (- v (SEN v k play table)) 1) k) play)
                (list ( - (- v (SEN v k play table)) 1) k))));proverava nastanak sendvica gore
        
(defun naplislevo (v k play table)
                (if(or(equal (graniceS v (- k 1) table) nil)(if (equal(getElm table v k)'x)(equal(getElm table v (- k 1)) 'x)(equal(getElm table v (- k 1)) 'o)) (equal(getElm table v (- k 1)) '-)) nil
                (if (equal(getElm table v ( - (- k (SENLEVO v k play table)) 1)) play)
                    (list v ( - (- k (SENLEVO v k play table)) 1)))));proverava nastanak sendvica levo
        
(defun naplisdole (v k play table)
                (if(or(equal (graniceS (+ v 1) k table) nil)(if (equal(getElm table v k)'x)(equal(getElm table (+ v 1) k) 'x)(equal(getElm table (+ v 1) k) 'o)) (equal(getElm table (+ v 1) k) '-)) nil
        (if (equal(getElm table ( + (+ v (SENDOLE v k play table)) 1) k) play)
                (list ( + (+ v (SENDOLE v k play table)) 1) k))));proverava nastanak sendvica dole
        
(defun naplisdesno (v k play table)
                (if(or(equal (graniceS v (+ k 1) table) nil)(if (equal(getElm table v k)'x)(equal(getElm table v (+ k 1)) 'x)(equal(getElm table v (+ k 1)) 'o)) (equal(getElm table v (+ k 1)) '-)) nil
                (if (equal(getElm table v ( + (+ k (SENDESNO v k play table)) 1)) play)
                    (list v ( + (+ k (SENDESNO v k play table)) 1)))))
				
				
				
(defun sendwich (v k play table)
               (and(remove nil (list(naplis V K PLAY table)
                   (naplisdesno V K PLAY table)
                   (naplislevo V K PLAY table)
                   (naplisdole V K PLAY table))))) ;vraca listu listi pozicija gde se desio sendvic, ako nema sendvica vraca nil
