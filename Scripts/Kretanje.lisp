(defun checkRow ( startPos tempPos endPos table )
	(cond

	;	((=  startPos endPos) '())
		((= endPos tempPos) T)
		((< startPos endPos) (If (eq (nth (+ 1 tempPos) table) '-) (checkRow startPos (1+ tempPos) endPos table) (if (= (- endPos startPos) 2) t '()) ))
		((> startPos endPos) (If (eq (nth (- tempPos 1) table) '-) (checkRow startPos (1- tempPos) endPos table) (if (= (- startPos endPos) 2) t '())))
		
	)
)

(defun checkColum ( startPos tempPos endPos dim table )
	(cond
		
		;((=  startPos endPos) '())
		((= endPos tempPos) T)
		((< startPos endPos) (If (eq (nth (+ dim tempPos) table) '-) (checkColum startPos (+  dim tempPos) endPos dim table) (if (= (- endPos startPos)  (* 2 dim)) t '())))
		((> startPos endPos) (If (eq (nth (- tempPos dim) table) '-) (checkColum startPos (- tempPos  dim) endPos dim table) (if (= (- startPos endPos) (* 2 dim))t '())))
		
	)
)



(defun moveValidate (logAxis dim play table)
	(let ((tempR (caar logAxis)) (tempC (cadar logAxis)) (endR (caadr logAxis)) (endC (cadadr logAxis)))
		( cond
			((null logAxis) '())
			
			((not (eq play (getElm table tempR tempC ))) '())

			((not (eq '- (getElm table endR endC ))) '())
			
			((and (= tempR endR) (/= tempC endC) )  (checkRow (+ (* tempR dim) tempC) (+ (* tempR  dim) tempC) (+ (* endR  dim) endC) table )
			)

			(( and (/= tempR endR) (= tempC endC)) (checkColum (+ (* tempR  dim) tempC) (+ (* tempR dim) tempC) (+ (* endR dim) endC) dim table) 
			)

		) 
			
	)
)


