;inicijalizacija liste koja ce sluziti za transformaciju slova u brojeve i obrnuto
(load "Kretanje.lisp")
(load "nakon-kretanja.lisp")
(load "Sendwich.lsp")
(setq y-axis '((A 0) (B 1) (C 2) (D 3 ) (E 4) (F 5) (G 6) (H 7) (I 8) (J 9) (K 10) (L 11) (M 12) (N 13) (O 14) (P 15) (Q 16) (R 17) (S 18) (T 19) (U 20) (V 21) (W 22) (X 23) (Y 24) (Z 25)) )

; unos dimenzije
(defun insertDim() 
	(format t "Unesite dimenziju table: ")
	(let ((dim (read))) (cond 
							((and (numberp dim) (< dim 21) (> dim 6)) dim )
							(t(insertDim))
						)
		;test
		;(setq dim (read)
			)
)




;pomocna f-ja za inicijalizaciju table

(defun initTablePom (i dim tabla) 
			(cond 
			 
			 	((< i (* dim 2) ) (append tabla '(x) ))
			 	((< i (-(* dim dim)  (* 2 dim))) (append tabla '(-)))
			 	(t (append tabla '(o)))

			)
)

;funkcija za inicijalizaciju promenjive koja ce cuvati stanje table

;inicijalizacija pocetnog stanja
(defun initTable(tabla dim) 
	(loop for i from 0 to (-(* dim dim) 1)
		do (setq tabla (initTablePom i dim tabla) ) ; 
	)
	tabla
)
;selekcija elementa iz matrice
(defun getElm (mat row col)
	(cond 
		((or (>= row dim) (>= col dim) (< row 0) (< col 0)) '())
		(t (nth (+(* row dim) col) mat))
	)

)


;(format t " TABLA: ~% ~A" table) ;;test

;prikaz table
;koristi globalne dim i table
(defun prikaz(row table)
	(cond 
		((> row dim) '())
		( (= row 0) (loop for i from 0 to dim do  ( if (= i 0) ( format t "     ") (format t "~d " i)))  (format t "~%~%") (prikaz (1+ row) table) )
		(t ( loop for i from 0 to dim do (if (= i 0) (format t " ~a   " (car (nth (- row 1) y-axis))) (format t "~a " (nth ( +(*(- row 1) dim) (- i 1)) table)) ) ) (format t "~%") (prikaz (1+ row) table))

	)
)

;fja transformacije slova u odgovarajuci broj
(defun transChar (char) 
	(cadr (assoc char y-axis))
)
;Fja za unos novih pozicija 
;Provera validnisti unosa
(defun inputValidate (input) 
	(cond 
		( (and (listp input) (listp (car input)) (listp (cadr input)))
			(and 
			(and (= (length input) 2) (= (length (car input)) 2) (= (length (cadr input)) 2)  )
			(and (< (transChar (caar input))  dim ) (<= (cadar input) dim) (< (transChar (caadr input)) dim ) (<= (cadadr input) dim))
			)
		)
		(t '())
		
	)
)

;transofmracija iz unetih koordinata u logicke
(defun toLogic (validInput)
	(list (list (transChar (caar validInput))  (- (cadar validInput) 1 )) 
		  (list (transChar (caadr validInput))  (- (cadadr validInput) 1 ) ) 
	)
)

;za promenu igraca
(setq player '((X O) (O X))) 
;TESTIRANO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;zamena elementa niza
(defun replaceElm (lista n elem)
  (cond 
    ((null lista) '())
    ((= n 0) (cons elem (cdr lista)))
    (t (cons(car lista) (replaceElm (cdr lista) (- n 1) elem))))
 )

;; vraca izmanjenu tabelu za prelazak iz (car axis) u (cadr axis) 
(defun changePosition (mat axis play)
	(let  ( )
		(setq mat (replaceElm mat (+ (* (caar axis) dim) (cadar axis)) '- ))
		(setq mat (replaceElm mat (+ (* (caadr axis) dim) (cadadr axis)) play)) mat
	)
)

;vraca unetov korisnikov izbor
(defun insertGameMod ()
	(let ((input '()))
		(loop
			(format t "~%1) Covek~%2) Racunar~%Unesite ko prvi igra:")
			(setq input (read))
			(when (and (numberp input) (> input 0) (< input 3)) (return input))

		)
	)
)

(defun clearRow (table coo axis tempCoo) ;Pomocna funkcija za clearPositions koja postavlja '- izmedju pocetne i krajnje pozicije u vrsti tabele "table"
	(cond 
		((= (1+ (cadr tempCoo)) (cadr axis)) table)
		((> (cadr coo) (cadr axis)) (clearRow table axis coo axis))
		( t (clearRow (replaceElm table (+ (* dim (car tempCoo)) (1+ (cadr tempCoo))) '-) coo axis (list (car tempCoo) (1+ (cadr tempCoo)))))	

	)

)

(defun clearColumn (table coo axis tempCoo);Pomocna funkcija za clearPositions koja postavlja '- izmedju pocetne i krajnje pozicije u koloni tabele "table"
	(cond 
		((= (+ 1 (car tempCoo)) (car axis)) table)
		((> (car coo) (car axis)) (clearColumn table axis coo axis))
		( t (clearColumn (replaceElm table (+ (* dim (1+ (car tempCoo)))  (cadr tempCoo)) '-) coo axis (list (1+ (car tempCoo)) (cadr tempCoo)) ))
	)
)


(defun clearPositions (table coo axis) ;postavlja '- od koordinate gde je odigran potez(axis) do svih kooridnata do kojih su se desili sendvici(coo) na tabeli "table"
	(cond 
		((null coo) table)
		((and (= (caar coo) (car axis)) (/= (cadar coo) (cadr axis))) (clearPositions (clearRow table (car coo) axis (car coo)) (cdr coo) axis))
		(t (clearPositions (clearColumn table (car coo) axis (car coo)) (cdr coo) axis))
	)
)
(defun numCleared (axis coo) ;vraca ukupan broj  protivnikovih figura koje ce biti uklonjene potezom "axis"                                                                                                               
	(cond 
		((null coo) 0)
		((= (car axis) (caar coo)) (+ (1- (abs (- (cadr axis) (cadar coo)))) (numCleared axis (cdr coo))))
		(t (+ (1- (abs (- (caar coo) (car axis)))) (numCleared axis (cdr coo))))
	)
)
(defun isWinner (play numx numo)

	(if (equal play 'x) (< numo 5) (< numx 5))
)
;globalna promenjvia dim
;num=1, numFail=0 => inicijalne vrednosti
;;pomocne fje za fju newCoordinates
(defun leftCoo (axis curTable play num numFail)
	(cond 
		((= numFail 2) '())
		((moveValidate (list axis (list (car axis) (- (cadr axis) num))) dim play curTable) (cons (list (car axis) (- (cadr axis) num)) (leftCoo axis curTable play (1+ num) numFail)  )) ;(if (moveValidate (list axis (list (car axis) (1- (cadr axis)))) dim play curTable)) T (if) )
		(t (leftCoo axis curTable play (1+ num) (1+ numFail)))
	)
)


(defun rightCoo (axis curTable play num numFail)
	(cond 
		((= numFail 2) '())
		((moveValidate (list axis (list (car axis) (+ (cadr axis) num))) dim play curTable) (cons (list (car axis) (+ (cadr axis) num)) (rightCoo axis curTable play (1+ num) numFail)  )) ;(if (moveValidate (list axis (list (car axis) (1- (cadr axis)))) dim play curTable)) T (if) )
		(t (rightCoo axis curTable play (1+ num) (1+ numFail)))
	)
)
(defun upCoo (axis curTable play num numFail)
	(cond 
		((= numFail 2) '())
		((moveValidate (list axis (list (- (car axis) num) (cadr axis) )) dim play curTable) (cons (list (- (car axis) num) (cadr axis)) (upCoo axis curTable play (1+ num) numFail)  )) ;(if (moveValidate (list axis (list (car axis) (1- (cadr axis)))) dim play curTable)) T (if) )
		(t (upCoo axis curTable play (1+ num) (1+ numFail)))
	)
)
(defun downCoo (axis curTable play num numFail)
	(cond 
		((= numFail 2) '())
		((moveValidate (list axis (list (+ (car axis) num) (cadr axis) )) dim play curTable) (cons (list (+ (car axis) num) (cadr axis)) (downCoo axis curTable play (1+ num) numFail)  )) ;(if (moveValidate (list axis (list (car axis) (1- (cadr axis)))) dim play curTable)) T (if) )
		(t (downCoo axis curTable play (1+ num) (1+ numFail)))
	)
)
;;-------------------------------------------------------------


;;vraca kordinate koje mogu da se odigraju
;;axis - trenutna pozicija, curTable - trenutno stanje, play - trenutno ko je na potezu
;koristi se globalna promenjvia dim
(defun newCoordinates (axis curTable play)
	(append
		(leftCoo axis curTable play 1 0)
			(append (upCoo axis curTable play 1 0)
				(append (rightCoo axis curTable play 1 0)
					(downCoo axis curTable play 1 0)
				)		
			)
	)
)

; axis - pocetna pozicija  newCoo - pozicija na koju se prelazi
; curTable - trenutno stanje tabele, play- ko igra (x/o), numOp - broj protivnikovih figura
;vraca novo stanje tabele i "T" ako je korisnik "play" pobedio i vraca broj protivnikovih figura
;
(defun newState (axisa newCoo curTable play numOp)

	(setq  curTable (changePosition curTable (list axisa newCoo) play))
	(setq numOp (- numOp (numCleared newCoo (Sendwich (car newCoo) (cadr newCOO) play curTable) )))
	(setq curTable (clearPositions curTable (Sendwich (car newCoo) (cadr newCoo)  play curTable) newCoo))
			
				
	;;uslovi za pobedu ili poraz
	(list curTable (if (or (< numOp 5) (playJoin (car newCoo) (cadr newCoo) play curTable)) t '()) numOp (list axisa newCoo))

)

;novo stanje
;axis- trenutna pozicija, curTable - trenutno stanje tabele, play- ko igra? (X/O), numOP - borj protivnickih figura 
;vraca sva stanja za trenutnu poziciju "axis"
;newCoo vraca fja newCoordinates koja zapravo vraca sve moguce kordinate svih mogucih sledecih poteza
;fja vraca niz rezultata oblika kakve vraca newState za sve poteze koji mogu da se odigraju iz pozicije axis
(defun newStates (axisa curTable play numOp newCoo)
	(cond 
		((null newCoo) '())
		(t (cons (newState axisa (car newCoo) curTable play numOP) (newStates axisa curTable play numOP (cdr newCoo))))
	)
)
;dim globalna
(defun getPositions (play numPlay ctable)
	(let (
		(curCoo '(0 0))
		(saveCoo '())
		)
		(loop
			(if (equal (getElm ctable (car curCoo) (cadr curCoo)) play) (let () (setq saveCoo (cons curCoo saveCoo )) (setq numPlay(1- numPlay))) )
			(if (= (cadr curCoo) (1- dim)) (setq curCoo (list (1+ (car curCoo)) '0)) (setq curCoo (list (car curCoo) (1+ (cadr curCoo)))))

		(when (= numPlay 0) (return saveCoo))) ;ako je doso do kraja tabele ili ako je naso sve play figure izlazi iz petlje
	)
)


;dim globalna
;vraca sva moguca stanja tabele koja mogu da proisteknu iz trenutnog stanja
(defun processState (state play numPlay pos );pos se dobija sa getPositions
	(cond 
		((null pos) '())
		(t (append (newStates (car pos) (car state) play (caddr state) (newCoordinates (car pos) (car state) play)) (processState state play numPlay (cdr pos))))
	)
)

;provera da li su tabele iste
;testirana
(defun equalT (table1 table2)
	(cond 
		((null table1) t)
		((not(equal (car table1) (car table2))) '())
		(t (equalT (cdr table1) (cdr table2)) )
	)
)

;gornja fja ce s eprimenjivati za identicnosti stanja, pa je samo zapakovana u fju ispod
(defun equalState (state1 state2)
	(equalT (car state1) (car state2))
)
;proverava dal se state nalazi u niz stateArrey
(defun memberState (state stateArray)
	(cond 
		((null stateArray) '())
		((equalState state (car stateArray)) t)
		(t (memberState state (cdr stateArray)))
	)
)

;vraca niz stanja iz childs koja se ne nalaze u processed
(defun filterChilds (childs processed)
	(cond 
		((null childs) '())
		((memberState (car childs) processed) (filterChilds (cdr childs) processed))
		(t (cons (car childs) (filterChilds (cdr childs) processed)))

	)


)
;(setq tmpState (list table '() '14 '(1 3)))
;(setq tmpTree (statesTree tmpState 'x '14 '14 '3 '()))
;vraca stablo  stanja zadate dubine
(defun StatesTree (curState play numOp numPlay depth processed)
	(cond
		( (or (= depth 0) (equal (car(cdr curState)) 'T)) (list (list curState '())))

		(t
			(let 
				( 
		 		(childs '())
		 		(processed2 '())
		 		(forExecute '())
		 		(nodes '())
				) 
		
				(setq childs (processState curState play numPlay (getPositions play numPlay (car curState)))) 	;;deca 
				(setq processed2 (if (null processed) (list curState) (append processed (list curState ))) ) ;;Obradjeni 
				(setq forExecute (filterChilds childs processed ))	;;koji nisu obradjeni forExecute
				(cons (list curState childs)		;;Nodes spajamo koren i decu (a(bcd)) ... 
				
					(dolist (child forExecute nodes)	;;... i na to dodajemo decu sa njihovom decom, na taj nacin kreiramo stablo
						(if (null nodes) 
							(setq nodes (StatesTree child (cadr (assoc play player)) numPlay (caddr child) (1- depth) processed2))
							(setq nodes (append nodes (StatesTree child (cadr (assoc play player)) numPlay (caddr child) (1- depth) processed2) ))
							)
						nodes
					)
				)
			)
		)
	)
)
;;vraca cvor iz stabla koje odgovara datom stanju (cvor je stanje sa svim svojom decom(stanjima))
(defun returnNode (state tree) 
	(cond 
		((equalState state (caar tree)) (car tree))
		((null tree) '())
		(t (returnNode state (cdr tree)))


	)

)
;;vraca vrednost poteza koordinata=axis (pozitivno za x, negativno za o)
(defun checkState (State play)
	(let* (
			(win (cadr State))
			(coo (car (last State)))
			(axis (cadr coo))
			(board (car State))

		)
		
		(if win (if (equal play 'x) (list '100 axis) (list (- 0 100) axis)))	;;ako je pobednik vraca vrednost 100
		
		(if (equal play 'x)
			
		
				(if (null (Sendwich (car axis) (cadr axis) play board))  ;;ako nije sendvic 
					(let( (valueHorizontala '0) 
						(valueVertikala '0)
					
						)
					(setq valueHorizontala (+ (senlevo (car axis) (cadr axis) play board) (sendesno (car axis) (cadr axis) play board)))		;;vraca broj prot. figura pored zadatate figure po horizontali  		
				
					(setq valueVertikala (+ (sen (car axis) (cadr axis) play board) (sendole (car axis) (cadr axis) play board)))	;;isto samo po vertikali vraca broj prot. fig
				
					(if(> valueHorizontala valueVertikala) (list (+ 10 valueHorizontala) coo) (list(+ 10 valueVertikala) coo))	;;ako imamo vise uzastopnih prot. figura vracamo 10+br.prot.figura
		
					)
					(list (+  50 (numCleared axis (Sendwich (car axis) (cadr axis) play board))) coo) ;;else ako je sendvic vracam vrednost 50+ broj odnetih figura
				
				)
				
				;(if(playJoin (car axis) (cadr axis) play board) 100)	;;ako je pobednik vraca vrednost 100
		
				(if (null (Sendwich (car axis) (cadr axis) play board))  ;;ako nije sendvic 
					(let( (valueHorizontala '0) 
						(valueVertikala '0)
					
						)
					(setq valueHorizontala (- 0 (+ (senlevo (car axis) (cadr axis) play board) (sendesno (car axis) (cadr axis) play board))))		;;vraca broj prot. figura pored zadatate figure po horizontali  		
				
					(setq valueVertikala (- 0 (+ (sen (car axis) (cadr axis) play board) (sendole (car axis) (cadr axis) play board))))	;;isto samo po vertikali vraca broj prot. fig
				
					(if(> valueHorizontala valueVertikala) (list (- 0 (- 10 valueHorizontala)) coo) (list (- 0 (- 10 valueVertikala)) coo))	;;ako imamo vise uzastopnih prot. figura vracamo 10+br.prot.figura
		
					)
					(list (- 0 50 (numCleared axis (Sendwich (car axis) (cadr axis) play board))) coo) ;;else ako je sendvic vracam vrednost 50+ broj odnetih figura
				
				)
				)
		)
			
		 ;;then grana ako je x vraca pozitivne vrednosti 
)
;; vraca najbolji potez za max(a) i svoje ili koordinate roditelja, u zavisnosti da li je a inicijalizovana (pored vrednosti) sa koordinatama  
(defun maxValue ( node play tree a b depth)
	(cond
		((= depth 0) (checkState (car node) (cadr (assoc play player) ))) ;; vrsi procena vrednosti uzimajuci da je suprotan igrac Play-u na potezu
		(t (let ( (tmp '()) (coo (car (last (car node))) )  
				)					
				
			(setq tmp 
				(dolist (child (cadr node))
					(setq a  (vece a (minValue (returnNode child tree) (cadr (assoc play player)) tree a b (- depth 1)) coo));u a se pamti (vednost (koordinate)), a u slucaju da ih cvor koji se obradjuje vec ima, ostaju njegove koordinate
					 (if(GE a b) (return-from maxValue b))
					
				)
			)
			(if(null tmp) (return-from maxValue a ) tmp)
			)
		)
		
	)

)
;; podaci za debagiranje
;;(setq tmpState (list table '() 14 '()))
;;(setq tmpTree (statestree tmpstate 'x 14 14 2 '()))
;;(setq tmpNode (returnnode tmpstate tmpTree))
;;(maxvalue tmpNode  'x tmpTree -200 200 2)
;;(maxvalue stat  'x tree '(0(a a)) '(5(b b)) 2)
;;(0 (a a)) 
;;----------------------------------------------


;;vraca najbolji potez za min(a) i svoje ili koordinate roditelja, u zavisnosti da li je a inicijalizovana (pored vrednosti) sa koordinatama
;;inicijalno se stanje cvora kojeg saljemo fji ima koordinate nill 
(defun minValue ( node play tree a b depth)
	(cond
		((= depth 0) (checkState (car node) (cadr (assoc play player) ))) ;; vrsi procena vrednosti uzimajuci da je suprotan igrac Play-u na potezu
		(t (let ( (tmp '()) (coo (car (last (car node))))  

				)
			(setq tmp 
				(dolist (child (cadr node))
					(setq b (manje b (maxValue  (returnNode child tree) (cadr (assoc play player)) tree a b (- depth 1)) coo));; u b se pamti (vednost (koordinate)), a u slucaju da ih cvor koji se obradjuje vec ima, ostaju njegove koordinate
					(if(LE b a) (return-from minValue a))
					
					
				)
			)
			(if(null tmp) (return-from minValue  b) tmp)
			)
		)
		
	)

)
;-------------------------------------------------
;-------From Sandwich.lisp------------------------
;-------------------------------------------------
;pomocne fje za min i max algoritme
(defun vece (a b coo)
	(if(and (listp b) (listp a)) 
		(if(> (car a)  (car b)) (return-from vece a) (if coo  (return-from vece (list (car b) coo))  (return-from vece b))) 		
	)
	(if(and (atom a)(atom b))
		(if(> a b) (return-from vece a) (return-from vece b))
	)
	(if(and(listp a)(atom b))
		(if(> (car a) b) (return-from vece a) (return-from vece (list b (cadr a))))
	)
	(if(and(atom a)(listp b))
		(if(> a (car b))(return-from vece a) (return-from vece b))
	)
	
)

(defun manje (a b coo)
	(if(and (listp b) (listp a)) 
		(if(< (car a)  (car b)) (return-from manje a) (if coo (return-from manje (list (car b) coo)) (return-from manje b)))		
	)
	(if(and (atom a)(atom b))
		(if(< a b) (return-from manje a) (return-from manje b))
	)
	(if(and(listp a)(atom b))
		(if(< (car a) b)  (return-from manje a) (return-from manje (list b (cadr a))))
	)
	(if(and(atom a)(listp b))
		(if (< a (car b)) (return-from manje a) (return-from manje b))
	)
	
)


(defun GE (a b)		;;greather equal
		(if(and (listp b) (listp a)) 
		(if(>= (car a)  (car b)) 'T '()) 		
	)
	(if(and (atom a)(atom b))
		(if(>= a b) 'T '())
	)
	(if(and(listp a)(atom b))
		(if(>= (car a) b) 'T '())
	)
	(if(and(atom a)(listp b))
		(if(>= a (car b)) 'T '())
	)
	
)

(defun LE (a b)		;;greather equal
		(if(and (listp a) (listp b)) 
		(if(<= (car a)  (car b)) 'T '()) 		
	)
	(if(and (atom a)(atom b))
		(if(<= a b) 'T '())
	)
	(if(and(listp a)(atom b))
		(if(<= (car a) b) 'T '())
	)
	(if(and(atom a)(listp b))
		(if(<= a (car b)) 'T '())
	)
	
)
;-------------------------------------------------
;-------------------------------------------------
;-------------------------------------------------

;-------------------------------------------------
;unos dimenzije tabele
(setq dim (insertDim))

;inicijalizacija table
(defvar table '())
(setq table (initTable table dim))
;izbor moda
(if (= (insertGameMod) 1) (setq user 'x) (setq user 'o))

;IGRA 
(do 
  	( (play 'X (cadr (assoc play player)))
  	  (win '() win)
  	  (numX (* 2 dim) numX)
  	  (numO (* 2 dim) numO)
  	  
  	)
  	
  	((and t win) ( let () (setq play (cadr (assoc play player))) (format t"Pobednik je ~a!" play)) ) ;uslov za izlaz iz petlje

	(let ( (input '()) (axis '())  ) ;pocetne vrednosti lokalnih promenjivih
		(if (equal user play) ;komentarisano zbog testiranja - na ovaj nacin mogu da igraju 2 korisnika
			;; unos korisnika
			(let () 
				;stampa matrice 
				(format t "~%")
				(prikaz 0 table)
				(loop 
				(format t"Potez ~a:" play)	
				(setq input (read))												;;;
				(if (inputValidate input) (setq axis (toLogic input)))						;;; korisnik unosi sve dok ne unese validnu vrednost	
				;(print axis)	;TEST
				(when (moveValidate axis dim play table) (return))  								;;; nezavrsena fja moveValidate() load from Kretanje.lisp
				)
			)
			(let ((tmpTree '()) (curState '()) (numPlay (if (equal play 'x) numX numO)) (numOp (if (equal play 'x) numO numX))  )
				(setq curState (list table win numOp '())) 
				(setq tmpTree ( statesTree curState play numOp numPlay 2 '() ))
				(setq axis (cadr (if (equal play 'x) (maxValue (returnNode curState tmpTree) play tmpTree '-200 '200 '2) 
													  (minValue (returnNode curState tmpTree) play tmpTree '-200 '200 '2) )))
	
			)
		)	
		;;racunarov potez
	
		;----------------------------------------------------------------
			 
		
				;fja koja ce da izmeni matricu kada se proslede kordinate za pomeranje
				(setq  table (changePosition table axis play))
				(if (equal play 'X) (setq numO (- numO (numCleared (cadr axis) (Sendwich (caadr axis) (cadadr axis) play table)))) (setq numX (- numX (numCleared (cadr axis) (Sendwich (caadr axis) (cadadr axis) play table)))))
				(setq table (clearPositions table (Sendwich (caadr axis) (cadadr axis)  play table) (cadr axis)))
				
				
				;;uslovi za pobedu ili poraz
				(if (or (isWinner play numX numO) (playJoin (caadr axis) (cadadr axis)  play table)) (setq win t))
		;-------------------------------------------------------------------		
		;fja obrada pristiglih koodinate iz fje Sandwich() i na osnovu koordinate preuredjuje Table ako je potrebo
		;Sandwitch load from Sendwich.lisp

		
		
	)


				
	
)


