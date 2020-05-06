(defun provera(v play)
               (cond ((equal play 'x)(if(< v '2) t))
                     (t(equal play 'o) (if(>= v (- dim 2))t)) ) )
;;play je ko je na potezu,v je redni broj vrste ,funkcija vraca T ako se nalazi u neutralno polje,koje se ne racuna za pobedu.


(defun granice(v k)
                (or (< v '0)(> v (- dim 1)) (< k '0) (> k (- dim 1))  )
                )
;;vraca vrednost true ako se ne nalaze u granicama,inace ako su u granicama vraca NIL

(setq x (make-array '(8 8) 
                     :initial-contents '((x x x x x x x x) (x x x x x x x x) (- - - - - - - -) (- - - - - - - -) (- - - - - - - -) (- - - - - - - -) (o o o o o o o o)(o o o o o o o o) )))

(defun slpolje(v k table)
               (if(equal (getElm table v k) '-) t '())
               )
;;slpolje funkcija vraca true ako je prazno inace vraca nill ako je zauzeto 

(defun gLevo(v k play table)
               (if (or (equal (granice (- v 1) (- k 1)) T) 
                       (equal (slpolje (- v 1) (- k 1) table) T)
                       (equal (provera (- v 1) play) T)
                        (not(equal (kojafigura (- v 1) (- k 1)table) play ))
                       ) 0 (1+ (gLevo (- v 1) (- k 1) play table)) 
                        
                   )
  )
;;vraca broj elemenata gornje leve dijagonale bez mog elementa

(defun dDesno(v k play table)
               (if (or (equal (granice (+ v 1) (+ k 1)) T) 
                       (equal (slpolje (+ v 1) (+ k 1) table) T)
                       (equal (provera (+ v 1) play) T)
                       (not(equal (kojafigura (+ v 1) (+ k 1) table) play ))
                       ) 0 (1+ (dDesno (+ v 1) (+ k 1) play table)) 
                        
                   )
  )
;;vraca broj elemenata donje desne dijagonale bez mog elementa

(defun kojafigura(v k table)
                (getElm table v k)
                )

;;vracafiguru na tom polju

(DEFUN GLAVNADIJ (V K PLAY table)
                (OR (> (GLEVO V K PLAY table) 3)
                    (> (+ (GLEVO V K PLAY table) (DDESNO V K PLAY table)) 3)))

;;vraca T ako ima pobednika na glavnoj dijagonali inace false

;-----
;(defun gDesno(v k play)
;               (if (or (equal (granice (- v 1) (+ k 1)) T) 
;                       (equal (slpolje (- v 1) (+ k 1) table) T)
;                       (equal (provera (- v 1) play) T)
;                        (not(equal (kojafigura (- v 1) (+ k 1)) play table))
;                       ) 0 (1+ (gLevo (- v 1) (+ k 1) play table)) 
;                        
 ;                  )
 ; )
;--------
(defun gDesno(v k play table)
               (if (or (equal (granice (- v 1) (+ k 1)) T) 
                       (equal (slpolje (- v 1) (+ k 1) table) T)
                       (equal (provera (- v 1) play) T)
                       (not(equal (kojafigura (- v 1) (+ k 1) table) play ))
                       ) 0 (1+ (gDesno (- v 1) (+ k 1) play table)) 
                        
                   )
  )

;;gDesno vraca broj elemenata sa gornje desne dijagonale bez mog elementa (sporedna gornja)


;(defun dLevo(v k play table)
 ;              (if (or (equal (granice (+ v 1) (- k 1)) T) 
 ;                      (equal (slpolje (+ v 1) (- k 1) table) T)
  ;                     (equal (provera (+ v 1) play) T)
 ;                      (not(equal (kojafigura (+ v 1) (- k 1)) play table))
 ;                      ) 0 (1+ (dDesno (+ v 1) (- k 1) play table)) 
                        
 ;                  )
 ; )
;;dLevo vraca broj elemenata sa donje leve dijagonale bez mog elementa(sopredna donja)
(DEFUN GVERTIKALA (V K PLAY table)
                (IF (OR (EQUAL (GRANICE (- V 1) K) T)
                        (EQUAL (SLPOLJE (- V 1) K table) T)
                        (EQUAL (PROVERA (- V 1) PLAY) T)
                        (NOT (EQUAL (KOJAFIGURA (- V 1) K table) PLAY )))
                    0
                  (1+ (GVERTIKALA (- V 1) K PLAY table))))


(DEFUN DVERTIKALA (V K PLAY table)
                (IF (OR (EQUAL (GRANICE (+ V 1) K) T)
                        (EQUAL (SLPOLJE (+ V 1) K table) T)
                        (EQUAL (PROVERA (+ V 1) PLAY) T)
                        (NOT (EQUAL (KOJAFIGURA (+ V 1) K table) PLAY )))
                    0
                  (1+ (DVERTIKALA (+ V 1) K PLAY table))))
(DEFUN vertikala (V K PLAY table)
                (OR (> (gvertikala V K PLAY table) 3)
                    (> (+ (gvertikala V K PLAY table) (dvertikala V K PLAY table)) 3)))

(DEFUN PLAYJOIN (V K PLAY table)
                (OR (EQUAL (GLAVNADIJ V K PLAY table) T)
                    (EQUAL (SPOREDNADIJ V K PLAY table) T) ;fali ti ova fja
                    (EQUAL (VERTIKALA V K PLAY table) T)))
;;proverava sve dijagonale vraca 0 ako nema pobednika,vraca vecu vrednost od 0 ako ima pobednika


;;za sporednu gornja desno dijagonala
(defun dLevo(v k play table)
               (if (or (equal (granice (+ v 1) (- k 1)) T) 
                       (equal (slpolje (+ v 1) (- k 1) table) T)
                       (equal (provera (+ v 1) play) T)
                        (not(equal (kojafigura (+ v 1) (- k 1) table) play ))
                       ) 0 (1+ (dLevo (+ v 1) (- k 1) play table)) 
                        
                   )
  )
;;donja leva spojene cine sporednu dijagonalu
(DEFUN SPOREDNADIJ (V K PLAY table)
                (OR (> (DLEVO V K PLAY table) 3)
                    (> (+ (DLEVO V K PLAY table) (GDESNO V K PLAY table)) 3)))