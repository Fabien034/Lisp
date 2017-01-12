
(vl-load-com)
(setq *AcDoc* (vla-get-activeDocument (vlax-get-acad-object)))

;;;										
;;;	MCUR (routine passe courant un calque créé si inexistant)		
;;;										

(defun MCUR (layname laycol /)
  (if (not (tblsearch "LAYER" layname))
    (entmake (append
	       '((0 . "LAYER")
		 (100 . "AcDbSymbolTableRecord")
		 (100 . "AcDbLayerTableRecord")
		 (70 . 0)
		 (370 . -3)
		 (6 . "Continuous")
		)
	       (list (cons 62 laycol))
	       (list (cons 2 layname))
	     )
    )
  )
  (setvar "clayer" layname)
  (setvar "cecolor" "DuCalque")
)


;;;				  C:LU						
;;;										
;;;				LINTEAU						
;;;										


(defun c:lu (/ p1 p2 p3 ang oldos)
(setq p1 (getpoint)
      p2 (getcorner p1)
      p3 (inters p1 p2 (list (car p1) (cadr p2)) (list (car p2) (cadr p1))))

(if (> (abs (- (car p1) (car p2))) (abs (- (cadr p1) (cadr p2))))
  (setq ang (angle p1 (list (car p2) (cadr p1))))
  (setq ang (angle p1 (list (car p1) (cadr p2))))
  )
(setq ang (cvunit ang "radians" "degrés"))
  (and (> ang 180) (setq ang (- ang 180)))
(setvar "clayer" "0 SEGE ST PRO P - Poutre béton G")
  (setq oldos (getvar "osmode"))
  (setvar "osmode" 0)
(command "rectangle" p1 p2)
  (MCUR "0 SEGE ST PRO P - Poutre béton T" 7)
(command "texte" "j" "mc" p3 ang "L")
  (setvar "osmode" oldos)
  (setvar "clayer" "0")
  )

;;;				C:TREMIE					
;;;										
;;;			  Dessin d'une tremie					
;;;										




(defun c:tremie ( / p1 p2 oldos)

  (setq p1 (getpoint )
      p2 (getcorner p1)
	oldos (getvar "osmode")
	oldlay (getvar "clayer"))

  (setvar "osmode" 0)
  (setvar "clayer" "0 SEGE ST PRO P - Dalle G")

  (command "rectang" p1 p2)

  (setvar "clayer" "0 SEGE ST PRO P - Dalle H")

  (command "polylign"
	   p1
	   (list (car p2)(cadr p1))
	   (polar p1 (-(/ pi 4)) (/ (min (-(car p2)(car p1)) (-(cadr p1)(cadr p2))) 5))
	   (list (car p1)(cadr p2))
	   "c")
  (setvar "cecolor" "RGB:168,168,168")
  (command "hachures" "s" (entlast) "")


  (setvar "osmode" oldos)
  (setvar "clayer" oldlay)
  (setvar "cecolor" "DUCALQUE")
  )


;;;				    C:POT					
;;;										
;;;				POTEAU (POT)					
;;;										


(defun c:pot (/	choix p1 p2 base hauteur clayer	pt1 pt2	pt3 pt4	lst
	      dimpot calque)
  (vla-StartUndoMark *AcDoc*)  
  (setvar "cmdecho" 0)
  (setvar "HPANNOTATIVE" 0)
  (initget 1 "Centre Haut Droit Bas Gauche")
  (setq	choix	(getkword
		  "\nJustification Milieu [Centre/Haut/Droit/Bas/Gauche] <Centre> :"
		)
	p1	(getpoint "\Centre du rectangle :")
	p2	(getcorner p1 "\Dimensions du rectangle")
	base	(abs (- (car p2) (car p1)))
	hauteur	(abs (- (cadr p2) (cadr p1)))
	clayer	(getvar "clayer")
	lst	(list 4 10 5 1 6 0.01)
	calque	"0 SEGE ST PRO P - Poteau béton G"
  ) ;_setq
  (defun makerec (pt1 pt2 pt3 pt4 calque /)
    (entmake
      (append
	'((0 . "LWPOLYLINE")
	  (100 . "AcDbEntity")
	  (100 . "AcDbPolyline")
	  (90 . 4)
	  (70 . 1)
	 )
	(list (cons 8 calque))
	(list (cons 10 pt1))
	(list (cons 10 pt2))
	(list (cons 10 pt3))
	(list (cons 10 pt4))
      ) ;_append
    ) ;_entmake
    (princ)
  ) ;_defun makerec
  (if (= choix "Centre")
    (setq pt1 (list (- (car p1) (/ base 2)) (+ (cadr p1) (/ hauteur 2)))
	  pt2 (list (+ (car p1) (/ base 2)) (+ (cadr p1) (/ hauteur 2)))
	  pt3 (list (+ (car p1) (/ base 2)) (- (cadr p1) (/ hauteur 2)))
	  pt4 (list (- (car p1) (/ base 2)) (- (cadr p1) (/ hauteur 2)))
    )
  ) ;_if "Centre"
  (if (= choix "Haut")
    (setq pt1 (list (- (car p1) (/ base 2)) (cadr p1))
	  pt2 (list (+ (car p1) (/ base 2)) (cadr p1))
	  pt3 (list (+ (car p1) (/ base 2)) (- (cadr p1) hauteur))
	  pt4 (list (- (car p1) (/ base 2)) (- (cadr p1) hauteur))
    )
  ) ;_if "Haut"
  (if (= choix "Bas")
    (setq pt1 (list (- (car p1) (/ base 2)) (+ (cadr p1) hauteur))
	  pt2 (list (+ (car p1) (/ base 2)) (+ (cadr p1) hauteur))
	  pt3 (list (+ (car p1) (/ base 2)) (cadr p1))
	  pt4 (list (- (car p1) (/ base 2)) (cadr p1))
    )
  ) ;_if "Bas"
  (if (= choix "Droit")
    (setq pt1 (list (- (car p1) base) (+ (cadr p1) (/ hauteur 2)))
	  pt2 (list (car p1) (+ (cadr p1) (/ hauteur 2)))
	  pt3 (list (car p1) (- (cadr p1) (/ hauteur 2)))
	  pt4 (list (- (car p1) base) (- (cadr p1) (/ hauteur 2)))
    )
  ) ;_if "Droite"
  (if (= choix "Gauche")
    (setq pt1 (list (car p1) (+ (cadr p1) (/ hauteur 2)))
	  pt2 (list (+ (car p1) base) (+ (cadr p1) (/ hauteur 2)))
	  pt3 (list (+ (car p1) base) (- (cadr p1) (/ hauteur 2)))
	  pt4 (list (car p1) (- (cadr p1) (/ hauteur 2)))
    )
  ) ;_if "Gauche"
  (makerec pt1 pt2 pt3 pt4 calque)
  (MCUR "0 SEGE ST PRO P - Poteau béton H" (vla-get-color (vla-item *layers* "0 SEGE ST PRO P - Poteau béton G")))
  (setvar "hplayer" ".") 
  (command "_bhatch"
	   "an"
	   "n"
	   "p"
	   "ansi31"
	   (/ 0.1 (getvar "cannoscalevalue"))
	   0
	   "s"
	   (entlast)
	   ""
	   ""
  )
  (while lst
    (if	(= (car lst) (getvar "insunits"))
      (setq dimpot (strcat "P "
			   (rtos (/ base (cadr lst)) 2 0)
			   "x"
			   (rtos (/ hauteur (cadr lst)) 2 0)
		   )
	    lst	   nil
      )
      (setq lst (cddr lst))
    ) ;_if
  ) ;_while
  (MCUR "0 SEGE ST PRO P - Poteau béton T" (vla-get-color (vla-item *layers* "0 SEGE ST PRO P - Poteau béton G")))
  (command "_text"
	   "j"
	   "bg"
	   (list (car pt2) (- (cadr pt2) (/ hauteur 2)))
	   0
	   dimpot
	   ""
  )
  (setvar "clayer" clayer)
  (setvar "cmdecho" 1)
  (vla-EndUndoMark *AcDoc*)
  (princ)
) ;_defun c:pot


;;;				C:CCAL						
;;;										
;;;			** Ferraillage mini d'1 poteau **			
;;;		*** créé le 18/06/08 par Vincent Dufournaud ***			
;;;										

(defun c:ccal (/ l e elist p1 P2 o p a Al At p2 fimax)
  (if (and (< (setq c (getvar "insunits")) 7) (> c 3))
    (cond ((= c 4) (setq c 1000))
	  ((= c 5) (setq c 100))
	  ((= c 6) (setq c 1))
    )
    (setq c "")
  )
  (while (not e)
    (setq e (car (entsel "selection du poteau : ")))
  )
  (setq	elist (entget e)
	p1    (cdr (assoc 10 elist))
	elist (vl-remove (assoc 10 elist) elist)
	p2    (cdr (assoc 10 elist))
	elist (vl-remove (assoc 10 elist) elist)
	l     (/ (distance p1 p2) c)
	o     (vlax-ename->vla-object e)
	p     (/ (vla-get-length o) c)
	a     (/ (vla-get-area o) (* c c))
	Al    (min (* 500 a) (max (* 20 a) (* 4 p)))
  )
  (while (setq p2 (cdr (assoc 10 elist)))
    (setq elist (vl-remove (assoc 10 elist) elist))
    (if	(assoc 10 elist)
      (or (< l (setq l1 (/ (distance p2 (cdr (assoc 10 elist))) c)))
	  (setq l l1)
      )
      (or (< l (setq l1 (/ (distance p1 p2) c))) (setq l l1))
    )
  )


  (alert
    (strcat
      "ARMATURES LONGITUDINALES\n\nAl = "
      (rtos Al 2 2)
      " cm² tous les "
      (rtos (* 100 (min (+ l 0.1) 0.4)) 2 2)
      " cm maxi"
    )
  )
  (initget 2 "6 8 10 12 14 16 18 20 22 24")
  (setq
    fimax (getkword "choix du Ømax [6/8/10/12/14/16/18/20/22/24] :")
  )
  (alert
    (strcat "ARMATURES TRANSVERSALES\n\nØ >  "
	    (rtos (/ (atof fimax) 3) 2 0)
	    " tous les "
	    (rtos (min (* 15 (/ (atof fimax) 10)) 40 (* 100 (+ l 0.1))) 2 0)
	    " cm"
    )
  )

)					;defun



;;;					C:TR					
;;;										
;;;			Création des traces au niveau inférieur			
;;;										




(defun c:tr (/ ss p1 p2 ech n e tx)


  (command "_.undo" "d")
  
  (setq ss (ssget '((-4 . "<OR")
		    (-4 . "<AND")
		    (0 . "HATCH")
		    (8 . "0 SEGE ST PRO P - Mur H")
		    (-4 . "AND>")
		    (-4 . "<AND")
		    (0 . "HATCH")
		    (8 . "0 SEGE ST PRO P - Poteau béton H")
		    (-4 . "AND>")
		    (-4 . "<AND")
		    (0 . "INSERT")
		    (8 . "0 SEGE ST PRO P - Mur G")
		    (-4 . "AND>")
		    (-4 . "<AND")
		    (0 . "LWPOLYLINE")
		    (8 . "0 SEGE ST PRO P - Poteau béton G")
		    (-4 . "AND>")
		    (-4 . "<AND")
		    (0 . "TEXT,MTEXT")
		    (8 . "0 SEGE ST PRO P - Poteau béton T")
		    (-4 . "AND>")
		    (-4 . "OR>")
		    )))    
  
  (setq p1 (getpoint "Choisissez un point sur le niveau courant :")
	p2 (getpoint p1 "Choisissez un point sur le point de destination :"))

  (setq n (1- (sslength ss)))

  (while (not ech)    
    (cond ((equal '(2 . "ANSI32") (assoc 2 (entget (ssname ss n))))
	   (setq ech (* 0.5 (cdr (assoc 41 (entget (ssname ss n)))))))
	  ((or (equal '(2 . "ANSI31") (assoc 2 (entget (ssname ss n)))) (equal '(2 . "ANSI37") (assoc 2 (entget (ssname ss n)))))
	   (setq ech (* (/ 1.0 3) (cdr (assoc 41 (entget (ssname ss n))))))))
    (setq n (1- n)))    

  (repeat (setq n (sslength ss))
    (command "_.copy" (setq e (ssname ss (setq n (1- n)))) "" p1 p2)       
    (cond ((equal '(8 . "0 SEGE ST PRO P - Mur H") (assoc 8 (entget e)))
	   (progn
	     (entmod (subst '(8 . "0 SEGE ST PRO P - Traces H") (assoc 8 (entget (entlast))) (entget (entlast))))
	     (command "_.hatchedit" (entlast) "P" "AR-CONC" (* 0.05 (cdr (assoc 41 (entget e)))) 0)))
	  ((equal '(8 . "0 SEGE ST PRO P - Mur G") (assoc 8 (entget e)))
	   (entmod (subst '(8 . "0 SEGE ST PRO P - Traces G") (assoc 8 (entget (entlast))) (entget (entlast)))))
	  ((equal '(8 . "0 SEGE ST PRO P - Poteau béton G") (assoc 8 (entget e)))
	   (entmod (subst '(8 . "0 SEGE ST PRO P - Traces G") (assoc 8 (entget (entlast))) (entget (entlast)))))
	  ((and (equal '(8 . "0 SEGE ST PRO P - Poteau béton H") (assoc 8 (entget (ssname ss n)))) (equal '(2 . "ANSI31") (assoc 2 (entget (ssname ss n)))))
	   (progn
	     (entmod (subst '(8 . "0 SEGE ST PRO P - Traces H") (assoc 8 (entget (entlast))) (entget (entlast))))
	     (command "_.hatchedit" (entlast) "P" "ANSI33" (* 2 (cdr (assoc 41 (entget e)))) 90)))
;;;	  ((and (equal '(8 . "0 SEGE ST PRO P - Poteau béton H") (assoc 8 (entget (ssname ss n)))) (equal '(2 . "SOLID") (assoc 2 (entget (ssname ss n)))))
;;;	   (progn
;;;	     (entmod (subst '(8 . "0 SEGE ST PRO P - Traces H") (assoc 8 (entget (entlast))) (entget (entlast))))
;;;	     (command "_.hatchedit" (entlast) "P" "ANSI33" ech 90)
;;;	     (command "_.hatchedit" (entlast) "c" "p" "")
;;;	     (entmod (subst '(8 . "0 SEGE ST PRO P - Traces H") (assoc 8 (entget (entlast))) (entget (entlast))))))
	  ((equal '(0 . "TEXT") (assoc 0 (entget e)))
	   (progn
	     (entmod (subst '(8 . "0 SEGE ST PRO P - Traces T") (assoc 8 (entget (entlast))) (entget (entlast))))	     
	     (if (or (= (substr (cdr (assoc 1 (entget e))) 1 1)  "P") (= (substr (cdr (assoc 1 (entget e))) 1 1)  "R"))
	     (entmod (subst (cons 1 (strcat "Am. " (cdr (assoc 1 (entget e))))) (assoc 1 (entget (entlast))) (entget (entlast)))))	       
	       ))
	  ((equal '(0 . "MTEXT") (assoc 0 (entget e)))	   	     	     
	     (if (= (substr (cdr (assoc 1 (entget (entlast)))) 1 1)  "P")
	       (progn
	       (vla-put-textstring (vlax-ename->vla-object (entlast)) (strcat "Am. " (cdr (assoc 1 (entget (entlast))))))
	       (vla-put-layer (vlax-ename->vla-object (entlast)) "0 SEGE ST PRO P - Traces T"))
	       (vla-put-layer (vlax-ename->vla-object (entlast)) "0")
	     ))
	  );_cond    
    );_repeat

  

  (command "_.undo" "f")

  
  
  );_defun

;;;				C:LI	C:LO	C:LP				
;;;										
;;;				Commandes de cotation				
;;;										

(defun c:li ()
  (vla-StartUndoMark *AcDoc*)
  (setq clayer (getvar "clayer")
	cecolor	(getvar "cecolor"))
  (MCUR "0 SEGE ST PRO P - Cotation G" 253)
  (command "_dimlinear" pause pause pause)
  (setvar "clayer" clayer)
  (setvar "cecolor" cecolor)
  (vla-EndUndoMark *AcDoc*)
)

(defun c:lo ()
  (setq	clayer	(getvar "clayer")
	cecolor	(getvar "cecolor"))
  (MCUR "0 SEGE ST PRO P - Cotation G" 253)
  (command "_dimaligned" pause pause pause)
  (setvar "clayer" clayer)
  (setvar "cecolor" cecolor)
  (vla-EndUndoMark *AcDoc*)
)

(defun c:lp ()
  (vla-StartUndoMark *AcDoc*)
  (setq	clayer	(getvar "clayer")
	cecolor	(getvar "cecolor"))
  (MCUR "0 SEGE ST PRO P - Cotation G" 253)
  (command "_dimcontinue"
	   (while (not (zerop (getvar "cmdactive")))
	     (command pause)
	   )
  )
  (setvar "clayer" clayer)
  (setvar "cecolor" cecolor)
  (vla-EndUndoMark *AcDoc*)
)

