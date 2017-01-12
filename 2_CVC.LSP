;;; Lisp perso CVC - V02 07.11.2016 ;;;


;;; A FAIRE ;;;

(princ "Rajouter à c:elsyst pourvoir suprimer un un element de système")
(princ "Rajouter la posibilité d'une gestion des système par fenetre en crabe")

;;; lisp fonctionnant avec les routines list de Gilles Chanteau		
;;; charger routine list.LSP						

(or rt-list (setq rt-list (load "routine list")))

;;; chargement des variables
(load "0_Config")

;;;                          c:addsyst							
;;;        Creation des calques CVC pour les systeme					
;;; Format normalisé SEGE: 0 SEGE CVC PRO P - SYST"nb" TYPE "variable" ELEMENT TYPE-ENT	
;;;											

(defun c:addsyst (/ lst-ss lst-chx lst-dxf lst-deslay lst-layname)
  ;; création d'un jeu de selection
  (setq ss (ssget))
  ;; création d'une liste jeu de selection, un ss pour les calques SEGE, un ss pour les autres
  ;; si pas de selection lst-ss nil
  (if ss
    (progn
      (setq lst-ss (list (ssadd) (ssadd)))
      ;; vérifie si les claques des object selectionés sont normalisés SEGE
      (repeat (setq n (sslength ss))
	(if
	  (fr:rt-veriflay
	    (cdr
	      (assoc 8
		     (entget (setq ent (ssname ss (setq n (1- n)))))
	      )
	    )
	  )
	   (ssadd ent (nth 0 lst-ss))
	   (ssadd ent (nth 1 lst-ss))
	)
      )
    )
    (setq lst-ss nil)
  )

  (setq	chx-nb "0"
	chx-societe
	 "OTEIS"
	chx-lot	"CVC"
	chx-statut
	 "PRO"
	chx-plan "P"
  )
  (initget 1 "V H")
  (setq	chx-cvc-syst
	 (getkword "\nChoix [Ventilation/Hydraulique] : <V> ")
  )
  (if (= chx-cvc-syst "")
    (setq chx-cvc-syst "V")
  )
  (setq lst-chx nil)
  (cond	((equal chx-cvc-syst "V")
	 (progn
	   (setq lst-chx      (reverse (foreach	x cvc-syst-ventil
					 (setq lst-chx (cons (car x) lst-chx))
				       )
			      )
		 str-cvc-syst "Ventilation"
	   )
	 )
	)
	((equal chx-cvc-syst "H")
	 (progn
	   (setq lst-chx (reverse (foreach x cvc-syst-hydro
				    (setq lst-chx (cons (car x) lst-chx))
				  )
			 )
		 str-cvc-syst "Hydraulique"
	   )
	 )
	)
  )
  (initget 1 (gc:lst2str lst-chx " "))
  (setq
    chx-syst (getkword
	       (strcat "\nChoix [" (gc:lst2str lst-chx "/") "] : ")
	     )
  )

  (setq	layname	(strcat	chx-nb	   " "	      chx-societe
			" "	   chx-lot    " "	 chx-statut
			" "	   chx-plan   " - "	 chx-syst
			" G"
		       )
  )

  (setq	lst-dxf	(nth 2
		     (nth (vl-position chx-syst lst-chx)
			  (if (equal chx-cvc-syst "V")
			    cvc-syst-ventil
			    cvc-syst-hydro
			  )
		     )
		)
  )
  (if lst-dxf
    (setq laycolor (list (assoc 62 lst-dxf)))
    (setq laycolor (acad_truecolordlg 7))
  )
  (mkcur layname laycolor)

  ;; passe les éléments du jeu de selection dans le calque
  (if lst-ss
    (progn
      ;; si il existe traitement du premier jeu de selection
      (if (/= (sslength (nth 0 lst-ss)) 0)
	(repeat	(setq n (sslength (nth 0 lst-ss)))
	  (setq	ent	   (ssname (nth 0 lst-ss) (1- n))
		lst-deslay (fr:rt-lst-deslay ent)
	  )
	  (if (/= 1 lst-deslay)
	    (progn
	      (if (distof (substr (car lst-deslay) (strlen (car lst-deslay))))
		(setq
		  str-syst (substr (car lst-deslay) 1 (- (strlen (car lst-deslay)) 2))
		)
		(setq str-syst (car lst-deslay))
	      )	      
	      (if (equal (fr:rt-syst-ventil-or-hydro str-syst) str-cvc-syst)
		(progn
		  (setq	lst-layname (gc:str2lst layname " ")
			laycolor    (vla-get-TrueColor
				      (vla-item	(vla-get-layers *acdoc*)
						layname
				      )
				    )
			layLinetype (vla-get-Linetype
				      (vla-item	(vla-get-layers *acdoc*)
						layname
				      )
				    )
			nw-layname
				    (gc:lst2str
				      (reverse
					(append
					  (list	(car (reverse lst-layname))
					  )
					  (reverse (cdr lst-deslay))
					  (cdr (reverse lst-layname))
					)
				      )
				      " "
				    )
		  )
		  (if (not (tblsearch "LAYER" nw-layname))
		    (progn
		      (setq nw-layer (vla-add *layers* nw-layname))
		      (vla-put-truecolor nw-layer laycolor)
		      (vla-put-Linetype nw-layer layLinetype)
		      (vla-put-layer
			(vlax-ename->vla-object ent)
			nw-layname
		      )
		    )
		    (vla-put-layer
		      (vlax-ename->vla-object ent)
		      nw-layname
		    )
		  )
		)			;progn  vrai
		(vla-put-layer
		  (vlax-ename->vla-object ent)
		  layname
		)
	      )				;if
	    )				;progn vrai
	    ;; mettre l'element dans le calque courant
	    (vla-put-layer
	      (vlax-ename->vla-object ent)
	      layname
	    )
	  )				;if
	  (setq n (1- n))
	)				;repeat
      )					;if (/= (sslength (nth 0 lst-ss)) 0)
      ;; si il existe traitement du deuxième jeu de selection
      (if (/= (sslength (nth 1 lst-ss)) 0)
	(repeat	(setq n (sslength (nth 1 lst-ss)))
	  ;; mettre l'element dans le calque courant
	  (vla-put-layer
	    (vlax-ename->vla-object (ssname (nth 1 lst-ss) (1- n)))
	    layname
	  )
	  (setq n (1- n))
	)				;repeat
      )					;if (/= (sslength (nth 1 lst-ss)) 0)
    )					;progn
  )					;if lst-ss
  (princ)
) ;_defun


;;;                    c:nbsyst					
;;; Ajoute un numéro de système aprés la désignation principal	
;;;								

(defun c:nbsyst	(/ lst-syst lst-layname)
  (setq	ss (ssget)
	n  nil
  )
  ;; vérifie si les claques des object selectionés sont normalisés
  (repeat (setq n (sslength ss))
    (if
      (not
	(fr:rt-veriflay
	  (cdr
	    (assoc 8 (entget (setq ent (ssname ss (setq n (1- n))))))
	  )
	)
      )
       (ssdel ent ss)
    )
  )
  ;; liste avec les différent type de système

  (setq lst-syst (acad_strlsort (fr:rt-lst-syst ss)))

  ;; Si plusieur choix ecrit dans la ligne de commande les différent choix
  (if (and lst-syst (/= (length lst-syst) 1))
    (progn
      ;; ecrit dans la ligne de commande les différent choix
      (setq nb 0
	    verif-chx 0
      )
      (foreach x lst-syst
	(print (strcat "(" (rtos (setq nb (1+ nb))) ")" x))
      )
      (setq chx-syst	(nth
			  (1-
			    (while
			      (not (and	(> verif-chx 0)
					(<= verif-chx (length lst-syst))
				   )
			      )
			       (setq
				 verif-chx (getint
					     "\nChoisir le système : "
					   )
			       )
			    )
			  )
			  lst-syst
			)
	    verif-chx	"verif-chx"
	    chx-nb-syst	(while (not (or	(equal "" verif-chx)
					(and (> (distof verif-chx) 0)
					     (> (strlen verif-chx) 0)
					     (<= (strlen verif-chx) 3)
					)
				    )
			       )
			  (setq
			    verif-chx
			     (getstring
			       "\nentrez le numéro du système : "
			     )
			  )
			)
      )
      (if (= 1 (strlen chx-nb-syst))
	(setq chx-nb-syst (strcat "0" chx-nb-syst))
      )
      ;; suprime les entitées qui n'ont pas CHX dans le nom de leur calque
      (repeat (setq n (sslength ss))
	(if
	  (not
	    (vl-string-search
	      (if (equal (substr chx-syst (strlen chx-syst)) "*")
		(substr chx-syst 1 (1- (strlen chx-syst)))
		(strcat chx-syst " ")
	      )
	      (vla-get-layer
		(vlax-ename->vla-object
		  (setq ent (ssname ss (1- n)))
		)
	      )
	    )
	  )
	   (ssdel ent ss)
	)
	(setq n (1- n))
      )
    )
    (progn
      (setq chx-syst	(car lst-syst)
	    verif-chx	"verif-chx"
	    chx-nb-syst	(while (not (or	(equal "" verif-chx)
					(and (> (distof verif-chx) 0)
					     (> (strlen verif-chx) 0)
					     (<= (strlen verif-chx) 3)
					)
				    )
			       )
			  (setq
			    verif-chx
			     (getstring
			       "\nentrez le numéro du système : "
			     )
			  )
			)
      )
      (if (= 1 (strlen chx-nb-syst))
	(setq chx-nb-syst (strcat "0" chx-nb-syst))
      )
    )
  )
  ;; création des variables
  (repeat (setq n (sslength ss))
    (setq ent	      (ssname ss (1- n))
	  layname     (vla-get-layer (vlax-ename->vla-object ent))
	  laycolor    (vla-get-TrueColor
			(vla-item (vla-get-layers *acdoc*) layname)
		      )
	  layLinetype (vla-get-Linetype
			(vla-item (vla-get-layers *acdoc*) layname)
		      )
	  lst-layname (gc:str2lst layname " ")
	  str-syst    (car (fr:rt-lst-deslay ent))
    )
    (if	(distof (substr str-syst (strlen str-syst)))
      (setq nw-str-syst
	     (strcat (substr str-syst 1 (- (strlen str-syst) 2))
		     chx-nb-syst
	     )
      )
      (setq nw-str-syst (strcat str-syst chx-nb-syst))
    )
    (if	(/= str-syst nw-str-syst)
      (progn
	(setq nw-layname
	       (gc:lst2str
		 (gc:SubstAt nw-str-syst 6 lst-layname)
		 " "
	       )
	)
	(if (not (tblsearch "LAYER" nw-layname))
	  (progn
	    (setq nw-layer (vla-add *layers* nw-layname))
	    (vla-put-truecolor nw-layer laycolor)
	    (vla-put-Linetype nw-layer layLinetype)
	    (vla-put-layer (vlax-ename->vla-object ent) nw-layname)
	  )
	  (vla-put-layer (vlax-ename->vla-object ent) nw-layname)
	)
      )
    )
    (setq n (1- n))
  )
  (princ)
) ;defun

;;;                    c:tpsyst					
;;; Ajoute un type de système aprés la désignation principal	
;;;	choix suivant système ventilation ou hydraulique	
			

(defun c:tpsyst	(/ lst-type-syst lst-chx lst-syst lst-layname)
  (setq	ss (ssget)
	n  nil
  )
  ;; vérifie si les claques des object selectionés sont normalisés
  (repeat (setq n (sslength ss))
    (if
      (not
	(fr:rt-veriflay
	  (cdr
	    (assoc 8 (entget (setq ent (ssname ss (setq n (1- n))))))
	  )
	)
      )
       (ssdel ent ss)
    )
  )
  ;; liste avec les différent type de système

  (setq lst-syst (acad_strlsort (fr:rt-lst-syst ss)))

  ;; Si plusieur choix ecrit dans la ligne de commande les différent choix
  (if (and lst-syst (/= (length lst-syst) 1))
    (progn
      ;; ecrit dans la ligne de commande les différent choix
      (setq nb 0
	    verif-chx 0
      )
      (foreach x lst-syst
	(print (strcat "(" (rtos (setq nb (1+ nb))) ")" x))
      )
      (setq chx-syst (nth
		       (1-
			 (while
			   (not	(and (> verif-chx 0)
				     (<= verif-chx (length lst-syst))
				)
			   )
			    (setq
			      verif-chx	(getint
					  "\nChoisir le système : "
					)
			    )
			 )
		       )
		       lst-syst
		     )
      )
      ;; donne une liste de type suivant le système selectionné
      (if (distof (substr chx-syst (strlen chx-syst)))
	(setq str-syst (substr chx-syst 1 (- (strlen str-syst) 2)))
	(if (equal (substr chx-syst (strlen chx-syst)) "*")
	  (setq str-syst(substr chx-syst 1 (1- (strlen chx-syst))))
	  (setq str-syst chx-syst)
	)
      )
      (setq type-syst (fr:rt-syst-ventil-or-hydro str-syst))
      (cond ((equal type-syst "Ventilation")
	     (progn
	       (setq lst-chx   nil
		     lst-chx   (reverse	(foreach x cvc-type-syst-ventil
					  (setq
					    lst-chx (cons (car x) lst-chx)
					  )
					)
			       )
	       )
	       (initget 1 (gc:lst2str lst-chx " "))
	       (setq chx-type-syst
		      (nth 1
			   (nth	(vl-position
				  (getkword
				    (strcat "\nChoix ["
					    (gc:lst2str lst-chx "/")
					    "] : "
				    )
				  )
				  lst-chx
				)
				cvc-type-syst-ventil
			   )
		      )
	       )
	     )
	    )
	    ((equal type-syst "Hydraulique")
	     (progn
	       (setq lst-chx   nil
		     lst-chx   (reverse	(foreach x cvc-type-syst-hydro
					  (setq
					    lst-chx (cons (car x) lst-chx)
					  )
					)
			       )
	       )
	       (initget 1 (gc:lst2str lst-chx " "))
	       (setq chx-type-syst
		      (nth 0
			   (nth	(vl-position
				  (getkword
				    (strcat "\nChoix ["
					    (gc:lst2str lst-chx "/")
					    "] : "
				    )
				  )
				  lst-chx
				)
				cvc-type-syst-hydro
			   )
		      )
	       )
	     )
	    )
      )					; cond

      ;; suprime les entitées qui n'ont pas chx-syst dans le nom de leur calque
      (repeat (setq n (sslength ss))
	(if
	  (not
	    (vl-string-search
	      (if (equal (substr chx-syst (strlen chx-syst)) "*")
		(substr chx-syst 1 (1- (strlen chx-syst)))
		(strcat chx-syst " ")
	      )
	      (vla-get-layer
		(vlax-ename->vla-object
		  (setq ent (ssname ss (1- n)))
		)
	      )
	    )
	  )
	   (ssdel ent ss)
	)
	(setq n (1- n))
      )
    )					;progn
    (progn
      (setq chx-syst (car lst-syst))
      ;; donne une liste de type suivant le système selectionné
      (if (distof (substr chx-syst (strlen chx-syst)))
	(setq str-syst (substr chx-syst 1 (- (strlen str-syst) 2)))
	(setq str-syst chx-syst)
      )
      (setq type-syst (fr:rt-syst-ventil-or-hydro str-syst))
      (cond ((equal type-syst "Ventilation")
	     (progn
	       (setq type-syst "Ventilation"
		     lst-chx   nil
		     lst-chx   (reverse	(foreach x cvc-type-syst-ventil
					  (setq
					    lst-chx (cons (car x) lst-chx)
					  )
					)
			       )
	       )
	       (initget 1 (gc:lst2str lst-chx " "))
	       (setq chx-type-syst
		      (nth 1
			   (nth	(vl-position
				  (getkword
				    (strcat "\nChoix ["
					    (gc:lst2str lst-chx "/")
					    "] : "
				    )
				  )
				  lst-chx
				)
				cvc-type-syst-ventil
			   )
		      )
	       )
	     )
	    )
	    ((equal type-syst "Hydraulique")
	     (progn
	       (setq type-syst "Hydraulique"
		     lst-chx   nil
		     lst-chx   (reverse	(foreach x cvc-type-syst-hydro
					  (setq
					    lst-chx (cons (car x) lst-chx)
					  )
					)
			       )
	       )
	       (initget 1 (gc:lst2str lst-chx " "))
	       (setq chx-type-syst
		      (nth 0
			   (nth	(vl-position
				  (getkword
				    (strcat "\nChoix ["
					    (gc:lst2str lst-chx "/")
					    "] : "
				    )
				  )
				  lst-chx
				)
				cvc-type-syst-hydro
			   )
		      )
	       )
	     )
	    )
      )
    )
  )
  ;; création des variables
  (repeat (setq n (sslength ss))
    (setq ent		(ssname ss (1- n))
	  layname	(vla-get-layer (vlax-ename->vla-object ent))
	  laycolor	(vla-get-TrueColor
			  (vla-item (vla-get-layers *acdoc*) layname)
			)
	  layLinetype	(vla-get-Linetype
			  (vla-item (vla-get-layers *acdoc*) layname)
			)
	  lst-layname	(gc:str2lst layname " ")
	  str-type-syst	(cadr (fr:rt-lst-deslay ent))
    )
    (cond ((equal type-syst "Ventilation")
	   (setq lst-type-syst
		  (reverse (foreach x cvc-type-syst-ventil
			     (setq lst-type-syst
				    (cons (nth 1 x) lst-type-syst)
			     )
			   )
		  )
	   )
	  )
	  ((equal type-syst "Hydraulique")
	   (setq lst-type-syst
		  (reverse (foreach x cvc-type-syst-hydro
			     (setq lst-type-syst
				    (cons (nth 0 x) lst-type-syst)
			     )
			   )
		  )
	   )
	  )
    )					;cond
    (if	(and (not (equal chx-type-syst "Sans type"))
	     (not (equal chx-type-syst "S"))
	)
      (progn
	;; création du nom du calque de destination
	(if (vl-position str-type-syst lst-type-syst)
	  (setq	nw-layname
		 (gc:lst2str
		   (gc:SubstAt chx-type-syst 7 lst-layname)
		   " "
		 )
	  )
	  (setq	nw-layname
		 (gc:lst2str
		   (gc:insertAt chx-type-syst 7 lst-layname)
		   " "
		 )
	  )
	)
      )					;progn
      (progn
	(if (vl-position str-type-syst lst-type-syst)
	  (setq	nw-layname
		 (gc:lst2str
		   (vl-remove str-type-syst lst-layname)
		   " "
		 )
	  )
	  (setq nw-layname layname)
	)
      )					;progn
    )					;if


    ;; Création du calque de destination
    (if	(/= layname nw-layname)
      (progn
	(if (not (tblsearch "LAYER" nw-layname))
	  (progn
	    (setq nw-layer (vla-add *layers* nw-layname))
	    (vla-put-truecolor nw-layer laycolor)
	    (vla-put-Linetype nw-layer layLinetype)
	    (vla-put-layer (vlax-ename->vla-object ent) nw-layname)
	  )
	  (vla-put-layer (vlax-ename->vla-object ent) nw-layname)
	)
      )
    )
    (setq n (1- n))
  )
  (princ)
)					;defun

;;;                    c:elsyst					
;;; Ajoute un type de système aprés la désignation principal	
;;;	choix suivant liste cvc-type-syst-ventil

(defun c:elsyst
       (/ lst-element-syst lst-chx lst-syst lst-layname lst-deslay)
  (setq	lst-element-syst nil
	lst-chx	nil
	lst-syst nil
	lst-layname nil
  )
  (setq	ss (ssget)
	n  nil
  )
  ;; vérifie si les claques des object selectionés sont normalisés
  (repeat (setq n (sslength ss))
    (if
      (not
	(fr:rt-veriflay
	  (cdr
	    (assoc 8 (entget (setq ent (ssname ss (setq n (1- n))))))
	  )
	)
      )
       (ssdel ent ss)
    )
  )
  ;; liste avec les différent type de système

  (setq lst-syst (acad_strlsort (fr:rt-lst-syst ss)))

  ;; Si plusieur choix ecrit dans la ligne de commande les différent choix
  (if (and lst-syst (/= (length lst-syst) 1))
    (progn
      ;; ecrit dans la ligne de commande les différent choix
      (setq nb 0
	    verif-chx 0
      )
      (foreach x lst-syst
	(print (strcat "(" (rtos (setq nb (1+ nb))) ")" x))
      )
      (setq chx-syst	     (nth
			       (1-
				 (while
				   (not	(and (> verif-chx 0)
					     (<= verif-chx (length lst-syst))
					)
				   )
				    (setq
				      verif-chx	(getint
						  "\nChoisir le système : "
						)
				    )
				 )
			       )
			       lst-syst
			     )
	    lst-chx	     nil
	    lst-element-syst nil
	    lst-chx	     (reverse (foreach x cvc-element-syst
					(setq lst-chx (cons (car x) lst-chx))
				      )
			     )
	    lst-element-syst (reverse (foreach x cvc-element-syst
					(setq lst-element-syst
					       (cons (cadr x)
						     lst-element-syst
					       )
					)
				      )
			     )
      )
      (initget 1 (gc:lst2str lst-chx " "))
      (setq chx-element-syst
	     (nth 1
		  (nth (vl-position
			 (getkword
			   (strcat "\nChoix ["
				   (gc:lst2str lst-element-syst "/")
				   "] : "
			   )
			 )
			 lst-chx
		       )
		       cvc-element-syst
		  )
	     )
      )


      ;; suprime les entitées qui n'ont pas chx-element-syst dans le nom de leur calque
      (repeat (setq n (sslength ss))
	(if
	  (not
	    (vl-string-search
	      (if (equal (substr chx-syst (strlen chx-syst)) "*")
		(substr chx-syst 1 (1- (strlen chx-syst)))
		(strcat chx-syst " ")
	      )
	      (vla-get-layer
		(vlax-ename->vla-object
		  (setq ent (ssname ss (1- n)))
		)
	      )
	    )
	  )
	   (ssdel ent ss)
	)
	(setq n (1- n))
      )
    )					;progn
    (progn
      (setq chx-syst (car lst-syst)
	    lst-chx nil
	    lst-element-syst
	     nil
	    lst-chx
	     (reverse
	       (foreach	x cvc-element-syst
		 (setq lst-chx (cons (car x) lst-chx))
	       )
	     )
	    lst-element-syst
	     (reverse (foreach x cvc-element-syst
			(setq lst-element-syst
			       (cons (cadr x)
				     lst-element-syst
			       )
			)
		      )
	     )
      )
      (initget 1 (gc:lst2str lst-chx " "))
      (setq chx-element-syst
	     (nth 1
		  (nth (vl-position
			 (getkword
			   (strcat "\nChoix ["
				   (gc:lst2str lst-element-syst "/")
				   "] : "
			   )
			 )
			 lst-chx
		       )
		       cvc-element-syst
		  )
	     )
      )
    )
  )
  ;; création des variables
  (repeat (setq n (sslength ss))
    (setq ent		   (ssname ss (1- n))
	  layname	   (vla-get-layer (vlax-ename->vla-object ent))
	  laycolor	   (vla-get-TrueColor
			     (vla-item (vla-get-layers *acdoc*) layname)
			   )
	  layLinetype	   (vla-get-Linetype
			     (vla-item (vla-get-layers *acdoc*) layname)
			   )
	  lst-layname	   (gc:str2lst layname " ")
	  str-element-syst (car (reverse (fr:rt-lst-deslay ent)))
    )
    ;; création du nom du calque de destination    
    (if	(vl-position str-element-syst lst-element-syst)
      (setq nw-layname
	     (gc:lst2str
	       (gc:SubstAt
		 chx-element-syst
		 (- (length lst-layname) 2)
		 lst-layname
	       )
	       " "
	     )
      )
      (setq nw-layname
	     (gc:lst2str
	       (gc:insertAt
		 chx-element-syst
		 (1- (length lst-layname))
		 lst-layname
	       )
	       " "
	     )
      )
    )
    ;; Création du calque de destination
    (if	(/= layname nw-layname)
      (progn
	(if (not (tblsearch "LAYER" nw-layname))
	  (progn
	    (setq nw-layer (vla-add *layers* nw-layname))
	    (vla-put-truecolor nw-layer laycolor)
	    (vla-put-Linetype nw-layer layLinetype)
	    (vla-put-layer (vlax-ename->vla-object ent) nw-layname)
	  )
	  (vla-put-layer (vlax-ename->vla-object ent) nw-layname)
	)
      )
    )
    (setq n (1- n))
  )					;repeat
)					;defun

;;; Routine pour charger les types de ligne CVC

(defun fr:loadlinetype (/)
  (setq	lin	    (vla-get-linetypes *acdoc*)
	lstLineType (list "AXESX2") ;;indiquez les types de ligne
	addLineType 1
  )
  (foreach l lstLineType
    (setq n 0
	  addLineType 1)
    (while (<= n (1- (vla-get-count lin)))
      (if (= (vla-get-Name (vla-item lin n)) l)
	(setq addLineType 0
	      n		  (vla-get-count lin)
	)
	(setq n (1+ n))
      ) ;if
    ) ;while
    (if	addLineType
      (and (vl-catch-all-error-p
	     (vl-catch-all-apply 'vla-item (list lin l))
	   )
	   (vla-load lin l (findfile "acad.lin"))
      )
    ) ;if
  ) ;foreach
) ;defun




;;; 			fr:attlaycvc				
;;; donne les couleurs epaisseur et type de ligne au calque CVC	
;;;								

(defun fr:attlaycvc (/)
  (setq	lay	  (cdr (assoc 2 (tblnext "layer" T))))
  (while (setq lay (cdr (assoc 2 (tblnext "layer"))))
    (if	(= "CVC" (nth 2 (gc:str2lst lay " ")))
      (cond ((=	"Axe"
		(nth (- (length (gc:str2lst lay " ")) 2)
		     (gc:str2lst lay " "))
		)	    
	     (progn
	       (if (setq lstDxf (nth 2 (nth 5 cvc-element-syst))) ;nth 5 = Axe
		 (setq colorIndex (cdr (assoc 62 lstDxf))
		       lineType	  (cdr (assoc 6 lstDxf))
		 )
		 (setq colorIndex (acad_truecolordlg 7))
	       )
	       (setq layColor (vlax-create-object
				(strcat	"AutoCAD.AcCmColor."
					(substr (vlax-product-key) 28 2)
				)
			      )
		     l      (vla-item (vla-get-layers *acdoc*) lay)
	       )
	       (vla-put-colorindex layColor colorIndex)
	       (vla-put-truecolor l layColor)
	       (fr:loadlinetype)
	       (vla-put-Linetype l lineType)
	     )				;progn
	    )
	    ((=	"Annotation"
		(nth (- (length (gc:str2lst lay " ")) 2)
		     (gc:str2lst lay " "))
		)	    
	     (progn
	       (if (setq lstDxf (nth 2 (nth 6 cvc-element-syst))) ;nth 6 = Annotation
		 (setq colorIndex (cdr (assoc 62 lstDxf)))
		 (setq colorIndex (acad_truecolordlg 7))
	       )
	       (setq layColor (vlax-create-object
				(strcat	"AutoCAD.AcCmColor."
					(substr (vlax-product-key) 28 2)
				)
			      )
		     l      (vla-item (vla-get-layers *acdoc*) lay)
	       )
	       (vla-put-colorindex layColor colorIndex)
	       (vla-put-truecolor l layColor)	       
	     )				;progn
	    )
	    ((=	"CCF"
		(nth (- (length (gc:str2lst lay " ")) 2)
		     (gc:str2lst lay " "))
		)	    
	     (progn
	       (if (setq lstDxf (nth 2 (nth 4 cvc-element-syst))) ;nth 4 = CCF
		 (setq colorIndex (cdr (assoc 62 lstDxf)))
		 (setq colorIndex (acad_truecolordlg 7))
	       )
	       (setq layColor (vlax-create-object
				(strcat	"AutoCAD.AcCmColor."
					(substr (vlax-product-key) 28 2)
				)
			      )
		     l      (vla-item (vla-get-layers *acdoc*) lay)
	       )
	       (vla-put-colorindex layColor colorIndex)
	       (vla-put-truecolor l layColor)	       
	     )				;progn
	    )
	    (T
	     (progn
	       (setq n 0)
	       (repeat (1- (length cvc-type-syst-ventil))
		 (if (=	(nth 1 (nth n cvc-type-syst-ventil))
			(nth 7 (gc:str2lst lay " "))
		     )
		   (progn		     
		     (if (setq lstDxf (nth 2 (nth n cvc-type-syst-ventil)))
		       (if (= (nth 3 (gc:str2lst lay " ")) "EDL")
			 (setq colorIndex (+ (cdr (assoc 62 lstDxf)) 3))
			 (setq colorIndex (cdr (assoc 62 lstDxf)))
		       )
		       (setq colorIndex (acad_truecolordlg 7))
		     )
		     (setq
		       layColor	(vlax-create-object
				  (strcat
				    "AutoCAD.AcCmColor."
				    (substr (vlax-product-key) 28 2)
				  )
				)
		     )
		     (vla-put-colorindex layColor colorIndex)
		     (vla-put-truecolor
		       (vla-item (vla-get-layers *acdoc*) lay)
		       layColor
		     )
		   )			;progn
		 )			;if
		 (setq n (+ n 1))
	       )			;repeat
	     )				;progn
	    )
      )					; cond 
    )					; if   
  )					; while	
)					;defun attlaycvc



;;;                  c:xd			
;;;   ajoute un débit en Xdata a un terminal	
;;;						

(defun c:xd (/)

  ;; met toutes les variables a nil
  ;; A METTRE EN ARGUMENT DU DEFUN UNE FOIS LE CODE FINI
  (setq	ent nil
	ent-txt	nil
	deb nil
	xdata-deb nil
	nw-xdata-ent
	 nil
	lst-layname
	 nil
	nw-layname
	 nil
  )
  ;; Selection d'une entitée et demande le débit
  (setq	ent (car (entsel))
					; vérifie si l'entitée à une xdata "xdebit"
	deb (if	(setq xdata-deb
		       (cdr (assoc
			      1040
			      (cdadr (assoc -3 (entget ent '("xdebit"))))
			    )
		       )
		)
	      (getreal (strcat "\nDébit <" (rtos xdata-deb 2 2) ">:"))
					;vrai
	      (getreal "\nDébit :")	;Faux
	    )
  )

  (if xdata-deb
    ;; Si l'entitée à une xdata "xdebit"
    (progn
      ;; recherche dans l'espace object l'entitée avec le handle
      ;; indiquée dans le "xdebit" de l'entitée selectionnée	
      (vlax-for	x *mspace*
	(if (equal
	      (vla-get-handle x)
	      (cdr
		(assoc
		  1005
		  (cdadr (assoc -3 (entget ent '("xdebit"))))
		)
	      )
	    )
	  ;; si on trouve une entitée avec le même handle que celui indiqué dans le xdata 1005 "xdébit"
	  (setq ent-txt x)
	)
      )
      ;;vérifie que les deux entitée soit lié par leur xdata
      (if ent-txt
	(if
	  (equal
	    (vla-get-handle (vlax-ename->vla-object ent))
	    (cdr
	      (assoc
		1005
		(cdadr (assoc -3
			      (entget (vlax-vla-object->ename ent-txt)
				      '("xdebit")
			      )
		       )
		)
	      )
	    )
	  )
	   (progn
	     ;; modifier le texte existant avec la valeur deb
	     (vla-put-TextString ent-txt deb)
	     ;; mettre a jour les xdata
	     ;; créer une list xdata pour le terminal
	     (fr:rt-maj-terminaux-xdebit
	       (vla-get-handle ent-txt)	;handle
	       deb			;débit
	       ent			;entitée
	     )
	   )
	   (progn
	     (setq nw-xdata-ent T)
	   )
	)
	;; si on ne trouve pas une entitée avec le même handle que celui indiqué dans le xdata 1005 "xdébit"
	;; L'entitée n'existe plus il faut la recréer
	(progn
	  (setq nw-xdata-ent T)
	  (alert
	    "l'entité référence a été supprimé\nIl faut la réinsérer !!!!"
	  )
	)				;progn Faux
      )					;if ent-txt
    )					;progn Vrai
    ;; Si l'entitée n'a pas une xdata "xdebit", on la crée
    (setq nw-xdata-ent T)
  )					;if xdata-deb
  ;; créer une xdata "xdebit" sur l'entitée selectionnée
  (if nw-xdata-ent
    (progn
      ;; créer un texte avec deb
      (setq nw-mtext
	     (vla-addmtext
	       *mspace*
	       (vla-GetPoint *util* nil "\nPoint d'insertion: ")
	       2
	       deb
	     )
      )
      (vla-put-stylename nw-mtext ".Iso 2")

      ;; création du nom pour le calque
      (setq layname	(vla-get-layer (vlax-ename->vla-object ent))
	    lst-layname	(gc:str2lst layname " ")
	    nw-layname
			(if (or	(equal (car (reverse lst-layname)) "G")
				(equal (car (reverse lst-layname)) "H")
				(equal (car (reverse lst-layname)) "T")
			    )
			  (gc:lst2str
			    (subst "D"
				   (car (reverse lst-layname))
				   lst-layname
			    )
			    " "
			  )
			  (gc:lst2str
			    (reverse (cons "D" (reverse lst-layname)))
			    " "
			  )
			)
      )
      (if (not (tblsearch "LAYER" nw-layname))
	(progn
	  (setq nw-layer (vla-add *layers* nw-layname))
	  (vla-put-truecolor
	    nw-layer
	    (vla-get-TrueColor
	      (vla-item *layers* layname)
	    )
	  )
	  (vla-put-layer nw-mtext nw-layname)
	)
	(vla-put-layer nw-mtext nw-layname)
      )
      ;; Mise à jour des xdata pour le terminal
      (fr:rt-maj-terminaux-xdebit
	(vla-get-handle nw-mtext)	;handle
	deb				;débit
	ent				;entitée
      )

      ;; créer une list xdata pour le text
      (setq
	xdata
	 (list
	   -3
	   (list "xdebit"
		 (cons 1000 "Debit")
		 (cons 1002 "{")
		 (cons 1005
		       (vla-get-handle (vlax-ename->vla-object ent))
		 )
		 (cons 1002 "}")
	   )
	 )
      )
      ;; lier les xdata au text
      (entmod (append (entget (vlax-vla-object->ename nw-mtext))
		      (list xdata)
	      )
      )

      ;; Reacteur sur nw-mtext pour mttre a jour le xdebit du terminal
      (setq reac-mtext
	     (vlr-Object-reactor
	       (list nw-mtext)
	       (vlax-ename->vla-object ent)
	       '((:vlr-modified . reac-maj-debit))
	     )
      )
      (vlr-pers reac-mtext)
    )					;progn
  )					;if nw-xdata-ent
  (princ)
)					;defun

;;; routine fr:rt-maj-terminaux-xdebit						
;;; Met à jour les xdata d'un terminal						
;;; argument: 	Handle du Texte							
;;;		Débit en chiffre						
;;;		Entitée								

(defun fr:rt-maj-terminaux-xdebit (hand deb ent)
  ;; créer une application
  (setq app (regapp "xdebit"))
  ;; créer une list xdata pour le terminal
  (setq
    xdata (list	-3
		(list "xdebit"		      
		      (cons 1000 "Debit")
		      (cons 1002 "{")
		      (cons 1005 hand)
		      (cons 1040 deb)
		      (cons 1002 "}")
		)
	  )
  )
  ;; lier les xdata à l'entitée terminal
  (entmod (append (entget ent) (list xdata)))
)

;;; Reacteur pour la mise a jour d'un terminal si modification de texte
(defun reac-maj-debit (notifier reacteur arg-list)
  (if (and (vlr-data reacteur)
	   (vlax-write-enabled-p (vlr-data reacteur))
	   (vlax-read-enabled-p notifier)
      )
    (fr:rt-maj-terminaux-xdebit
      (vla-get-handle notifier)		;handle
      (distof (vla-get-textstring notifier)) ;débit
      (vlax-vla-object->ename (vlr-data reacteur)) ;entitée
    )
  )
)

;;;                  c:cumul			
;;;   cumul les débits en Xdata des terminaux	
;;;						
(defun c:cumul (/ insert-deb file-name
		  path dcl-id)  
(setq lst-result-deb nil
      lst-syst nil
      lst-syst-ss nil
      lst-type nil
)  
  (setq	ss (ssget)
	n  nil
	all-chx-syst nil
  )
  ;; vérifie type de calque
  (repeat (setq n (sslength ss))
    (if
      (not (and
	     ;; vérifie si les claques des object selectionés sont normalisés
	     (fr:rt-veriflay
	       (cdr
		 (assoc 8 (entget (ssname ss (1- n))))
	       )
	     )
	     ;; suprime les entitées qui n'ont pas un debit indiqué
	     (cdr
	       (assoc
		 1040
		 (cdadr
		   (assoc -3
			  (entget (ssname ss (1- n))
				  '("xdebit")
			  )
		   )
		 )
	       )
	     )
	     ;; suprime les entitées qui ne sont pas sur un calque terminaux
	     (vl-string-search
	       "Terminaux"
	       (vla-get-layer
		 (vlax-ename->vla-object
		   (ssname ss (1- n))
		 )
	       )
	     )
	   )
      )
       (progn
	 (ssdel (ssname ss (1- n)) ss)
	 (setq n (1- n))
       )
       (setq n (1- n))
    )
  )
  ;; Vérifie qu'il reste une entitée dans la liste
  (if (/= (sslength ss) 0)
    (progn

      ;; Choix type de Systeme
      (setq lst-syst (acad_strlsort (fr:rt-lst-syst ss)))

      ;; Si plusieur choix ecrit dans la ligne de commande les différent choix
      (if (and lst-syst (/= (length lst-syst) 1))
	(progn
	  (setq nb 0
		verif-chx 0)
	  (foreach x lst-syst
	    (print (strcat "(" (rtos (setq nb (1+ nb))) ")" x))
	  )
	  (setq	chx-syst
		 (nth
		   (1- (while
			 (not (and (> verif-chx 0) (<= verif-chx (length lst-syst))))
			  (setq
			    verif-chx	(getint "\nChoisir le système : ")
			  )
		       )
		   )
		   lst-syst
		 )
	  )
	  (if (equal (substr chx-syst (strlen chx-syst))
		     "*"
	      )
	    (setq all-chx-syst T)	    
	  )
	  ;; suprime les entitées qui n'ont pas chx-syst dans le nom de leur calque
	  (repeat (setq n (sslength ss))
	    (if
	      (not
		(vl-string-search
		  (if all-chx-syst
		    (substr chx-syst 1 (- (strlen chx-syst) 2))
		    (strcat chx-syst " ")
		  )
		  (vla-get-layer
		    (vlax-ename->vla-object
		      (setq ent (ssname ss (1- n)))
		    )
		  )
		)
	      )
	       (ssdel ent ss)
	    )
	    (setq n (1- n))
	  )
	)
      )

      (if (not all-chx-syst)
	(progn
	  ;; Choix du type de systeme  
	  (repeat (setq n (sslength ss))
	    (foreach x cvc-type-syst-ventil
	      (cond
		((vl-position
		   (cadr x)
		   (fr:rt-lst-deslay (ssname ss (1- n)))
		 )
		 (setq lst-type
			(gc:distinct
			  (cons
			    (cadr x)
			    lst-type
			  )
			)
		 )
		)
	      )
	    )
	    (setq n (1- n))
	  )
	  ;; Si plusieur choix ecrit dans la ligne de commande les différent choix
	  (if (and lst-type (/= (length lst-type) 1))
	    (progn
	      (setq nb 0)
	      (foreach x lst-type
		(print (strcat "(" (rtos (setq nb (1+ nb))) ")" x))
	      )
	      (setq chx-type
		     (nth (1- (getint "\nChoisir le système : "))
			  lst-type
		     )
	      )
	      ;; suprime les entitées qui n'ont pas chx-type dans le nom de leur calque
	      (repeat (setq n (sslength ss))
		(if
		  (not
		    (vl-string-search
		      chx-type
		      (vla-get-layer
			(vlax-ename->vla-object
			  (setq ent (ssname ss (1- n)))
			)
		      )
		    )
		  )
		   (ssdel ent ss)
		)
		(setq n (1- n))
	      )
	    )
	  )
	)
      )


      ;; Calcul le débit du jeu de selection      
      (if all-chx-syst
	(progn
	  ;; supprime de lst-syst les système avec * et qui ne sont pas dans le choix
	  (foreach x lst-syst
	    (if	(or (equal (substr x (strlen x))
			   "*"
		    )
		    (not (vl-string-search
			   (substr chx-syst 1 (1- (strlen chx-syst)))
			   x
			 )
		    )
		)
	      (setq lst-syst (vl-remove x lst-syst))
	    )
	  )
	  ;; tri les jeux de selection par type dans une liste 
	  (foreach x lst-syst	    
	    (setq lst-syst-ss (cons (setq name-ss (ssadd)) lst-syst-ss))
	    (repeat (setq n (sslength ss))
	      (if
		(vl-string-search
		  (strcat x " ")
		  (vla-get-layer
		    (vlax-ename->vla-object
		      (setq ent (ssname ss (1- n)))
		    )
		  )
		)
		 (ssadd ent name-ss)
	      )
	      (setq n (1- n))
	    )
	  )
	  (setq lst-syst-ss (reverse lst-syst-ss))
	  ;; Par type de système
	  (foreach xss lst-syst-ss
	    ;; création de la liste type
	    (setq lst-type nil)
	    (repeat (setq n (sslength xss))
	      (foreach x cvc-type-syst-ventil
		(cond
		  ((vl-position
		     (cadr x)
		     (fr:rt-lst-deslay (ssname xss (1- n)))
		   )
		   (setq lst-type
			  (gc:distinct
			    (cons
			      (cadr x)
			      lst-type
			    )
			  )
		   )
		  )
		)
	      )
	      (setq n (1- n))
	    )				;repeat création lst-type
	    (if	lst-type
	      (progn
		(foreach x lst-type
		  ;; création d'un nouveau jeu de selection
		  (setq ss-type (ssadd))
		  (repeat (setq n (sslength xss))
		    (ssadd (ssname xss (1- n)) ss-type)
		    (setq n (1- n))	
		  )		  
		  ;; supprime les entitée qui n'ont pas chx-type
		  (repeat (setq n (sslength ss-type))
		    (if
		      (not
			(vl-string-search
			  x
			  (vla-get-layer
			    (vlax-ename->vla-object
			      (setq ent (ssname ss-type (1- n)))
			    )
			  )
			)
		      )
		       (ssdel ent ss-type)
		    )
		    (setq n (1- n))
		  )
		  ;; envoi le jeu de selection par type en calcul
		  (setq result-deb (fr:rt-calcul-debit ss-type))
		  ;; création d'une liste avec tous les résesultas
		  (setq	lst-result-deb
			 (cons
			   (list
			     (nth (vl-position xss lst-syst-ss)
				  lst-syst
			     )
			     x
			     result-deb
			   )
			   lst-result-deb
			 )
		  )
		)			;foreach lst-type
	      )
	      (progn
		;; envoi le jeu de selection par type en calcul
		(setq result-deb (fr:rt-calcul-debit xss))
		;; création d'une liste avec tous les résesultas
		(setq lst-result-deb
		       (cons
			 (list
			   (nth	(vl-position xss lst-syst-ss)
				lst-syst
			   )
			   "Sans type"
			   result-deb
			 )
			 lst-result-deb
		       )
		)
	      )
	    )				;if lst-type
	  ) ;foreach lst-syst-ss
	  
	  (setq	lst-result-deb (reverse lst-result-deb)
		result-deb     nil
	  )
	  (print lst-result-deb)
	)				;progn
	(print (setq result-deb (fr:rt-calcul-debit ss)))
      )

      (princ)
    )					;progn general
    (alert
      "Aucune entitée selectionnée\nne correspond pour un calcul de débit"
    )
  )					;if

  (if insert-deb
    (progn
      (setq nw-mtext
	     (vla-addmtext
	       *mspace*
	       (vla-GetPoint *util* nil "\nPoint d'insertion: ")
	       2
	       (strcat result-deb " m³/h")
	     )
      )
      (vla-put-stylename nw-mtext ".Iso 2")
    )
  )
  (princ)
)					;defun


;;; routine fr:rt-lst-deslay							
;;; retourne  une list avec les désignations d'un calque d'une entitée		
;;; ou d'une liste claque (normalisation)					
;;; argument: 	entitée sous forme <Nom d'entité: ID> (setq x (car(entsel)))	
;;;		ou liste calque sous forme (normalisation)			

(defun fr:rt-lst-deslay	(x / lst-layname)
  (cond	((not (listp x))
	 (setq
	   x (gc:str2lst (vla-get-layer (vlax-ename->vla-object x)) " ")
	 )
	)
  )
  (reverse (cdr (reverse (cdr (member "-" x)))))
)

;;; routine fr:rt-lst-syst							
;;; retourne  une list avec les différent système d'un jeu de selection		
;;; argument: Jeu de selection							
;;;										
;;; Si un système a un numéro retourne nom-syst*				

(defun fr:rt-lst-syst (ss / lst-syst)
  (repeat (setq n (sslength ss))
    (setq ent	   (ssname ss (1- n))
	  str-syst (car (fr:rt-lst-deslay ent))
    )
    (if	(distof (substr str-syst (strlen str-syst)))
      (progn
	(setq
	  lst-syst (gc:distinct
		     (cons str-syst lst-syst)
		   )
	  lst-syst (gc:distinct
		     (cons
		       (strcat
			 (substr str-syst 1 (- (strlen str-syst) 2))
			 "*"
		       )
		       lst-syst
		     )
		   )
	)
      )
      (setq lst-syst (gc:distinct (cons str-syst lst-syst))
      )
    )
    (setq n (1- n))
  )
  (if (= (length lst-syst) 2)
    (foreach x lst-syst
      (if (equal (substr x (strlen x)) "*")
	(setq lst-syst (vl-remove x lst-syst))
      )
    )
  )
  lst-syst
) ;defun

;;; routine fr:rt-calcul-debit							
;;; Retourne le cumul des débit d'un jeu de selection				
;;; argument: Jeu de selection							

(defun fr:rt-calcul-debit (ss / n deb lst-deb)
  ;; Calcul le débit du jeu de selection
  (repeat (setq n (sslength ss))
    (if	(setq deb
	       (cdr
		 (assoc
		   1040
		   (cdadr (assoc -3 (entget (ssname ss (1- n)) '("xdebit")))
		   )
		 )
	       )
	)
      (setq lst-deb (cons deb lst-deb))
    )
    (setq n (1- n))
  )
  (strcat (rtos (apply '+ lst-deb) 2 2))
)

;;; routine fr:rt-syst-ventil-or-hydro						
;;; retourne si le nom du système et un système de ventilation ou d'hydrolique	
;;; argument: str-syst "nom du système" 					

(defun fr:rt-syst-ventil-or-hydro (str-syst / type-syst)
  (cond	((vl-position
	   0
	   (mapcar '(lambda (x) (vl-position str-syst x))
		   cvc-syst-ventil
	   )
	 )
	 (setq type-syst "Ventilation")
	)
	((vl-position
	   0
	   (mapcar '(lambda (x) (vl-position str-syst x))
		   cvc-syst-hydro
	   )
	 )
	 (setq type-syst "Hydraulique")
	)
  )
  type-syst
)

;;; ALIAS CVC ;;;

(defun c:sa () (c:addsyst))
(defun c:st () (c:tpsyst))
(defun c:se () (c:elsyst))
(defun c:gl () (fr:attlaycvc))
































































































































































