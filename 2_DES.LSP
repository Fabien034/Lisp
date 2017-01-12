;;; routine 	GetOrCreateTableStyle doit être chargé depuis fichier 1_tableau.lsp - Argument "nom du style			
;;; routine 	rt_addtab doit être chargé depuis fichier 1_tableau.lsp								
;;;		- Argument "liste des cellules" "nom du tableau" "Hauteur texte" "longueur cellule" "largeur cellule"		
;;; routine 	rt_addlaycol doit être chargé depuis fichier 1_Calques.lsp 	- Argument "nom du calque" "couleur du calque"	

(defun c:circ (/	 *error*   layname   laycolor  laycour
	       pt	 contpline longpline lst       u
	       lst-coef	 xunit	   coef	     ent-last 
	      )

  ;; gestion des erreur
  (defun *error* (msg)
    (princ (strcat "\nErreur: " msg))
    (setvar "clayer" laycour)
    (vla-EndUndoMark *acdoc*)
    (command-s "_undo" "")
    (terpri)
  )


  ;; Vérifie si la variable "insunits" est définie sur Metre Centimetre ou Milimetre

  (cond	((vl-every '(lambda (x) (/= x (getvar "insunits"))) '(4 5 6))
	 (progn	(alert "l'unité de dessin n'est pas réglée")
		(command "DDUNITS")
	 )
	)
  )

  (vla-StartUndoMark *acdoc*)


  ;; création du calque "0 SEGE CVC PRO P - DES Circualtion G"
  (setq
    layname  "0 SEGE CVC PRO P - DES Circualtion G"
    laycolor '((62 . 7))
    laycour  (if
	       (not (equal (vla-get-name (vla-get-ActiveLayer *acdoc*))
			   layname
		    )
	       )
		(vla-get-name (vla-get-ActiveLayer *acdoc*))
		laycour
	     )
  )
  (if (not (tblsearch "LAYER" layname))	;Verifie si calque existe
    (progn (vla-add *layers* layname)
	   (rt_addlaycol layname laycolor)
					;donne la couleur au calque par la routine rt_addlaycol
	   (setvar "clayer" layname)
    )					;progn vrai
    (progn (setvar "clayer" layname)
    )					;progn faux
  )

  ;; création de la polyligne contour circulation
  (setq	ent-last  (entlast)
	contpline (entlast)
  )
  (while (equal contpline ent-last)
    (progn
      (setq pt (getpoint "\nPoint de départ du contour :")
      )
      (princ "\n Points suivants")
      (command "_pline"
	       pt
	       (while (not (zerop (getvar "cmdactive")))
		 (command pause)
	       )
      )
      (setq contpline (entlast)
	    longpline (entlast)
      )
      (if
	(equal :vlax-false
	       (vla-get-closed (vlax-ename->vla-object contpline))
	)
	 (vla-put-closed (vlax-ename->vla-object contpline) -1)
      )
    )
  )

  ;;création d'une polyligne pour longueur circulation
  (while (equal contpline longpline)
    (progn
      (setq pt (getpoint "\nPoint de départ pour la longueur :"))
      (princ "\n Points suivants")
      (command "_pline"
	       pt
	       (while (not (zerop (getvar "cmdactive")))
		 (command pause)
	       )
      )
      (setq longpline (entlast))
    )
  )

  ;;création de liste pour création du tableau    
  (setq	u	 "m"
	lst-coef
		 (cdr (car (vl-remove-if-not
			     '(lambda (x) (= (car x) (getvar "insunits")))
			     (list (list 4 1 0.1 0.001)
				   (list 5 10 1 0.01)
				   (list 6 1000 100 1)
			     )
			   )
		      )
		 )
	coef	 (cond ((= u "m") (caddr lst-coef))
		       ((= u "cm") (cadr lst-coef))
		       ((= u "mm") (car lst-coef))
		 )
  )					;Coefficient pour afficher les résultats en mètre 

  (setq	lst
	 (list
	   (list
	     "Surface"
	     (strcat
	       "%<\\AcObjProp.16.2 Object(%<\\_ObjId "
	       (itoa
		 (vla-get-objectid (vlax-ename->vla-object contpline))
	       )
	       ">%).Area \\f \"%lu2%pr2%ps[, "
	       u
	       "²]%ct8["
	       (rtos (* coef coef) 1)
	       "]\">%"
	     )
	   )
	   (list
	     "Longueur"
	     (strcat
	       "%<\\AcObjProp.16.2 Object(%<\\_ObjId "
	       (itoa
		 (vla-get-objectid (vlax-ename->vla-object longpline))		 
	       )
	       ">%).Length \\f \"%lu2%pr2%ps[, "
	       u
	       "]%ct8["
	       (rtos coef 1)
	       "]\">%"
	     )
	   )
	   (list "Largeur"
		 (strcat "%<\\AcExpr (B2/B3) \\f \"%lu2%pr2%ps[, "
			 u
			 "]%ct8["
			 (rtos coef 1)
			 "]\">%"
		 )
	   )
	 )
  )

  ;; création d'un style de texte

  (setq	Xunit		  (cond	((= 4 (getvar "insunits")) 100)
				((= 5 (getvar "insunits")) 10)
				((= 6 (getvar "insunits")) 0.1)
			  )
	htxt		  (* Xunit 1.5)
					; Hauteur Texte - Mettre la valeur en cm sur papier pour une echelle 1:100
	nw-style-text	  (vla-Add (vla-get-TextStyles *acdoc*)
				   (strcat "isocp " (rtos htxt 2))
			  )
	active-style-text (vla-get-ActiveTextStyle *acdoc*)
  )
  (vla-put-height nw-style-text htxt)
  (vla-put-fontFile nw-style-text "isocp.shx")
  (vla-put-ActiveTextStyle *acdoc* nw-style-text)

  ;;routine pour Récupèrer ou créer un un style de tableau
  (GetOrCreateTableStyle (strcat "Tab_" (vla-get-name nw-style-text)))

  (vla-put-ActiveTextStyle *acdoc* active-style-text)
  ;;création du tableau par la routine rt_addtab					
  (rt_addtab lst			;"liste des cellules" 
	     "Circulation"		;"nom du tableau" 
	     htxt			;"Hauteur textee" 
	     (* Xunit 10)		;"longueur cellule" 
	     (* Xunit 1.2)		;"largeur cellule"
  )

  (setvar "clayer" laycour)

  (vla-EndUndoMark *acdoc*)

  (princ)
)					;defun


