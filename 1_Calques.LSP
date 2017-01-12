(vl-load-com)
(setq *AcDoc* (vla-get-activeDocument (vlax-get-acad-object)))

;;; routine fr:rt-veriflay			
;;; Vérifier si un calque est normalisé 	
;;; Retourne T ou nil				
;;; argument: nom du calque sous forme str	


(defun fr:rt-veriflay (layname /)
  (and (equal (nth 1 (gc:str2lst layname " ")) entreprise )
       (equal (nth 5 (gc:str2lst layname " ")) "-")
  )
)  


;;;**************************************************************************;;;
;;;	MKCUR (routine passe courant un calque créé si inexistant)	     ;;;
;;;**************************************************************************;;;

(defun mkcur (layname laycol /)
  (if (not (tblsearch "LAYER" layname))
    (entmake (append
	       '((0 . "LAYER")
		 (100 . "AcDbSymbolTableRecord")
		 (100 . "AcDbLayerTableRecord")
		 (70 . 0)
		 (370 . -3)
		 (6 . "Continuous")
		)
	       laycol
	       (list (cons 2 layname))
	     )
    )
  )
  (setvar "clayer" layname)
  (setvar "cecolor" "DuCalque")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   routine Donne une couleur à un calque       ;;;
;;;   Suivant couleur 255 ou couleur vrai         ;;;
;;;   Version Fabien 05.12.14		          ;;;
;;;   Argument:	Layname = Nom du calque	          ;;;
;;;   		Laycolor= Couleur du calque       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun rt_addlaycol (layname laycolor / tc)
  (if (setq tc (assoc 420 laycolor))
    (progn
      (setq accmcolor
	     (vla-getinterfaceobject
	       (vlax-get-acad-object)
	       (strcat "AutoCAD.AcCmColor."
		       (itoa (fix (atof (getvar "acadver"))))
	       )
	     )
      )
      (vla-setrgb
	accmcolor
	(lsh (cdr tc) -16)
	(lsh (lsh (cdr tc) 16) -24)
	(lsh (lsh (cdr tc) 24) -24)
      )
      (vla-put-truecolor
	(vla-item *layers* layname)
	accmcolor
      )

      (vla-put-colormethod accmcolor acColorMethodByACI)
      (vla-put-colorindex accmcolor acByLayer)
      (vl-catch-all-apply
	(function (lambda ()
		    (vlax-release-object accmcolor)
		  )
	)
      )
    )					;progn
    (vla-put-color
      (vla-item *layers* layname)
      (cdr (assoc 62 laycolor))
    )
  )					;if tc    
  (command "regen")
)					;defun



;;;				C:ADDLAY					
;;;                                      					
;;;     		Defun calques Normalisé          			
;;;                                      					


(defun c:addlay	()

  (setq nb (getstring "\nNumero : <0> "))
  (setq societe (getstring (strcat "\nSociété : <" entreprise "> ")))
  (initget 1 "ARC CVC Pb ST VRD EL FM")
  (setq lot (getkword "\nLot [ARC/CVC/Pb/FM/ST/VRD/EL] : <ARC> "))
  (initget 1 "PRO Demol Edl")
  (setq statut (getkword "\nStatut [Pro/Demol/Edl] : <Pro> "))
  (initget 1 "P C S")
  (setq plan (getkword "\nChoix [Plan/Coupe/Schema] : <Plan> ")
	desip (getstring "\nDésignation Principale : ")
	desis (getstring T "\nDésignation Secondaire : "))    
  (initget 1 "G H T")
  (setq ght (getkword "\nChoix [Graphisme/Hachure/Texte] : <G> "))

  (if (= nb "")
    (setq nb "0")
  )
  (if (= societe "")
    (setq societe entreprise)
  )
  (if (/= desis "")
    (setq desis (strcat desis " "))
  )

  (setq	layname	(strcat	nb     " "    societe	    " "	   lot
			" "    statut " "    plan   " - "  desip
			" "    desis  ght
		       )
  )
  (setq laycolor (acad_truecolordlg 7))
  
  (mkcur layname laycolor)
  
  (princ)
) ;_defun


;;;				C:ADDLAYP					
;;;                                     					
;;; 			Defun calques de présentations      			
;;;                                     					

(defun c:addlayp (/ lstlay)

  (mkcur "00 Window" (list(cons 62 51)))
  (mkcur "00 Cadre" (list(cons 62 7)))
  
  (setq lstlay (entget (tblobjname "layer" "00 Window")))
  (entmod (subst (cons 290 0) (assoc 290 lstlay) lstlay))
  (princ)
) ;_defun c:addlayp

;;;				C:ADDLAYX					
;;;                                     					
;;;         		Defun calques Xref          				
;;;                                     					

(defun c:addlayx (/ lstlay)

  (setq layname (strcat	"0 Xref " (getstring "\nNom :")))

  (mkcur layname (list(cons 62 7)))  
  
  (princ)
) ;_defun c:addlayx


;;;**************************************************************************;;;
;;; 		Routine gestion des exclusions pour GHT			     ;;;
;;;**************************************************************************;;;
(defun rt-exclusion-ght	(layname /)
  (if (> (strlen layname) 3)
  (or (equal " D" (substr layname (1- (strlen layname))))
					;le calque fini par " D"
      (equal "00" (substr layname 1 2))	;le calque commence par "00"
  )
    nil)
) ;defun

;;;**************************************************************************;;;
;;; 		Routine boucle de gestion de claque   			     ;;;
;;;**************************************************************************;;;

(defun boucle (selset end / n elist ename nom newname fin laylist)
  (repeat (setq n (sslength selset))
    (setq elist	(entget (setq ename (ssname selset (setq n (1- n)))))
	  nom	(cdr (assoc 8 elist))
    )
    (if (not (rt-exclusion-ght nom))
      (progn 
    (if	(> 3 (strlen nom))		;définit le nom du calque où l'entité va être déplacée
      (setq newname (strcat nom end))
      (progn
	(setq fin (substr nom (1- (strlen nom))))
	(if (not (or (= fin " G") (= fin " H") (= fin " T")))
	  (setq newname (strcat nom end))
	  (setq
	    newname (strcat (substr nom 1 (- (strlen nom) 2)) end)
	  )
	) ;_if
      ) ;_progn
    ) ;_if - définit le nom du calque où l'entité va être déplacée
    (if	(not (tblsearch "layer" newname))
      (if (not (tblsearch "layer"
			  (substr newname 1 (- (strlen newname) 2))
	       )
	  )
	(progn
	  (setq	laylist
		 (entget (tblobjname
			   "layer"
			   nom
			 )
		 )
	  )
	  (entmake (subst (cons 2 newname) (assoc 2 laylist) laylist))
	)
	(progn
	  (setq	laylist
		 (entget (tblobjname
			   "layer"
			   (substr newname 1 (- (strlen newname) 2))
			 )
		 )
	  )
	  (entmake (subst (cons 2 newname) (assoc 2 laylist) laylist))
	) ;_progn
      )					;if
    ) ;_if - crée le calque s'il n'existe pas

    (entmod (subst (cons 8 newname) (assoc 8 elist) elist))
					;déplace l'entité sur le bon calque
    (ssdel ename ss)			;soustrait l'entité au jeu de sélection global
    );progn
  );if exclusion
  ) ;_repeat
) ;_defun boucle


;;;				C:V - C:VT - C:VG - C:VH			
;;;				C:B - C:BT - C:BG - C:BH			
;;;                                                         			
;;; 			Déplacement Manuel dans calque Normalisé 		
;;;                                                         			

;;; fonction général Vers ;;;

(defun c:v (/ chx)

  (initget 2 "Graphique Texte Hachure Plan Coupe Edl Pro Demol")
  (setq	chx
	 (getkword
	   "Déplacer dans un calque vers [Graphique/Texte/Hachure/---/PLan/Coupe/---/Edl/PRo/Demol]:<Texte>"
	 )
  )
  (cond	((= chx "Texte") (c:vt))
	((= chx "Graphique") (c:vg))
	((= chx "Hachure") (c:vh))
	((= chx "Plan") (c:vp))
	((= chx "Coupe") (c:vc))
	((= chx "Edl") (c:vedl))
	((= chx "Pro") (c:vpro))
	((= chx "Demol") (c:vdemol))
  ))



(defun c:vt ()
  ;; Vers calque Textes
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (setq ss (ssget))
  (boucle ss " T")
  (vla-EndUndoMark doc)
)

(defun c:vg ()
  ;; Vers calque Graphique
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (setq ss (ssget))
  (boucle ss " G")
  (vla-EndUndoMark doc)
)

(defun c:vh ()
  ;; Vers calque Hachures
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (setq ss (ssget))
  (boucle ss " H")
  (vla-EndUndoMark doc)
)

(defun c:b (/ chx)
  (initget 1 "Graphique Texte Hachure")
  (setq	chx
	 (getkword
	   "Remplacer par un calque [Graphique/Texte/Hachure]:<Texte>"
	 )
  )
  (cond	((= chx "Texte") (c:bt))
	((= chx "Graphique") (c:bg))
	((= chx "Hachure") (c:bh))
  )
)

(defun c:bg (/ e1 ss e1list doc)	;déplace dans le calque Graphique de l'entité de référence
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (while (not e1)
    (setq e1 (car (entsel "\nChoix de l'entité de référence :")))
  )
  (setq ss (ssget))
  (repeat (setq n (sslength ss))
    (setq e1list (entget (ssname ss (setq n (1- n)))))
    (entmod
      (subst (assoc 8 (entget e1)) (assoc 8 e1list) e1list)
    )
  )
  (boucle ss " G")
  (vla-EndUndoMark doc)
) ;_defun

(defun c:bh (/ e1 ss e1list doc)	;déplace dans le calque Hachures de l'entité de référence
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (while (not e1)
    (setq e1 (car (entsel "\nChoix de l'entité de référence :")))
  )
  (setq ss (ssget))
  (repeat (setq n (sslength ss))
    (setq e1list (entget (ssname ss (setq n (1- n)))))
    (entmod
      (subst (assoc 8 (entget e1)) (assoc 8 e1list) e1list)
    )
  )
  (boucle ss " H")
  (vla-EndUndoMark doc)
) ;_defun

(defun c:bt (/ e1 ss e1list doc)	;déplace dans le calque Hachures de l'entité de référence
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (while (not e1)
    (setq e1 (car (entsel "\nChoix de l'entité de référence :")))
  )
  (setq ss (ssget))
  (repeat (setq n (sslength ss))
    (setq e1list (entget (ssname ss (setq n (1- n)))))
    (entmod
      (subst (assoc 8 (entget e1)) (assoc 8 e1list) e1list)
    )
  )
  (boucle ss " T")
  (vla-EndUndoMark doc)
) ;_defun

;;;					C:GHT					
;;;                                                          			
;;;		Déplacement Automatique dans calque Normalisé      		
;;;                                                          			

(defun c:ght (/ ss ss1 ss2)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (initget 1 "T o")
  (setq
    choix (getkword "\nAppliquer la norme sur [Tout/Objet] : <Tout>")
  )
  (if (= choix "T")
    
    (setq ss  (ssget "_X" )
	  ss1 (ssget "_X" '((0 . "HATCH") (410 . "MODEL")))
	  ss2 (ssget "_X"
		     '((-4 . "<OR")
		       (0 . "TEXT")
		       (0 . "MTEXT")
		       (0 . "MULTILEADER")
		       (0 . "LEADER")
		       (-4 . "OR>")
		       (410 . "MODEL")
		      )
	      )
    )
    (progn
      (setq ss	(ssget)
	    ss1	(ssget "_P" '((0 . "HATCH") (410 . "MODEL")))
	    ss2	(ssadd)
      )
      (repeat (setq n (sslength ss))
	(setq typent
	       (cdr
		 (assoc
		   0
		   (entget (setq entname (ssname ss (setq n (1- n)))))
		 )
	       )
	)
	(if (or (= typent "TEXT") (= typent "MTEXT") (= typent "LEADER") (= typent "MULTILEADER"))
	  (ssadd entname ss2)
	)
      )
    ) ;_progn
  ) ;_if
  (if ss1
    (boucle ss1 " H")
  )
  (if ss2
    (boucle ss2 " T")
  )
  (if ss
    (boucle ss " G")
  )
  (vla-EndUndoMark doc)
) ;_defun c:ght

;::					C:phase					
;;;										
;;;				GESTION DES PHAESES				


(defun c:phase (/ chx)
  ;; Vers calque Textes
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (setq	ss  (ssget)
	chx (getstring T "Numéro de la phase :")
  )

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
    )
    (if	(fr:rt-veriflay layname)
      (setq nw-layname
	     (gc:lst2str
	       (gc:SubstAt chx 0 lst-layname)
	       " "
	     )
      )
    )					;if
    (if	(not (tblsearch "LAYER" nw-layname))
      (progn
	(setq nw-layer (vla-add *layers* nw-layname))
	(vla-put-truecolor nw-layer laycolor)
	(vla-put-Linetype nw-layer layLinetype)
	(vla-put-layer (vlax-ename->vla-object ent) nw-layname)
      )
      (vla-put-layer (vlax-ename->vla-object ent) nw-layname)
    )					;if

    (setq n (1- n))
  )					; repeat
  (vla-EndUndoMark doc)
  (princ)
)					; defun


;;;				C:CG - C:CH - C:CT				
;;;                                                          			
;;;		Rend courant le calque ght de l'objet sélectionné  		
;;;                                                          			

(defun mklayght	(nom end / newname fin laylist)
  (if (> 3 (strlen nom))		;définit le nom du calque où l'entité va être déplacée
    (setq newname (strcat nom end))
    (progn
      (setq fin (substr nom (1- (strlen nom))))
      (if (not (or (= fin " G") (= fin " H") (= fin " T")))
	(setq newname (strcat nom end))
	(setq
	  newname (strcat (substr nom 1 (- (strlen nom) 2)) end)
	)
      ) ;_if
    ) ;_progn
  )
  (if (not (tblsearch "layer" newname))
    (if	(not (tblsearch	"layer"
			(substr newname 1 (- (strlen newname) 2))
	     )
	)
      (progn
	(setq laylist
	       (entget (tblobjname
			 "layer"
			 nom
		       )
	       )
	)
	(entmake (subst (cons 2 newname) (assoc 2 laylist) laylist))
      )
      (progn
	(setq laylist
	       (entget (tblobjname
			 "layer"
			 (substr newname 1 (- (strlen newname) 2))
		       )
	       )
	)
	(entmake (subst (cons 2 newname) (assoc 2 laylist) laylist))
      ) ;_progn
    )					;if
  )
  (setvar "clayer" newname)
  (setq laycour newname) ;rajout Fabien Pour defun c:AD
)					;defun

(defun c:cg (/ sellist)
  (setq	sellist	(entget (car (entsel)))
	nom	(cdr (assoc 8 sellist))
  )
  (mklayght nom " G")
)

(defun c:ch (/ sellist)
  (setq	sellist	(entget (car (entsel)))
	nom	(cdr (assoc 8 sellist))
  )
  (mklayght nom " H")
)

(defun c:ct (/ sellist)
  (setq	sellist	(entget (car (entsel)))
	nom	(cdr (assoc 8 sellist))
  )
  (mklayght nom " T")
)


;;;				C:VC - C:VP						
;;;											
;;;		Déplace les entités dans les calque plan ou Coupe ou metré		
;;;											

(defun bcl (selset PC CP / n elist ename nom newname fin laylist)
  (repeat (setq n (sslength selset))
    (setq elist	(entget (setq ename (ssname selset (setq n (1- n)))))
	  nom	(cdr (assoc 8 elist))
    )
    (setq newname (strcat (substr nom 1 (vl-string-search CP nom))
			  PC
			  (substr nom (+ 4 (vl-string-search CP nom)))
		  )
    )

    (layexist)				;Fonction vérification existance du calque

    (entmod (subst (cons 8 newname) (assoc 8 elist) elist))
					;déplace l'entité sur le bon calque
    (ssdel ename ss)			;soustrait l'entité au jeu de sélection global
  ) ;_repeat
)


(defun c:vc (/)
  (setq ss (ssget))
  (bcl ss " C " " P ")
)

(defun c:vp (/)
  (setq ss (ssget))
  (bcl ss " P " " C ")
)


;::					C:TGE					
;;;										
;;;		Déplace les entités dans Terminaux Gaine ou Equipement		
;;;										

;;; FONCTION ;;;

(defun c:tge (/ chx)
  ;; Vers calque Textes
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (Initget 1 "Terminaux Gaine Equipement Distribution Autre")
  (setq	chx (getkword
	      "Déplacer vers un calque [Terminaux/Gaine/Equipement/Distribution/Autre] :"
	    )
  )

  (setq ss (ssget))
  (cond	((or (= chx "Terminaux")
	     (= chx "Gaine")
	     (= chx "Equipement")
	     (= chx "Distribution")
	 )
	 (bclcvc ss (strcat chx))
	)
	((= chx "Autre")
	 (progn	(setq chx (getstring T "Choix :"))
		(bclcvc ss (strcat chx))
	 )
	)
  )
  (vla-EndUndoMark doc)
)

;;; Boucle ;;;

(defun bclcvc (selset end / n elist ename nom newname fin laylist nbc)
  (repeat (setq n (sslength selset))
    (setq elist	(entget (setq ename (ssname selset (setq n (1- n)))))
	  nom	(cdr (assoc 8 elist))
    )
    (if	(> 3 (strlen nom))		;définit le nom du calque où l'entité va être déplacée
      (setq newname (strcat nom " " end))
      (progn

	(setq nbc (cond	((vl-string-search "Terminaux" nom)
			 (setq nbc (vl-string-search "Terminaux" nom))
			)
			((vl-string-search "Gaine" nom)
			 (setq nbc (vl-string-search "Gaine" nom))
			)
			((vl-string-search "Equipement" nom)
			 (setq nbc (vl-string-search "Equipement" nom))
			)
			((vl-string-search "Distribution" nom)
			 (setq nbc (vl-string-search "Distribution" nom))
			)
		  )
	      fin (substr nom (1- (strlen nom)))
	      fin (if (or (= fin " G") (= fin " H") (= fin " T"))
		    (strcat fin)
		    ""
		  )
	)
	(setq
	  newname (if nbc
		    (strcat (substr nom 1 nbc) end fin)
		    (strcat (if	fin
			      (substr nom 1 (- (strlen nom) 1))
			      nom
			    )
			    end
			    fin
		    )
		  )
	)
      )
    ) ;_if - définit le nom du calque où l'entité va être déplacée

    (layexist)				;Fonction vérification existance du calque

    (entmod (subst (cons 8 newname) (assoc 8 elist) elist))
					;déplace l'entité sur le bon calque
    (ssdel ename ss)			;soustrait l'entité au jeu de sélection global
  ) ;_repeat
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Fonction vérification si le calque existe   ;;;
;;;           Sinon Création du calque            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun layexist	(/)
  (if (not (tblsearch "layer" newname)) ;_if - crée le calque s'il n'existe pas
    (if	(not (tblsearch	"layer"
			(substr newname 1 (- (strlen newname) 2))
	     )
	)
      (progn
	(setq laylist
	       (entget (tblobjname
			 "layer"
			 nom
		       )
	       )
	)
	(entmake (subst (cons 2 newname) (assoc 2 laylist) laylist))
      )
      (progn
	(setq laylist
	       (entget (tblobjname
			 "layer"
			 (substr newname 1 (- (strlen newname) 2))
		       )
	       )
	)
	(entmake (subst (cons 2 newname) (assoc 2 laylist) laylist))
      ) ;_progn
    )					;if
  )
)					;defun



;;;			C:VEDL - C:VPRO - C:VDEMOL				
;;;										
;;;		Déplace les entités dans les calque Edl, PRO ou Démol		
;;;										

(defun bcl2 (selset etat / n elist ename nom newname pos fin laylist lg)
  (setq nbcarac (+ 4 (strlen entreprise)))
  (repeat (setq n (sslength selset))
    (setq elist	(entget (setq ename (ssname selset (setq n (1- n)))))
	  nom	(cdr (assoc 8 elist))
	  pos (+ nbcarac (vl-string-search " " (substr nom nbcarac)))
    )
    (cond ((equal (substr nom (+ 1 pos) 1) "E") (setq lg 4))
	  ((equal (substr nom (+ 1 pos) 1) "P") (setq lg 4))
	  ((equal (substr nom (+ 1 pos) 1) "D") (setq lg 6)))
    (setq newname (strcat (substr nom 1 pos)
			  etat
			  (substr nom (+ lg pos))
		  )
    )

    (layexist)				;Fonction vérification existance du calque

    (entmod (subst (cons 8 newname) (assoc 8 elist) elist))
					;déplace l'entité sur le bon calque
    (ssdel ename ss)			;soustrait l'entité au jeu de sélection global
  ) ;_repeat
)


(defun c:vedl (/)
  (setq ss (ssget))
  (bcl2 ss "EDL")
)

(defun c:vpro (/)
  (setq ss (ssget))
  (bcl2 ss "PRO")
)

(defun c:vdemol (/)
  (setq ss (ssget))
  (bcl2 ss "Demol")
)


;;;				C:GIVEP						
;;;										
;;;	DONNE DES EPAISSEURS AUX CALQUES EN FONCTION DE LEUR SUFFIXE 		
;;;										

  (defun givep (HT ep / a nom b lgnom)
    (setq a (tblnext "layer" T))
    (while (setq a (tblnext "layer"))
      (if (> (setq lgnom (strlen (setq nom (cdr (assoc 2 a))))) 3)
	(if (= (substr nom (1- lgnom)) HT)
	  (entmod
	    (subst (cons 370 ep)
		   (assoc 370 (setq b (entget (tblobjname "layer" nom))))
		   b
	    )
	  )
	)
      )
    )
  )


(defun givecol (mot col / nom b)
  (vlax-for n (vla-get-layers *acdoc*)
    (if	(vl-string-search mot (setq nom (vla-get-name n)))
      (if (assoc 420 (setq b (entget (tblobjname "layer" nom))))
	(entmod (subst (cons 420 col) (assoc 420 b) b))
	(entmod (append b (list (cons 420 col))))
      )
    )
  )
)


  (defun c:givep (/)
    (givep " H" 0)
    (givep " T" 30)
    (givecol "EDL" 11053224)
  )


;;;				C:XFRZ						
;;;										
;;;				GELXREF						
;;;										

(defun c:xfrz (/ elist lay)
  (while (/= (cdr (assoc 0 elist)) "INSERT")
    (setq elist (entget (car (entsel "\nPointez l'Xref à geler :"))))
  )
  (entmod
    (subst
      (cons 70 5)
      (assoc 70
	     (setq lay (entget (tblobjname "layer" (cdr (assoc 8 elist)))))
      )
      lay
    )
  )
  (princ
    (strcat "\nLe calque " (cdr (assoc 8 elist)) " a été gelé")
  )
  (princ)
)

;;;				   C:XU						
;;;										
;;;				DEGEL XREF 					
;;;										


(defun c:xu (/ lay)
  (setq lay (tblnext "layer" T))
  (while lay
    (if	(= (substr (cdr (assoc 2 lay)) 1 6) "0 Xref")
      (entmod
	(subst
	  (cons 70 4)
	  (assoc
	    70
	    (setq lay (entget (tblobjname "layer" (cdr (assoc 2 lay)))))
	  )
	  lay
	)
      )
    )
    (setq lay (tblnext "layer"))
  )
  (vlax-for n (vla-get-blocks *acdoc*)
  (if (equal (vla-get-isxref n) :vlax-true) 
    (vla-reload n)))
  (princ
    "tous les calques qui commence par 0 Xref sont affichés et vérouillés"
  )  
  (princ)
)

;;;				C:PI						
;;;										
;;;			DONNE LE CALQUE D'UNE ENTITE				
;;;										

(defun c:pi (/ e tmp file dcl_id nomcalque colcalque)
  (setq	e	  (entget (car (nentsel)))
	nomcalque (cdr (assoc 8 e))
  )

  ;;création de la boîte de dialogue

  (setq	tmp  (vl-filename-mktemp "tmp.dcl")
	file (open tmp "w")
  )
  (write-line
    (strcat "pi:dialog{label="
	    (vl-prin1-to-string "GESTION DU CALQUE DE L'ENTITE")
	    ";
    :row{
    :column{
    :text{label ="
	    (vl-prin1-to-string "Nom du calque : ")
	    ";}    
    :text{label ="
	    (vl-prin1-to-string "Couleur du calque : ")
	    ";}
    :button{label ="
	    (vl-prin1-to-string "Gel")
	    "; key ="
	    (vl-prin1-to-string "gel")
	    ";}
    }
    :column{width =50;
    :text{key ="
	    (vl-prin1-to-string "nomducalque")
	    ";}
    :button{key ="
	    (vl-prin1-to-string "couleurducalque")
	    "; label ="
	    (vl-prin1-to-string "quesac")
	    "; }
    ok_only;
    }
    }
    }
    "
    )
    file
  )
  (close file)

  ;;fin de création de boîte


  (setq dcl_id (load_dialog tmp))
  (if (not (new_dialog "pi" dcl_id))
    (exit)
  )



(setq l nil
      layname nil)

  (vlax-for i (vla-get-filedependencies (vla-get-activedocument (vlax-get-acad-object)))
    (or
      (not (equal (vla-get-feature i) "Acad:XRef"))
      (setq l (cons (vla-get-filename i) l))))

  (foreach n l
    (setq lgstr (strlen n))
    (if (equal (substr nomcalque 1 (- lgstr 4)) (substr n 1 (- (strlen n) 4))) (setq layname (substr nomcalque (- lgstr 2)))))


  (if layname (set_tile "nomducalque" layname) (set_tile "nomducalque" nomcalque))
  (action_tile "couleurducalque" "(col)")
  (action_tile "gel" "(a)")


  (start_dialog)
  (unload_dialog dcl_id)
  (vl-file-delete tmp)
  (princ)
)


(defun a ()
  (vla-put-freeze
    (vla-item (vla-get-layers *acdoc*) nomcalque)
    1
  )
  (done_dialog)
)

(defun col (/ tc)
  (setq	laycolor (acad_truecolordlg
		   (assoc 62 (tblsearch "LAYER" nomcalque))
		 )
  )

  (if (setq tc (assoc 420 laycolor))
    (progn

      (setq accmcolor
	     (vla-getinterfaceobject
	       (vlax-get-acad-object)
	       (strcat "AutoCAD.AcCmColor."
		       (itoa (fix (atof (getvar "acadver"))))
	       )
	     )
      )


      (vla-setrgb
	accmcolor
	(lsh (cdr tc) -16)
	(lsh (lsh (cdr tc) 16) -24)
	(lsh (lsh (cdr tc) 24) -24)
      )
      (vla-put-truecolor
	(vla-item (vla-get-layers *acdoc*) nomcalque)
	accmcolor
      )

      (vla-put-colormethod accmcolor acColorMethodByACI)
      (vla-put-colorindex accmcolor acByLayer)
      (vl-catch-all-apply
	(function (lambda ()
		    (vlax-release-object accmcolor)
		  )
	)
      )
    )


    (vla-put-color
      (vla-item (vla-get-layers *acdoc*) nomcalque)
      (cdr (assoc 62 laycolor))
    )
  )
  (command "regen")
)

;;;										
;;;			PASSE EN CALQUE 0					
;;;										

(defun c:0 (/ ss n elist)
  (if (setq ss (ssget)
      )
    (repeat (setq n (sslength ss))
      (setq elist (entget (ssname ss (setq n (1- n)))))
      (entmod (subst (cons 8 "0") (assoc 8 elist) elist))
    )
    (setvar "clayer" "0")
  )
  (princ)
)

;;;										
;;;			Créer un calque Aide au dessin				
;;;				C:ad C:da C:ead					
;;;										

(princ "Amélioration à prévoir c:AD vérouiller les autres calques")

(defun c:ad (/ layname laycolor)
  (or (= (vla-get-name (vla-get-ActiveLayer *acdoc*)) "00 aide")
      (progn
	(setq
	  getGraphicsWinModelBackgrndColor
	   (vlax-variant-value
	     (vlax-variant-change-type
	       (vla-get-GraphicsWinModelBackgrndColor
		 *disp*
	       )
	       vlax-vbLong
	     )
	   )
	  BackgrndColorcour
	   (if (not (equal getGraphicsWinModelBackgrndColor
			   10395294
		    )
	       )
	     getGraphicsWinModelBackgrndColor
	     BackgrndColorcour
	   )
	  typepoint (getvar "pdmode")
	  layname "00 aide"
	  laycolor '((62 . 173) (420 . 7107012))
	  laycour
	   (if
	     (not
	       (equal (vla-get-name (vla-get-ActiveLayer *acdoc*))
		      layname
	       )
	     )
	      (vla-get-name (vla-get-ActiveLayer *acdoc*))
	      laycour
	   )
	)
	(setvar "pdmode" 3)
	(if (not (tblsearch "LAYER" layname))
	  (progn
	    (vla-add *layers* layname)
	    (rt_addlaycol layname laycolor)
	    (vla-put-Plottable (vla-item *layers* layname) :vlax-false)
	    (setvar "clayer" layname)
	  )				;progn vrai
	  (progn (vla-put-LayerOn (vla-item *layers* layname) :vlax-True)
		 (setvar "clayer" layname)
	  )				;progn faux
	)				;if
	(BackgrndColorclair)
      )
  )
)					;defun

(defun c:da ()
  (or typepoint (setq typepoint 1))
  (or laycour (setq typepoint "0"))
  (setvar "clayer" laycour)
  (vla-put-LayerOn (vla-item *layers* "00 aide") :vlax-false)
  (setvar "pdmode" typepoint)
  (BackgrndColordefaut)
)					;defun

(defun c:ead ()
  (vlax-for n (vla-get-modelspace *acdoc*)
    (and (= (vla-get-layer n) "00 aide") (vla-delete n))
  )
)					;defun


(defun BackgrndColorclair ()
  ;; RGB 253 246 227  
  (vla-put-GraphicsWinModelBackgrndColor *disp* 14939901)
  (vla-put-ModelCrosshairColor *disp* 0)
  (princ)
)

(defun BackgrndColorgris ()
  (vla-put-GraphicsWinModelBackgrndColor *disp* 10395294)
  (vla-put-ModelCrosshairColor *disp* 0)
  (princ)
)

(defun BackgrndColorgrisFonce ()
  (vla-put-GraphicsWinModelBackgrndColor *disp* 3156001)
  (vla-put-ModelCrosshairColor *disp* 16777215)
  (princ)
)

(defun BackgrndColornoir ()
  (vla-put-GraphicsWinModelBackgrndColor *disp* 0)
  (vla-put-ModelCrosshairColor *disp* 16777215)
  (princ)
)

(defun BackgrndColorblanc ()
  (vla-put-GraphicsWinModelBackgrndColor *disp* 16777215)
  (vla-put-ModelCrosshairColor *disp* 0)
  (princ)
)

(defun BackgrndColordefaut ()
  (vla-put-GraphicsWinModelBackgrndColor *disp* BackgrndColorcour)
  (vla-put-ModelCrosshairColor *disp* 16777215)
  (princ)
)

(defun c:bgd ()
       (initget 1 "N B G")
       (setq BgndColor
	      (getkword "\nChoix [Noir/Blanc/Gris] : <G> ")
       )
       (if (= BgndColor "")
	 (setq BgndColor "G")
       )
       (cond ((equal BgndColor "N")
	      (BackgrndColornoir)
	     )
	     ((equal BgndColor "B")
	      (BackgrndColorblanc)	      
	     )
	     ((equal BgndColor "G")
	      (BackgrndColorgrisFonce)	      
	     )
       )       
)

































































































































































