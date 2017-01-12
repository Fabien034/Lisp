
(vl-load-com)

(or *acad* (setq *acad* (vlax-get-acad-object)))
(or *acdoc* (setq *acdoc* (vla-get-ActiveDocument *acad*)))
(or *blocks* (setq *blocks* (vla-get-Blocks *acdoc*)))
(or *layers* (setq *layers* (vla-get-Layers *acdoc*)))
(or *util* (setq *util* (vla-get-Utility *acdoc*)))
(or *mspace* (setq *mspace* (vla-get-modelspace *acdoc*)))
(or *disp* (setq *disp* (vla-get-display (vla-get-preferences *acad*))))

;;;Chargement de lisp

(load "0_Config")
(load "1_Blocs")
(load "1_Calques")
(load "1_Cotation")
(load "1_nettoyeur.lsp")
(load "1_Présentation")
(load "1_Tableau")
(load "1_compar_dwg_v0.4.lsp")
(load "métrés")
(load "Pentes")
(load "poly_....lsp")
(load "Rotation alignée")
(load "save_en_2000")
(load "scriptc550a3")
(load "Talus")
(load "SetPathTypeXref")


;;; routine à charger

(or rt-list (setq rt-list (load "routine list")))
(or rt-text (setq rt-text (load "1_Textes")));nécessaire à la création des styles de textes)


;;; chemin xref relatif

(SetPathType 0);voir SetPathTypeXref.lsp pour plus d'info


;;;				C:NEW						
;;;										
;;;			CONFIG NOUVEAU PLAN					
;;;										

(defun c:new (/ unite descrunite)
  (progn
  (initget 2 "me cm mm")
  (setq unite (getkword "\nUnité du dessin : [ME/CM/MM]"))
  (cond	((= unite "me") (setq descrunite '(1000 6 100 0.01)))
	((= unite "cm") (setq descrunite '(10 5 1 1)))
	((= unite "mm") (setq descrunite '(1 4 0.1 10)))
  );Définit les valeurs pour les variables dépendantes de l'unité de dessin
  (setvar "CELTSCALE"
	  (* (/ 1 (getvar "ltscale")) (last descrunite))
  );Définit le facteur d'échelle du type de ligne de l'objet courant
  (setvar "INSUNITS" (cadr descrunite))
  ;;Spécifie une valeur d'unités de dessin pour la mise à l'échelle automatique des blocs  
  (mkstyle (list ".Iso 2" 2 "isocp.shx"))
  (mkstyle (list ".Iso 1.5" 1.5 "isocp.shx"))
  (mkstyle (list ".Verdana 1.5" 1.5 "verdana.TTF"))
  (mkstyle (list ".Verdana 2" 2 "verdana.TTF"))
  (mkcotstyle)
  
  (vla-put-activetextstyle *acdoc* (vla-item (vla-get-textstyles *acdoc*)".Iso 2"))
  ;;création des styles de texte  
  (vla-put-activedimstyle *acdoc* (vla-item (vla-get-dimstyles *acdoc*) ".Plan"))
  ;;Définit le style de côte courant  
  (setvar "DIMLFAC" (caddr descrunite))
  ;;Définit un facteur d'échelle pour les cotes linéaires

  ;;A rajouter Créer les échelles 1:100 et 1:50
  (print "Modification du lisp c:new - Créer les échelles 1:100 et 1:50")


    (princ)
  );progn
);_defun new


;;;			C:ST - C:Pb - C:EL - C:CVC				
;;;										
;;;			GABARIT PAR LOT			 			
;;;										

(defun gabarit (nombloc / facteur)
  (setq facteur (* 10 (getvar "dimlfac")))
  (setvar "cmdecho" 0)
  (command "inserer" nombloc "0,0" (/ 1.0 facteur) (/ 1.0 facteur) 0)
  (command "_explode" (entlast))
  (command "zoom" "et")
  (command "regntout")
  (setvar "cmdecho" 1))
  ;;Insertion de la légende  


;;;				GABARIT ST					

(defun c:gabst (/ ss n ename coef elist)
  (progn
    (vla-StartUndoMark *AcDoc*)
    
    (gabarit "legende_ST")
    
    (setq ss (ssget "_X" '((0 . "LWPOLYLINE"))))
    (repeat (setq n (sslength ss))
      (setq ename (ssname ss (setq n (1- n)))
	    coef  nil
      );setq
      (cond
	((= (cdr (assoc 8 (setq elist (entget ename))))
	    (strcat "0 " entreprise " ST PRO P - Traces G")
	 )
	 (setq coef 1)
	)
	((= (cdr (assoc 8 elist))
	    (strcat "0 " entreprise " ST PRO P - Repère coupe G")
	 )
	 (setq coef 4)
	)
      )					;cond
      (if coef
	(entmod	(subst (cons 48 (* coef (getvar "celtscale")))
		       (assoc 48 elist)
		       elist
		)
	)
      );if
    );repeat
    
    (load "2_ST")
    
    (vla-EndUndoMark *AcDoc*)
    (print "Document configuré pour un plan ST")
  );progn
);defun c:st


;;;				GABARIT Pb					

(defun c:gabpb (/)
(progn
    (vla-StartUndoMark *AcDoc*)
  (gabarit "Legende_Pb")

  (vla-EndUndoMark *AcDoc*)
    (print "Document configuré pour un plan Pb")
  );progn
  );defun


;;;				GABARIT EL					

(defun c:gabel (/)
  (progn
    (vla-StartUndoMark *AcDoc*)
  (gabarit "Legende_EL")

    (vla-EndUndoMark *AcDoc*)
    (print "Document configuré pour un plan EL")
  )
  );defun

;;;				GABARIT CVC					

(defun c:gabcvc (/)
  (progn
    (vla-StartUndoMark *AcDoc*)    
    (gabarit "Legende_CVC")
    (print "Document configuré pour un plan CVC")
    (load "2_CVC")
    (load "2_DES")
    (vla-EndUndoMark *AcDoc*)    
  )
)					;defun

(defun c:gabsch (/)
  (progn
    (vla-StartUndoMark *AcDoc*)
    (setvar "INSUNITS" 5)
    (gabarit "legende_sch_CVC_PB")
    (print "Document configuré pour un plan CVC")
    (load "2_CVC")
    (load "2_DES")
    (vla-EndUndoMark *AcDoc*)    
  )
)					;defun


;;;		C:zh - C:zb - C:cph - C:cpb - C:dph - C:dpb					
;;;			Routine: testdist 							
;;;												
;;;Modif pour autocad 2016 : remplacement de (command "scu" "g") par (command "scu" "ge")	


(defun c:zh ()
  (setq ss (cadr (ssgetfirst)))
  (testdist)
  (progn (command "scu" "ge")
	 (command "-pan" '(0 0) (list 0 (* den -1)))
	 (command "scu" "p")
  )
  (sssetfirst nil ss)
)

(defun c:zb ()
  (setq ss (cadr (ssgetfirst)))
  (testdist)
  (progn (command "scu" "ge")
	 (command "-pan" '(0 0) (list 0 den))
	 (command "scu" "p")
  )
  (sssetfirst nil ss)
)

(defun c:cpb ()
  (testdist)
  (progn (command "scu" "ge")
	 (command "copier" "p" "" '(0 0) (list 0 (* den -1)) "q")
	 (command "scu" "p")
  )
)

(defun c:cph ()
  (testdist)
  (progn (command "scu" "ge")
	 (command "copier" "P" "" '(0 0) (list 0 den) "q")
	 (command "scu" "p")
  )
)

(defun c:dpb ()
  (testdist)
  (progn (command "scu" "ge")
	 (command "deplacer" "p" "" '(0 0) (list 0 (* den -1)) "q")
	 (command "scu" "p")
  )
)

(defun c:dph ()
  (testdist)
  (progn (command "scu" "ge")
	 (command "deplacer" "P" "" '(0 0) (list 0 den) "q")
	 (command "scu" "p")
  )
)

; Routine Xdata sur Calque 0
; Variable !den = Distance entre niveaux
; 1040 -> Reel
; 1000 -> String
(defun testdist	()
  (if (not
	(assoc -3 (entget (tblobjname "layer" "0") '("DISTANCE")))
      )
    (progn
      (setq den (getreal "\nDistance entre niveaux : "))
      (regapp "DISTANCE")
      (entmod
	(append	(entget (tblobjname "layer" "0"))
		(list (list -3 (list "distance" (cons 1040 den))))
	)
      )
    )
    (setq den
	   (cdr
	     (cadadr
	       (assoc -3
		      (entget (tblobjname "layer" "0") '("DISTANCE"))
	       )
	     )
	   )
    )
  )
)					;defun









  
 
    

