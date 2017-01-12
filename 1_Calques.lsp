
;;; normalisation des calques sous autocad					
;;; Version 2.0	- Décembre 2016							
;;; Dévelopeur Vincent Dufournaud - Fabien Rosso				
;;; Routine gc:... Giles Chanteau http://gilecad.azurewebsites.net/		

(vl-load-com)
(setq *AcDoc* (vla-get-activeDocument (vlax-get-acad-object)))


;;;										
;;;				CREATION D'ALIAS				
;;;										

(defun c:vt () (fr:rt_replaceCalque "T" "FIN")) ; Vers Texte
(defun c:vh () (fr:rt_replaceCalque "H" "FIN")) ; vers Hachure
(defun c:vg () (fr:rt_replaceCalque "G" "FIN")) ; Vers Graphique

;;;				C:ADDLAY					
;;;     	Création d'un calque Normalisé					
;;; 		Version   : 2.0 - 19.12.2016					
;;; 		Dévelopeur: Vincent Dufournaud - Fabien Rosso			
;;;										
;;; utilise les routines							
;;; gc:lst2str (routine list.LSP) Giles Chanteau				
;;; gc:take (routine list.LSP) Giles Chanteau					
;;; fr:rt_addlaycol (1_Calques.LSP)						


(defun c:addlay	()

  (setq nb (getstring "\nNumero : <0> "))
  (if (= nb "")
    (setq nb "0")
  )
  (setq societe (getstring (strcat "\nSociété : <" entreprise "> ")))
  (if (= societe "")
    (setq societe entreprise)
  )
  (initget 1 (gc:lst2str calque-lot " "))
  (setq	lot (getkword (strcat "\nLot ["
			      (gc:lst2str calque-lot "/")
			      "] : <"
			      (nth 0 calque-lot)
			      "> "
		      )
	    )
  )
  (initget 1 (gc:lst2str calque-statut " "))
  (setq
    statut (strcase (getkword (strcat "\nStatut ["
				      (gc:lst2str calque-statut "/")
				      "] : <"
				      (nth 0 calque-statut)
				      "> "
			      )
		    )
	   )
  )
  (initget 1
	   (gc:lst2str
	     (mapcar '(lambda (a) (vl-string-right-trim (substr a 2) a))
		     calque-vue
	     )
	     " "
	   )
  )
  (setq	vue (getkword (strcat "\nChoix ["
			       (gc:lst2str calque-vue "/")
			       "] : <"
			       (nth 0 calque-vue)
			       "> "
		       )
	     )
  )
  (setq	desip (getstring "\nDésignation Principale : ")
	desis (getstring T "\nDésignation Secondaire : ")
  )
  (if (/= desis "")
    (setq desis (strcat desis " "))
  )
  (initget 1
	   (gc:lst2str
	     (gc:take
	       nb-take-type-entite
	       (mapcar
		 '(lambda (a) (vl-string-right-trim (substr a 2) a))
		 calque-type-entite
	       )
	     )
	     " "
	   )
  )
  (setq	ght
	 (getkword
	   (strcat
	     "\nChoix ["
	     (gc:lst2str
	       (gc:take nb-take-type-entite		 
		 (mapcar '(lambda (a) (vl-string-right-trim (substr a 2) a))
			 calque-type-entite
		 )
	       )
	       "/"
	     )
	     "] : <"
	     (nth 0 calque-type-entite)
	     "> "
	   )
	 )
  )
  (setq	layname	(strcat	nb     " "    societe	    " "	   lot
			" "    statut " "    vue   " - "  desip
			" "    desis  ght
		       )
  )
  (setq laycolor (acad_truecolordlg 7))

  (if (not (tblsearch "LAYER" layname))
    (progn
      (setq nw-layer (vla-add *layers* layname))
      (fr:rt_addlaycol layname laycolor)
      (setvar "clayer" layname)
    )
    (setvar "clayer" layname)
  )
  (princ)
) ;_defun c:addlay

;;;				C:ADDLAYP					
;;;     	Création des calques de présentation				
;;; 		Version   : 2.0 - 19.12.2016					
;;; 		Dévelopeur: Vincent Dufournaud - Fabien Rosso			
;;;										
;;; utilise les routines							
;;; fr:rt_addlaycol (1_Calques.LSP)						

(defun c:addlayp (/ lstlay)
  (foreach l calque-presentation
    (setq layname  (nth 0 l)
	  layColor (nth 1 l)
    )
    (if	(not (tblsearch "LAYER" layname))
      (progn
	(setq nw-layer (vla-add *layers* layname))
	(fr:rt_addlaycol layname layColor)
	(setvar "clayer" layname)
      )
      (setvar "clayer" layname)
    )
  )
  (princ)
) ;_defun c:addlayp

;;;				C:ADDLAYPX					
;;		Création des calques Xref					
;;; 		Version   : 2.0 - 19.12.2016					
;;; 		Dévelopeur: Vincent Dufournaud - Fabien Rosso			
;;;										
;;; utilise les routines							
;;; fr:rt_addlaycol (1_Calques.LSP)						

(defun c:addlayx (/ lstlay)

  (setq layname (strcat "0 Xref " (getstring "\nNom :")))
  (if (not (tblsearch "LAYER" (nth 0 l)))
    (progn
      (setq nw-layer (vla-add *layers* layname))
      (setvar "clayer" layname)
    )
    (setvar "clayer" layname)
  )
  (princ)
) ;_defun c:addlayx

;;;		            C:VERSLAY						
;;;		Déplacement Manuel une enbtité dans calque Normalisé 		
;;; 		Version   : 2.0 - 19.12.2016					
;;; 		Dévelopeur: Vincent Dufournaud - Fabien Rosso			
;;;										
;;; utilise les routines							
;;; fr:rt_addlaycol (1_Calques.LSP)						
;;; fr:rt_replaceCalqueStatut							
;;; fr:rt_replaceCalqueVue							
;;; fr:rt_replaceCalqueTypeEntite						
;;; gc:take (routine list.LSP) Giles Chanteau					

(defun c:verslay (/)
  (initget 1
	   (strcat
	     (gc:lst2str calque-statut " ")
	     " "
	     (gc:lst2str
	       (mapcar
		 '(lambda (a) (vl-string-right-trim (substr a 2) a))
		 calque-vue
	       )
	       " "
	     )
	     " "
	     (gc:lst2str
	       (gc:take
		 nb-take-type-entite
		 (mapcar
		   '(lambda (a) (vl-string-right-trim (substr a 2) a))
		   calque-type-entite
		 )
	       )
	       " "
	     )
	   )
  )
  (setq	chx
	 (getkword
	   (strcat
	     "Déplacer dans un calque vers ["
	     (gc:lst2str calque-statut "/")
	     "/---/"
	     (gc:lst2str calque-vue "/")
	     "/---/"
	     (gc:lst2str
	       (gc:take nb-take-type-entite calque-type-entite)
	       "/"
	     )
	     "]:<"
	     (nth 0 calque-type-entite)
	     ">"
	   )
	 )
  )
  (if (= chx nil)
    (setq chx "G")
  )
  (cond	((vl-position chx calque-statut)
	 (fr:rt_replaceCalque (strcase chx) 3)
	)
	((vl-position chx calque-vue)
	 (fr:rt_replaceCalque chx 4)
	)
	((vl-position chx calque-type-entite)
	 (fr:rt_replaceCalque chx "FIN")
	)
  )
) ;_defun c:verslay


;;;									
;;;				ROUTINE					
;;;									

;;;	routine fr:rt-veriflay						
;;; 	Vérifier si un calque est normalisé				
;;;	Suivant le nom de l'entreprise et "-" en 5eme position		
;;;	Version 2.0 : 19.12.2016					
;;; 	Retourne T ou nil						
;;; 	argument: nom du calque sous forme str				


(defun fr:rt-veriflay (layname /)
  (and (equal (nth 1 (gc:str2lst layname " ")) entreprise)
       (equal (nth 5 (gc:str2lst layname " ")) "-")
  )
) ;_defun fr:rt-veriflay

;;;	routine fr:rt_addlaycol						
;;;	Attribue une couleur à un calque   			 	
;;;	Suivant couleur index ou couleur vrai   			
;;;	Version 2.0 : 19.12.2016	   			 	
;;;	Argument: Layname = Nom du calque (str)				
;;;		  Laycolor= Couleur du calque (acad_truecolordlg 7)	


(defun fr:rt_addlaycol (layname laycolor / tc)
  (if (setq tc (assoc 420 laycolor))
    (progn
      (setq accmcolor
	     (vlax-create-object
	       (strcat "AutoCAD.AcCmColor."
		       (substr (vlax-product-key) 28 2)
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
) ;_defun fr:rt_addlaycol

;;;	routine fr:rt_replaceCalque					
;;;	Remplace du calque	   				 	
;;;									
;;;	Version 2.0 : 19.12.2016	   			 	
;;;	Argument: chx = Remplacé par chx (str)				
;;;		  pos = Position normalisé à remplacer			
;;;									
;;; utilise les routines						
;;; fr:rt-veriflay (1_Calques.LSP)					
;;; gc:str2lst (routine list.LSP) Giles Chanteau			
;;; gc:lst2str (routine list.LSP) Giles Chanteau			
;;; gc:substAt (routine list.LSP) Giles Chanteau			

(defun fr:rt_replaceCalque (chx pos /)
  (vla-StartUndoMark *AcDoc*)
  (setq ss (ssget))
  ;; vérifie si les claques des object selectionés sont normalisés	
  ;; sinon les retire du jeu de selection				
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
    )
    ;; création du nom du calque de destination
    (if	(= pos "FIN")
      (setq nw-layname
	     (gc:lst2str
	       (gc:substAt
		 chx
		 (1- (length lst-layname))
		 lst-layname
	       )
	       " "
	     )
      )
      (setq nw-layname
	     (gc:lst2str
	       (gc:substAt
		 chx
		 pos
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
  )
  (vla-EndUndoMark *AcDoc*)
) ;_defun fr:rt_replaceCalque
