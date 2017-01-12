
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

;;;				C:FM						
;;;										
;;;			FMULT dans le bon calque				
;;;										

(defun c:fm ()
  (vla-StartUndoMark *AcDoc*)
  (setq clayer (getvar "clayer")
	cecolor (getvar "cecolor"))
  (MCUR "00 Window" 51)
  (command "_mview"
	   (while (not (zerop (getvar "cmdactive")))
	     (command pause)
	   )
  )
  (setvar "clayer" clayer)
  (setvar "cecolor" cecolor)
  (vla-EndUndoMark *AcDoc*)
)

  

(vl-load-com)


;;;				C:AEH						
;;;										
;;; Ajoute le bloc d'échelle graphique en fonction de l'echelle de la fenêtre	
;;;										

(defun c:aeh (/ fenetre bloc_echelle calque_courant)
  (if (not (tblsearch "layer" "00 Cadre"))
    (alert
      "Porcasse tu fais tes présentations sans les bons calques !"
    )
    (progn
      (setq fenetre	   (vlax-ename->vla-object (car (entsel "\nChoisis une fenêtre")))
	    echelle_fmult  (vla-get-customscale fenetre)
	    calque_courant (getvar "clayer")
      )
      (setvar "clayer" "00 Cadre")
      (setvar "cmdecho" 0)
      (command "_.insert"
	       "echelle"
	       (getpoint "Donnes le point bas droit")
	       echelle_fmult
	       echelle_fmult
	       0
      )
      (setvar "cmdecho" 1)
      (setq bloc_echelle (vlax-ename->vla-object (entlast)))
      (setvar "clayer" calque_courant)
      
      (vlr-pers (vlr-object-reactor (list fenetre) bloc_echelle '((:vlr-modified . echelle-graphique))))


    )
  )
);defun c:aeh

(defun echelle-graphique (notifier-object reactor-object parameter-list)

  (if
    (and
      (vlax-write-enabled-p (vlr-data reactor-object)) ;; teste si le bloc peut être modifié.
      (vlax-property-available-p notifier-object "CustomScale") ;; teste si l'échelle de la fenêtre peut être lue.
      )
    (progn
      (vla-put-XEffectiveScaleFactor (vlr-data reactor-object) (vla-get-customscale notifier-object))
      (vla-put-YEffectiveScaleFactor (vlr-data reactor-object) (vla-get-customscale notifier-object))
      )
    );if
 
(princ)
 
);defun
 
(princ)



