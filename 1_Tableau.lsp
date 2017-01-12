;;; 	    routine pour Récupèrer ou créer un un style de tableau			
;;; 				Argument "nom du style					

(defun GetOrCreateTableStyle (tbl_name	  /	      name
			      namelst	  objtblsty   objtblstydic
			      tablst	  txtsty
			     )
  (setq	objTblStyDic
	 (vla-item (vla-get-dictionaries *acdoc*)
		   "ACAD_TABLESTYLE"
	 )
  )
  (foreach itm (vlax-for itm objTblStyDic
		 (setq tabLst (append tabLst (list itm)))
	       )
    (if	(not
	  (vl-catch-all-error-p
	    (setq name (vl-catch-all-apply 'vla-get-Name (list itm)))
	  )
	)
      (setq nameLst (append nameLst (list name)))
    )
  )
  (if (not (vl-position tbl_name nameLst))
    (vla-addobject objTblStyDic tbl_name "AcDbTableStyle")
  )
  (setq	objTblSty (vla-item objTblStyDic tbl_name)
	TxtSty	  (variant-value (vla-getvariable *acdoc* "TextStyle"))
  )
  (mapcar '(lambda (x) (vla-settextstyle objTblSty x TxtSty))
	  (list acTitleRow acHeaderRow acDataRow)
  )
  (vla-setvariable *acdoc* "CTableStyle" tbl_name)
)


;;; 	    routine pour créer un tableau depuis une list"lst"  				     	
;;;       	  comprennant des sous-list pour chaque rangés  				        
;;; Argument "liste des cellules" "nom du tableau" "hauteur txt" "longueur cellule" "largeur cellule"	

(defun rt_addtab (lst tabname htxt longueurcel hauteurcel / pt)

  (setq	pt nil
    	tabcol 0
	n      0
	nb     (length lst)
  )
  (if (equal (1- nb) n)
    (setq tabcol (length (nth n lst)))
    (repeat nb
      (progn (if (< tabcol (length (nth n lst)))
	       (setq tabcol (length (nth n lst)))
	     )
	     (setq n (1+ n))
      )
    )
  )  

  (setq	myTable
	 (vla-addtable
	   *mspace*
	   (vla-GetPoint *util* nil "\nPoint d'insertion: ")
	   (1+ (length lst))		;nombre de ligne
	   tabcol			; nombre de colone
	   hauteurcel			;hauteur cellule
	   longueurcel			;largeur cellule
	 )
  )
  
  (mapcar '(lambda (x) (vla-setTextHeight myTable x htxt))
	  (list acTitleRow acHeaderRow acDataRow)
  )

  (mapcar '(lambda (x) (vla-setAlignment myTable x 5))
	  (list acTitleRow acHeaderRow acDataRow)
  )

  (vla-settext myTable 0 0 tabname)

  (setq n 0)
  (repeat nb
    (progn
      (setq nn 0)
      (repeat (length (nth n lst))
	(vla-settext myTable (1+ n) nn (nth nn (nth n lst)))
	(setq nn (1+ nn))
      )
      (setq n (1+ n))
    )
  )

  (princ)
)					;defun



