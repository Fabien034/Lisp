(vl-load-com)
(setq *AcDoc* (vla-get-activeDocument (vlax-get-acad-object)))  

;;;;					C:BU						
;;;;											
;;;;			** RENDRE UN BLOC UNIQUE **					
;;;;		*** créé le 06/03/08 par Vincent Dufournaud ***				
;;;;											
;;;;		permet la sélection de plusieurs blocs d'un même nom			


(defun c:bu (/ ss1 nom ename new)
  (vla-StartUndoMark *AcDoc*)
  (if (setq ss1 (ssget '((0 . "INSERT"))))
    (progn
    (setq nom (cdr (assoc 2 (setq elist (entget (setq ename (ssname ss1 0)))))))
    (repeat (setq n (sslength ss1))
      (if (/= nom (cdr (assoc 2 (entget (ssname ss1 (setq n (1- n)))))))
	(setq nom nil)))))

  (if (not nom)
    (alert "Sélection d'un ou plusieurs blocs identiques")
    (progn
      
  (while (not new)
  (setq new (getstring "\nNouveau nom du bloc : "))
    (if (tblsearch "block" new)
      (progn
	(setq new nil)	
	)
      )
    )

  (entdel ename)

  (setq	old (cdr (assoc 2 elist))
	n   (cdr (assoc -2 (tblsearch "block" old)))
	ss  (ssadd)
  )
  (while n
    (entmake (setq n (entget n)))
    (ssadd (entlast) ss)
    (setq n (entnext (cdr (assoc -1 n))))
  )

  (setvar "cmdecho" 0)
  (command "scu" "g")
  (command "bloc" new "0,0,0" ss "")
  (setq aunits (getvar "aunits"))
  (setvar "aunits" 3)

  (defun ib ()
  (command "_insert"
	   new
	   (cdr (assoc 10 elist))
	   (cdr (assoc 41 elist))
	   (cdr (assoc 42 elist))
	   (cdr (assoc 50 elist))
  )  

  (entmod (subst (assoc 8 elist) (assoc 8 (entget (entlast))) (entget (entlast))))
    )
  (ib)
  (repeat (1- (setq n (sslength ss1)))
    (setq elist (entget (setq ename (ssname ss1 (setq n (1- n))))))
    (entdel ename)
    (ib))
  (command "scu" "p")
  (setvar "cmdecho" 1)
  (setvar "aunits" aunits)
))
  (vla-EndUndoMark *AcDoc*)
  );defun



;;;				C:BR						
;;;                                      					
;;;     			Renomme un bloc         			
;;;                                      					
;;;				par Gile					

(defun c:br (/ b l n)
(vl-load-com)
(while (not (and
(setq b (car (entsel)))
(= (cdr (assoc 0 (entget b))) "INSERT")
)
)
)
(setq o (cdr (assoc 2 (entget b)))
n ""
)
(while (or (= n "")
(tblsearch "BLOCK" n)
)
(setq n (getstring (strcat "\nAncien nom : \""
o
"\"\tNouveau nom: "
)
)
)
)
(vla-put-Name
(vla-item
(vla-get-Blocks
(vla-get-ActiveDocument
(vlax-get-acad-object
)
)
)
o
)
n
)
(princ)
)


