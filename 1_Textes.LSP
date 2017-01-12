
(vl-load-com)
(setq *AcDoc* (vla-get-activeDocument (vlax-get-acad-object)))


;;;										
;;;				MKSTYLE						
;;;										

  (defun mkstyle (lstxtsty /)
    (if (not (tblsearch "STYLE" (car lstxtsty)))
    (entmake (append
	       '((0 . "STYLE")
		 (100 . "AcDbSymbolTableRecord")
		 (100 . "AcDbTextStyleTableRecord"))
	       (list (cons 2 (car lstxtsty))) 
		 '((70 . 0))
	       (list(cons 40 (cadr lstxtsty)))
	       (list(cons 3 (caddr lstxtsty)))
		 '((-3 ("AcadAnnotative" (1000 . "AnnotativeData")(1002 . "{")(1070 . 1)(1070 . 1)(1002 . "}"))))	       
	     )
    )
  ));defun mkstyle

;;;										
;;;				MAKETXT						
;;;										

(defun maketxt (txt /)
  (vla-addmtext
    (vla-get-ModelSpace
      (vla-get-ActiveDocument
	(vlax-get-acad-object)
      )
    )
    (vlax-3d-point (getpoint))
    2
    txt
  )
)


;;;		C:1 - C:2 - C:3 - C:4 - C:5 - C:6 - C:7 - C:8 - C:9		
;;;										
;;;		   Modificateurs de justifications de textes			
;;;										



(defun c:1 (/ ss)
  (setq ss (ssget) post 12 posmt 7)
  (bougepos)
  )
(defun c:2 (/ ss)
  (setq ss (ssget) post 13 posmt 8)
  (bougepos)
  )
(defun c:3 (/ ss)
  (setq ss (ssget) post 14 posmt 9)
  (bougepos)
  )
(defun c:4 (/ ss)
  (setq ss (ssget) post 9 posmt 4)
  (bougepos)
  )
(defun c:5 (/ ss)
  (setq ss (ssget) post 10 posmt 5)
  (bougepos)
  )
(defun c:6 (/ ss)
  (setq ss (ssget) post 11 posmt 6)
  (bougepos)
  )
(defun c:7 (/ ss)
  (setq ss (ssget) post 6 posmt 1)
  (bougepos)
  )
(defun c:8 (/ ss)
  (setq ss (ssget) post 7 posmt 2)
  (bougepos)
  )
(defun c:9 (/ ss)
  (setq ss (ssget) post 8 posmt 3)
  (bougepos)
  )

(defun bougepos (/)
  (setq *acdoc* (vla-get-activedocument (vlax-get-acad-object)))
  (vlax-for n (vla-get-activeselectionset *acdoc*)
    (cond ((equal (vla-get-objectname n) "AcDbText") (vla-put-alignment n post))
	((equal (vla-get-objectname n) "AcDbMText") (vla-put-AttachmentPoint n posmt))
	)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Recuperation d'un texte depuis une xref  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:rt (/)
  (setq txt1 (cdr (assoc 1 (entget (car (nentsel))))))
  (maketxt
    txt1
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 	Ajoute un texte + lignes             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:at ( / p1 p2 obtx milieu pmin pmax)
  (setq p1 (getpoint "\nPremier point") p2 (getpoint "\nDeuxième point") tx (getstring T "\nTexte : ") obtx (vla-addtext *mspace* tx (vlax-3d-point p1) 20) milieu (mapcar '(lambda (x1 x2) (/ (+ x1 x2) 2)) p1 p2))
  (vla-put-alignment obtx 10)
  (vla-put-TextAlignmentPoint obtx (vlax-3d-point milieu))
  (vla-GetBoundingBox obtx 'pmin 'pmax)
  (vla-put-Lineweight (vla-addline *mspace* (vlax-3d-point p1) (vlax-3d-point (list (- (car (vlax-safearray->list pmin)) 10)(cadr milieu)))) 13)
  (vla-put-Lineweight (vla-addline *mspace* (vlax-3d-point p2) (vlax-3d-point (list (+ (car (vlax-safearray->list pmax)) 10)(cadr milieu)))) 13)
  (princ)
  )