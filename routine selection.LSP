;;; Special_Selections -Gilles Chanteau- (gile)
;;; Quelques routines pour créer des jeux de sélection particuliers.

;;;===============================================================;;;

;; Sélection par calque
(defun c:ssl (/ ss ent)
  (and
    (or
      (and
	(setq ss (cadr (ssgetfirst)))
	(= 1 (sslength ss))
	(setq ent (ssname ss 0))
      )
      (and
	(sssetfirst nil nil)
	(setq ent (car (entsel)))
      )
    )
    (sssetfirst nil (ssget "_X" (list (assoc 8 (entget ent)))))
  )
  (princ)
)

;;;===============================================================;;;

;; Sélection par type d'entité
(defun c:sse (/ ss)
  (and
    (or
      (and
	(setq ss (cadr (ssgetfirst)))
	(= 1 (sslength ss))
	(setq ent (ssname ss 0))
      )
      (and
	(sssetfirst nil nil)
	(setq ent (car (entsel)))
      )
    )
    (sssetfirst nil (ssget "_X" (list (assoc 0 (entget ent)))))
  )
  (princ)
)

;;;===============================================================;;;

;; Sélection par couleur
(defun c:ssc (/ ent elst col)
  (and
    (or
      (and
	(setq ss (cadr (ssgetfirst)))
	(= 1 (sslength ss))
	(setq ent (ssname ss 0))
      )
      (and
	(sssetfirst nil nil)
	(setq ent (car (entsel)))
      )
    )
    (setq elst (entget ent)
	  col  (cond
		 ((assoc 430 elst))
		 ((assoc 420 elst))
		 ((assoc 62 elst))
		 (T (cons 62 256))
	       )
    )
    (sssetfirst nil (ssget "_X" (list col)))
  )
  (princ)
)

;;;===============================================================;;;

;; Sélection par type de ligne
(defun c:sstl (/ ent tl)
  (and
    (or
      (and
	(setq ss (cadr (ssgetfirst)))
	(= 1 (sslength ss))
	(setq ent (ssname ss 0))
      )
      (and
	(sssetfirst nil nil)
	(setq ent (car (entsel)))
      )
    )
    (or	(setq tl (assoc 6 (entget ent)))
	(setq tl (cons 6 "BYLAYER"))
    )
    (sssetfirst nil (ssget "_X" (list tl)))
  )
  (princ)
)

;;;===============================================================;;;

;; Sélection par motif de hachure
(defun c:sshp (/ ss ent elst)
  (and
    (or
      (and
        (setq ss (cadr (ssgetfirst)))
        (= 1 (sslength ss))
        (setq ent (ssname ss 0))
      )
      (and
        (sssetfirst nil nil)
        (setq ent (car (entsel "\nSélectionnez une hachure: ")))
      )
    )
    (= (cdr (assoc 0 (setq elst (entget ent)))) "HATCH")
    (sssetfirst nil (ssget "_X" (list '(0 . "HATCH") (assoc 2 elst))))
  )
  (princ)
)

;;;===============================================================;;;

;; Sélection par valeur d'attribut
(defun c:ssatt (/ doc att elst tag val name ss1 ss2)
  (vl-load-com)
  (and
    (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
    (setq att (car (nentsel "\nSélectionnez l'attribut source: ")))
    (setq att (vlax-ename->vla-object att))
    (= (vla-get-ObjectName att) "AcDbAttribute")
    (setq tag (vla-get-TagString att)
	  val (vla-get-TextString att)
	  blk (vla-ObjectIDToObject doc (vla-get-OwnerId att))
	  name (if (vlax-property-available-p blk 'EffectiveName)
		 (vla-get-EffectiveName blk)
		 (vla-get-Name blk)
	       )
	  ss2 (ssadd)
    )
    (princ "\nSélectionnez les blocs ou <tout>: ")
    (or
      (ssget (list '(0 . "INSERT")
		   '(66 . 1)
		   (cons 2 (strcat name ",`*U*"))
	     )
      )
      (ssget "_X"
	     (list '(0 . "INSERT")
		   '(66 . 1)
		   (cons 2 (strcat name ",`*U*"))
	     )
      )
    )
    (vlax-for blk (setq ss1 (vla-get-ActiveSelectionSet doc))
      (if (= name
	     (if (vlax-property-available-p blk 'EffectiveName)
	       (vla-get-EffectiveName blk)
	       (vla-get-Name blk)
	     )
	  )
	(foreach a (vlax-invoke blk 'GetAttributes)
	  (if (and (= (vla-get-TagString a) tag)
		   (= (vla-get-TextString a) val)
	      )
	    (ssadd (vlax-vla-object->ename blk) ss2)
	    T
	  )
	)
	T
      )
    )
    (not (vla-delete ss1))
    (sssetfirst nil ss2)
  )
  (princ)
)

;;;===============================================================;;;

;; SSCU (gile) 31/03/07
;; Sélection mutiple par cible, fenêtre ou capture
;; Le cadre de la fenêtre est parallèle au plan du SCU courant
;; La sélection est terminée en faisant Entrée, Espace ou clic droit

(defun c:sscu (/ sel sst loop p1 gr p2 p3 p4 po ss n ent)

  (defun ssd_err (msg)
    (if	(= msg "Fonction annulée")
      (princ)
      (princ (strcat "\nErreur: " msg))
    )
    (sssetfirst nil nil)
    (redraw)
    (setq *error* m:err
	  m:err	nil
    )
    (princ)
  )

  ;; Retourne un jeu de sélection, un point ou nil
  (defun sel (/ loop gr pt)
    (setq loop T)
    (while (and (setq gr (grread T 12 2)) (/= (car gr) 3) loop)
      (cond
	((= (car gr) 5)
	 (setq pt (cadr gr))
	)
	((or (member gr '((2 13) (2 32)))
	     (or (= (car gr) 11) (= (car gr) 25))
	 )
	 (setq loop nil
	       pt   nil
	 )
	)
      )
    )
    (if	pt
      (cond
	((ssget pt))
	(pt)
      )
    )
  )

  (setq	m:err	*error*
	*error*	ssu_err
  )
  (sssetfirst nil nil)
  (setq sst (ssadd))
  (while (and
	   (princ "\nChoix des objets: ")
	   (setq p1 (sel))
	 )
    (if	(listp p1)
      (progn
	(princ "\nSpécifiez le coin opposé: ")
	(setq p1 (list (car p1) (cadr p1)))
	(while (and (setq gr (grread T 12 0)) (/= (car gr) 3))
	  (if (= 5 (car gr))
	    (progn
	      (redraw)
	      (setq p2 (list (caadr gr) (cadr p1))
		    p3 (list (caadr gr) (cadadr gr))
		    p4 (list (car p1) (cadadr gr))
	      )
	      (if (< (car p1) (car p2))
		(progn
		  (setq po "_WP")
		  (grvecs (list 255 p1 p2 255 p2 p3 255 p3 p4 255 p4 p1)
		  )
		)
		(progn
		  (setq po "_CP")
		  (grvecs
		    (list -255 p1 p2 -255 p2 p3 -255 p3 p4 -255 p4 p1)
		  )
		)
	      )
	    )
	  )
	)
	(redraw)
	(if (setq ss (ssget po (list p1 p2 p3 p4)))
	  (repeat (setq n (sslength ss))
	    (setq ent (ssname ss (setq n (1- n))))
	    (if	(not (ssmemb ent sst))
	      (ssadd ent sst)
	    )
	    (sssetfirst nil sst)
	  )
	)
      )
      (progn
	(ssadd (ssname p1 0) sst)
	(sssetfirst nil sst)
      )
    )
  )
  (sssetfirst nil sst)
  (setq	*error*	m:err
	m:err nil
  )
  (princ)
)

;;;===============================================================;;;

;;; SelByObj -Gilles Chanteau- 06/10/06
;;; Crée un jeu de sélection avec tous les objets contenus ou 
;;; capturés, dans la vue courante, par l'objet sélectionné
;;; (cercle, ellipse, polyligne fermée).
;;; Arguments :
;;; - un nom d'entité (ename) 
;;; - un mode de sélection (Cp ou Wp)
;;; - un filtre de sélection ou nil
;;;
;;; modifié le 19/07/07 : fonctionne avec les objets hors fenêtre

(defun SelByObj	(ent opt fltr / obj dist n lst prec dist p_lst ss)
  (vl-load-com)
  (if (= (type ent) 'ENAME)
    (setq obj (vlax-ename->vla-object ent))
    (setq obj ent
	  ent (vlax-vla-object->ename ent)
    )
  )
  (cond
    ((member (vla-get-ObjectName obj) '("AcDbCircle" "AcDbEllipse"))
     (setq dist	(/ (vlax-curve-getDistAtParam
		     obj
		     (vlax-curve-getEndParam obj)
		   )
		   50
		)
	   n	0
     )
     (repeat 50
       (setq
	 lst
	  (cons
	    (trans
	      (vlax-curve-getPointAtDist obj (* dist (setq n (1+ n))))
	      0
	      1
	    )
	    lst
	  )
       )
     )
    )
    ((and (= (vla-get-ObjectName obj) "AcDbPolyline")
	  (= (vla-get-Closed obj) :vlax-true)
	  )
     (setq p_lst (vl-remove-if-not
		   '(lambda (x)
		      (or (= (car x) 10)
			  (= (car x) 42)
		      )
		    )
		   (entget ent)
		 )
     )
     (while p_lst
       (setq
	 lst
	  (cons
	    (trans (append (cdr (assoc 10 p_lst))
				 (list (cdr (assoc 38 (entget ent))))
			 )
			 ent
			 1
	    )
	    lst
	  )
       )
       (if (/= 0 (cdadr p_lst))
	 (progn
	   (setq prec (1+ (fix (* 25 (sqrt (abs (cdadr p_lst))))))
		 dist (/ (- (if	(cdaddr p_lst)
			      (vlax-curve-getDistAtPoint
				obj
				(trans (cdaddr p_lst) ent 0)
			      )
			      (vlax-curve-getDistAtParam
				obj
				(vlax-curve-getEndParam obj)
			      )
			    )
			    (vlax-curve-getDistAtPoint
			      obj
			      (trans (cdar p_lst) ent 0)
			    )
			 )
			 prec
		      )
		 n    0
	   )
	   (repeat (1- prec)
	     (setq
	       lst (cons
		     (trans
			 (vlax-curve-getPointAtDist
			   obj
			   (+ (vlax-curve-getDistAtPoint
				obj
				(trans (cdar p_lst) ent 0)
			      )
			      (* dist (setq n (1+ n)))
			   )
			 )
			 0
			 1
		       )
		     lst
		   )
	     )
	   )
	 )
       )
       (setq p_lst (cddr p_lst))
     )
    )
  )
  (cond
    (lst
     (vla-ZoomExtents (vlax-get-acad-object))
     (setq ss (ssget (strcat "_" opt) lst fltr))
     (vla-ZoomPrevious (vlax-get-acad-object))
     ss
    )
  )
)

;;;===============================================================;;;

;;; SSOC pour sélectionner tous les objets capturés, suivant
;;; la vue, par le cercle, l'ellipse ou la polyligne.

(defun c:ssoc (/ ss opt)
  (and
    (or
      (and
	(setq ss (cadr (ssgetfirst)))
	(= 1 (sslength ss))
      )
      (and
	(sssetfirst nil nil)
	(setq ss (ssget	"_:S:E"
			(list
			  '(-4 . "<OR")
			  '(0 . "CIRCLE")
			  '(-4 . "<AND")
			  '(0 . "ELLIPSE")
			  '(41 . 0.0)
			  (cons 42 (* 2 pi))
			  '(-4 . "AND>")
			  '(-4 . "<AND")
			  '(0 . "LWPOLYLINE")
			  '(-4 . "&")
			  '(70 . 1)
			  '(-4 . "AND>")
			  '(-4 . "OR>")
			)
		 )
	)
      )
    )
    (sssetfirst
      nil
      (ssdel (ssname ss 0) (SelByObj (ssname ss 0) "Cp" nil))
    )
  )
  (princ)
)

;;;===============================================================;;;

;;; SSOF pour sélectionner tous les objets contenus, suivant
;;; la vue, dans le cercle, l'ellipse ou la polyligne.

(defun c:ssof (/ ss opt)
  (and
    (or
      (and
	(setq ss (cadr (ssgetfirst)))
	(= 1 (sslength ss))
      )
      (and
	(sssetfirst nil nil)
	(setq ss (ssget	"_:S:E"
			(list
			  '(-4 . "<OR")
			  '(0 . "CIRCLE")
			  '(-4 . "<AND")
			  '(0 . "ELLIPSE")
			  '(41 . 0.0)
			  (cons 42 (* 2 pi))
			  '(-4 . "AND>")
			  '(-4 . "<AND")
			  '(0 . "LWPOLYLINE")
			  '(-4 . "&")
			  '(70 . 1)
			  '(-4 . "AND>")
			  '(-4 . "OR>")
			)
		 )
	)
      )
    )
    (sssetfirst
      nil
      (SelByObj (ssname ss 0) "Wp" nil)
    )
  )
  (princ)
)

;;;===============================================================;;;

;;; Inv_sel Inverse le jeu de sélection courant

(defun c:inv_sel (/ ssa ssf n)
  (setq
    ssa	(ssget "_A"
	       (list '(0 . "~VIEWPORT") (cons 410 (getvar "ctab")))
	)
  )
  (if (setq ssf (cadr (ssgetfirst)))
    (repeat (setq n (sslength ssf))
      (ssdel (ssname ssf (setq n (1- n))) ssa)
    )
  )
  (sssetfirst)
  (sssetfirst nil ssa)
)

;;;===============================================================;;;

;;; SSD version 2.6 (gile) 14/07/08 (dernière révision 17/11/2011)
;;; Sélection de blocs dynamiques par des valeurs de paramètres dynamiques
;;;
;;; Utilisations :
;;; - Pour créer un jeu de sélection, entrer SSD, sélectionner un bloc source
;;;   puis choisir les valeurs à filtrer dans la boite de dialogue.
;;; - A l'intérieur d'un commande de modification, à l'invite "Choix des objets: "
;;;   entrer (SSD).
;;; Le filtre peut se faire sur tout le dessin ou à l'intérieur d'une sélection

(defun ssd (/ *error* ToString DynBlkPropValue dz ss ent blk name pop fuzz ret sel res)

  (vl-load-com)
  (or *acad* (setq *acad* (vlax-get-acad-object)))
  (or *acdoc* (setq *acdoc* (vla-get-ActiveDocument *acad*)))

  (defun *error* (msg)
    (if	(/= msg "Fonction annulée")
      (princ (strcat "Erreur: " msg))
    )
    (and blk (not (redraw (vlax-vla-object->ename blk) 4)))
    (setvar 'dimzin dz)
    (princ)
  )

  (defun ToString (v u)
    (cond
      ((= 0 u) (vl-princ-to-string v))
      ((= 1 u) (angtos v (getvar 'aunits) 16))
      ((if (< (abs v) 1e-6)
	 (rtos v 1 6)
	 (rtos v (getvar 'lunits) 12)
       )
      )
    )
  )

  ;; DynBlkPropValue
  ;; Boite de dialogue pour choisir des valeurs de paramètres dynamiques
  ;;
  ;; Argument : la liste des paramètres dynamiques

  (defun DynBlkPropValue (lst / tmp file pn unt av dcl_id val)
    (setq tmp  (vl-filename-mktemp "Tmp.dcl")
	  file (open tmp "w")
    )
    (write-line
      (strcat
	"DynBlkProps:dialog{label=\"Filtre de blocs dynamiques\";"
	":text{label=\"Nom du bloc : \""
	(vl-prin1-to-string name)
	";}spacer;:boxed_column{label=\"Propriétés dynamiques\";"
      )
      file
    )
    (foreach p lst
      (setq pn	(vla-get-PropertyName p)
	    unt	(vla-get-UnitsType p)
      )
      (cond
	((setq av (vlax-get p 'AllowedValues))
	 (setq av  (mapcar '(lambda (x) (ToString x unt)) av)
	       pop (cons (vl-list* pn "*" av) pop)
	 )
	 (write-line
	   (strcat
	     ":popup_list{label="
	     (vl-prin1-to-string pn)
	     ";key="
	     (vl-prin1-to-string pn)
	     ";value="
	     (itoa (1+ (vl-position (ToString (vlax-get p 'Value) unt) av)))
	     ";edit_width=25;allow_accept=true;}"
	   )
	   file
	 )
	)
	((/= pn "Origin")
	 (setq fuzz (cons pn fuzz))
	 (write-line
	   (strcat
	     ":row{:edit_box{label="
	     (vl-prin1-to-string pn)
	     ";key="
	     (vl-prin1-to-string pn)
	     ";value="
	     (vl-prin1-to-string (ToString (vlax-get p 'Value) unt))
	     ";edit_width=18;allow_accept=true;}"
	     ":edit_box{label=\"Tolérance\";key="
	     (vl-prin1-to-string (strcat pn "_fuzz"))
	     ";value=\"1e-12\";edit_width=6;allow_accept=true;}}"
	   )
	   file
	 )
	)
      )
    )
    (write-line
      (strcat
	"}spacer;:radio_row{key=\"selset\";"
	":radio_button{label=\"Tout le dessin\";key=\"all\";value=\"1\";}"
	":radio_button{label=\"Sélection\";key=\"sel\";}}"
	"spacer;ok_cancel;}"
      )
      file
    )
    (close file)
    (setq dcl_id (load_dialog tmp))
    (if	(not (new_dialog "DynBlkProps" dcl_id))
      (exit)
    )
    (foreach p pop
      (start_list (car p))
      (mapcar 'add_list (cdr p))
      (end_list)
    )
    (action_tile
      "accept"
      "(foreach p (mapcar 'vla-get-PropertyName lst)
      (if (assoc p pop)
      (setq val (nth (atoi (get_tile p)) (cdr (assoc p pop))))
      (setq val (get_tile p)))
      (if (and val (/= val \"\") (/= val \"*\"))
      (setq ret (cons (cons p val) ret))))
      (setq fuzz (mapcar (function (lambda (x)
      (cons x (get_tile (strcat x \"_fuzz\"))))) fuzz))
      (and (not ret) (setq ret T))
      (setq sel (get_tile \"selset\"))
      (done_dialog)"
    )
    (action_tile "cancel" "(setq ret nil)")
    (start_dialog)
    (unload_dialog dcl_id)
    (vl-file-delete tmp)
    ret
  )

  ;;----------------------------------------------------;;

  (setq dz (getvar 'dimzin))
  (setvar 'dimzin 8)
  (and
    (or
      (and
	(setq ss (cadr (ssgetfirst)))
	(= 1 (sslength ss))
	(setq ent (ssname ss 0))
	(sssetfirst nil nil)
      )
      (and
	(sssetfirst nil nil)
	(setq ent (car (entsel)))
      )
    )
    (setq blk (vlax-ename->vla-object ent))
    (= (vla-get-ObjectName blk) "AcDbBlockReference")
    (= (vla-get-IsDynamicBlock blk) :vlax-true)
    (not (redraw ent 3))
    (setq name (vla-get-EffectiveName blk))
    (or
      (DynBlkPropValue (vlax-invoke blk 'getDynamicBlockProperties))
      (redraw ent 4)
    )
    (not (redraw ent 4))
    (if	(= sel "all")
      (ssget "_X"
	     (list '(0 . "INSERT") (cons 2 (strcat name ",`*U*")))
      )
      (ssget (list '(0 . "INSERT") (cons 2 (strcat name ",`*U*")))
      )
    )
    (setq res (ssadd))
    (vlax-for b	(setq ss (vla-get-ActiveSelectionSet *acdoc*))
      (if
	(and
	  (= (vla-get-EffectiveName b) name)
	  (or
	    (= ret T)
	    ((lambda (lst)
	       (apply
		 '=
		 (cons
		   T
		   (mapcar
		     (function
		       (lambda (p / n v l u f)
			 (setq n (car p)
			       l (assoc n lst)
			       u (vla-get-UnitsType (caddr l))
			 )
			 (equal	(cond
				  ((= 0 u) (cdr p))
				  ((= 1 u) (angtof (cdr p)))
				  (T (distof (cdr p)))
				)
				(if (= u 0)
				  (vl-princ-to-string (cadr l))
				  (cadr l)
				)
				(if (and (setq f (cdr (assoc n fuzz)))
					 (numberp (read f))
				    )
				  (atof f)
				  1e-12
				)
			 )
		       )
		     )
		     ret
		   )
		 )
	       )
	     )
	      (mapcar
		(function
		  (lambda (p / n v)
		    (list
		      (setq n (vla-get-PropertyName p))
		      (vlax-get p 'Value)
		      p
		    )
		  )
		)
		(vlax-invoke b 'getDynamicBlockProperties)
	      )
	    )
	  )
	)
	 (ssadd (vlax-vla-object->ename b) res)
      )
    )
    (vla-delete ss)
  )
  (setvar 'dimzin dz)
  res
)

(defun c:ssd ()
  (sssetfirst nil (ssd))
  (princ)
)