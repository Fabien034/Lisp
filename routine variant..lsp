(vl-load-com)
(or *acad* (setq *acad* (vlax-get-acad-object)))
(or *acdoc* (setq *acdoc* (vla-get-ActiveDocument *acad*)))
(or *blocks* (setq *blocks* (vla-get-Blocks *acdoc*)))
(or *layers* (setq *layers* (vla-get-Layers *acdoc*)))
(or *util* (setq *util* (vla-get-Utility *acdoc*)))

;;--------------- VARIANTS & SAFEARRAYS ----------------;;

;; Variant -> LISP

;; gc:VariantToLispData
;; Convertit un variant ou un safearray en donnée LISP
;;
;; Argument
;; var : variant ou safearray

(defun gc:VariantToLispData (var)
  (cond
    ((= (type var) 'variant)
     (gc:VariantToLispData (vlax-variant-value var)))
    ((= (type var) 'safearray)
     (mapcar 'gc:VariantToLispData (vlax-safearray->list var))
    )
    (T var)
  )
)

;; gc:2dVariantToPointList
;; Convertit un variant de coordonnées 2D en liste de points
;; LightweightPolyline dans le SCO
;;
;; Argument
;; var : un variant (array de doubles) tel que retourné par vla-get-Coordinates

(defun gc:2dVariantToPointList (var / foo)
  (defun foo (lst)
    (if lst
      (cons (list (car lst) (cadr lst)) (foo (cddr lst)))
    )
  )
  (foo (vlax-safearray->list (vlax-variant-value var)))
)

;; gc:3dVariantToPointList
;; Convertit un variant de coordonnées 3D en liste de points
;; Polyline dans le SCO (Z = 0)
;; 3DFace, 3DPolyline, Leader, MLine, PolyfaceMesh,
;; PolygonMesh, Solid, Trace dans le SCG
;;
;; Argument
;; var : un variant (array de doubles) tel que retourné par vla-get-Coordinates

(defun gc:3dVariantToPointList (var / foo)
  (defun foo (lst)
    (if lst
      (cons (list (car lst) (cadr lst) (caddr lst)) (foo (cdddr lst)))
    )
  )
  (foo (vlax-safearray->list (vlax-variant-value var)))
)

;; gc:VariantsToDxfList
;; Retourne une liste d'association (type liste DXF)
;;
;; Arguments
;; xtyp : variant (array d'entiers)
;; xval : varinat (array de variants)

(defun gc:VariantsToDxfList (xtyp xval)
  (mapcar 'cons (gc:VariantToLispData xtyp) (gc:VariantToLispData xval))
)

;; gc:GetXdata
;; Retourne la liste DXF des données étendues de l'objet
;;
;; Arguments
;; obj : (vla-object) l'objet auquel sont liées les données étendues
;; app : (string) le nom de l'application enregistrée ("" pour toutes les applications)

(defun gc:GetXdata (obj app / xtyp xval)
  (vla-GetXdata obj app 'xtyp 'xval)
  (gc:VariantsToDxfList xtyp xval)
)

;; gc:GetXrecordData
;; Retourne la liste DXF des données de l'objet XRECORD
;;
;; Arguments
;; xrec : (vla-object) l'objet XRECORD auquel sont liées les données

(defun gc:GetXrecordData (xrec / xtyp xval)
  (vla-GetXrecordData xrec 'xtyp 'xval)
  (gc:VariantsToDxfList xtyp xval)
)

;;------------------------------------------------------;;

;; LISP -> variant

;; gc:2dPointListToVariant (gile)
;; Retourne un variant de coordonnées 2d
;;
;; Argument : une liste de points 2d

(defun gc:2dPointListToVariant (lst)
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray
        vlax-VbDouble
        (cons 0 (1- (* 2 (length lst))))
      )
      (apply 'append lst)
    )
  )
)

;; gc:3dPointListToVariant (gile)
;; Retourne un variant de coordonnées 3d
;;
;; Argument : une liste de points 3d

(defun gc:3dPointListToVariant (lst)
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray
        vlax-VbDouble
        (cons 0 (1- (* 3 (length lst))))
      )
      (apply 'append lst)
    )
  )
)

;; gc:ObjectListToVariant
;; Retourne un variant (array d'objets)
;;
;; Argument
;; lst : liste de vla-objet

(defun gc:ObjectListToVariant (lst)
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray
        vlax-vbObject
        (cons 0 (1- (length lst)))
      )
      lst
    )
  )
)

;; gc:DxfListToVariants
;; Définit 2 variables et affecte un variant à chacune
;;
;; Arguments
;; lst : une liste DXF
;; typeSymbol : un symbole quoté
;; valueSymbol : un symbole quoté

(defun gc:DxfListToVariants (lst typeSymbol valueSymbol)
  (set typeSymbol
       (vlax-make-variant
         (vlax-safearray-fill
           (vlax-make-safearray
             vlax-vbInteger
             (cons 0 (1- (length lst)))
           )
           (mapcar 'car lst)
         )
       )
  )
  (set valueSymbol
       (vlax-make-variant
         (vlax-safearray-fill
           (vlax-make-safearray
             vlax-vbVariant
             (cons 0 (1- (length lst)))
           )
           (mapcar '(lambda (x)
                      (if (listp (setq x (cdr x)))
                        (vlax-3d-point x)
                        (vlax-make-variant x)
                      )
                    )
                   lst
           )
         )
       )
  )
)


;; gc:SetXdata
;; Attribue des données étendues à un objet
;;
;; Arguments
;; obj : (vla-object) l'objet auquel sont liées les données
;; lst : (liste DXF) les données sous la forme :
;; '((1001 . "Nom_App") (1002 . "{") (1000 . "chaîne") (1070 . 1) (1002 . "}"))

(defun gc:SetXdata (obj lst / xtyp xval)
  (gc:DxfListToVariants lst 'xtyp 'xval)
  (vla-SetXdata obj xtyp xval)
)

;; gc:SetXrecordData
;; Attribue des données à un objet Xrecord
;;
;; Arguments
;; xrec : (vla-object) l'objet  Xrecord
;; lst : (liste DXF) les données sous la forme :
;; '((1 . "chaîne") (70 . 1) (10 1.0 2.0 0.0))

(defun gc:SetXrecordData (xrec lst / xtyp xval)
  (gc:DxfListToVariants lst 'xtyp 'xval)
  (vla-SetXrecordData xrec xtyp xval)
)

;;-------------------- Safe methods --------------------;;

;; GetItem (gile)
;; Retourne le vla-object de l'item s'il est présent dans la collection (ou nil)
;;
;; Arguments
;; col : la collection (vla-object)
;; name : le nom de l'objet (string) ou son indice (entier)

(defun gc:GetItem (col name / obj)
  (vl-catch-all-apply
    (function (lambda () (setq obj (vla-Item col name))))
  )
  obj
)

;; gc:GetObject
;; Retourne un objet (Custom Object) du dictionnaire d'après son nom (ou nil)
;; Arguments
;; dict : le dictionnaire (vla-object)
;; name : le nom de l'objet (string)

(defun gc:GetObject (dict name / obj)
  (vl-catch-all-apply
    (function (lambda () (setq obj (vla-GetObject dict name))))
  )
  obj
)

;; gc:HandleToObject
;; Retourne un objet (vl-object) d'après son 'handle' (ou nil s'il est effacé)
;; Arguments
;; doc    : le document auquel appartient l'objet (vla-object)
;; handle : le 'handle' de l'objet (string)

(defun gc:HandleToObject (doc handle / obj)
  (vl-catch-all-apply
    (function (lambda () (setq obj (vla-HandleToObject doc handle))))
  )
  obj
)

;;-------------------- TRUE COLORS ---------------------;;

;; gc:GetAcCmColor
;; Retourne un objet acCmColor (utilisable avec 'vla-put-TrueColor'
;; avec la couleur choisie dans la boite de dialogue standard
;;
;; Arguments
;; color         : une paire pointée correspondant à la couleur par défaut
;; allowbylayer  : si nil, DuBloc et DuCalque sont désactivés dans la boite de dialogue

(defun gc:GetAcCmColor (color allowbylayer / accmcolor truecolor)
  (if (setq color (acad_truecolordlg color allowbylayer))
    (progn
      (setq accmcolor (vla-GetInterfaceObject
		     *acad*
		     (strcat "AutoCAD.AcCmColor." (itoa (atoi (getvar 'acadver))))
		   )
      )
      (if (setq truecolor (cdr (assoc 420 color)))
	(vla-SetRGB
	  accmcolor
	  (lsh (fix truecolor) -16)
	  (lsh (lsh (fix truecolor) 16) -24)
	  (lsh (lsh (fix truecolor) 24) -24)
	)
	(vla-put-ColorIndex accmcolor (cdr (assoc 62 color)))
      )
    )
  )
  accmcolor
)

;; gc:TrueColorToList
(defun gc:TrueColorToList (accmcolor / ci cm tc bn)
  (setq	ci (vla-get-ColorIndex accmcolor)
	cm (vla-get-ColorMethod accmcolor)
  )
  (cond
    ((= cm 192) '((62 . 256)))
    ((= cm 193) '((62 . 0)))
    ((= cm 195) (list (cons 62 ci)))
    (T
     (setq tc (+ (lsh (fix (vla-get-Red accmcolor)) 16)
		 (lsh (fix (vla-get-Green accmcolor)) 8)
		 (fix (vla-get-Blue accmcolor))
	      )
     )
     (if (= "" (setq bn (vla-get-BookName accmcolor)))
       (list (cons 62 ci)
	     (cons 420 tc)
       )
       (list (cons 62 ci)
	     (cons 420 tc)
	     (cons 430 (strcat bn "$" (vla-get-ColorName accmcolor)))
       )
     )
    )
  )
)

;;-------------------- OBJECT DBX ----------------------;;

;;; gc:GetAxDbDoc
;;; Accéder à un dessin fermé.
;;; Version compatible A2013 utilise les fonctions de irneb @ TheSwamp
;;; DLLRegister, ProgID->ClassID et DBX-Register
;;; http://www.theswamp.org/index.php?topic=40871.msg461180#msg461180
;;;
;;; Retourne :
;;; un objet IAxDbDocument si le document est trouve
;;; nil si le document n'a pu être trouvé ou s'il est ouvert.
;;;
;;; Argument :
;;; filename : le chemin complet du fichier
;;;
;;; Exemple d'utilisation :
;;; (if (setq doc (gc:GetAxDbDoc filename))
;;;   (progn
;;;     ...
;;;     ...
;;;     (vlax-release-object doc)
;;;   )
;;; )

(defun gc:GetAxDbDoc (filename / classname axdbdoc)
  (vl-load-com)
  (if (and
	(setq classname (DBX-Register nil nil))
	(setq axdbdoc (vlax-create-object className))
      )
    (if	(vl-catch-all-apply
	  'vla-open
	  (list axdbdoc filename)
	)
      (not (vlax-release-object axdbdoc))
      axdbdoc
    )
  )
)