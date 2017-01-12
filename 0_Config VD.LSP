
(vl-load-com)
(setq *AcDoc* (vla-get-activeDocument (vlax-get-acad-object)))


;;;*****************************************************************************
;;;*****************************************************************************
;;;************		PARAMETRES PERSONNALISES		****************
;;;*****************************************************************************
;;;*****************************************************************************


(defun c:vd (/)


;;;			VARIABLES NON ENREGISTREES				

(setvar "HPANNOTATIVE" 0)
;;Crée des achures non annotatives
(setvar "HPDRAWORDER" 1)
;;place les hachures en arrière plan
(setvar "OSNAPZ" 1)
;;Accrobj substitue la valeur Z du point spécifié par l'élévation courante
(setvar "MODEMACRO" "Config VD")
;;Affiche une chaîne de caractères sur la ligne d'état

  
;;;		VARIABLES ENREGISTREES DANS LE REGISTRE				

  (setvar "cmdecho" 0)
  (command "BARRETACHES" 0)
  (setvar "cmdecho" 1)
  
;; GESTION DU ALT TAB  
(setvar "EDGEMODE" 1)
;; Intersection projetées pour les commandes ajuster et prolonger
(setvar "FIELDDISPLAY" 0)
;; Masque l'arrière plan des champs
(setvar "DYNPICOORDS" 0)
;; Coordonnées relatives pour la saisie du second point
(setvar "PROXYSHOW" 0)
;; N'affiche pas les gbraphiques externes
(setvar "PROXYNOTICE" 0)
;; Aucun avertissement concernant les objets externes n'est affiché.
(setvar "XREFTYPE" 1)
;; Les Xref sont superposées (pour éviter les imbrications en chaîne)
(setvar "ANNOAUTOSCALE" -1)
;; n'ajoute pas l'échelle aux objets gelés, vérouillés ou desactivés
(setvar "HPMAXLINES" 10000000)
;; Gère le nombre maximum de lignes de hachures qui seront générées
(setvar "HPSEPARATE" 1)
;; Des objets de hachures distincts sont créés
(setvar "HPASSOC" 1)
;; Crée des hachures associatives
(setvar "OSMODE" 7095)
;; Définit les accrochages aux objets actifs
(setvar "OSNAPNODELEGACY" 1)
;; L'accrochage aux objets Noeud ignore le texte multiligne.
  
;;;(setvar "POLARANG" (angtof "15"))
;; Définit l'incrément des angles polaires
  
(setvar "LAYERFILTERALERT" 3)
;;Lorsque le dessin est ouvert, affiche un message indiquant le problème et
;;Affiche une boîte de dialogue pour sélectionner les filtres à supprimer
(setvar "PAPERUPDATE" 1)
;;Définit le format de papier en fonction de la valeur spécifiée
;;dans le fichier de configuration du traceur
(setvar "PEDITACCEPT" 1)
;;L'invite est supprimée, l'objet sélectionné est automatiquement converti en polyligne
(setvar "RECOVERYMODE" 1)
;;La fenêtre de récupération ne s'affiche pas après un plantage.
(setvar "LAYLOCKFADECTL" 50)
;;Gère l'atténuation des objets sur les calques verrouillés
(setvar "PICKADD" 0)
;choix du mode de sélection
(setvar "PICKBOX" 4)
;;Définit, en pixels, la hauteur de la cible de sélection d'objets.
(setvar "PUBLISHALLSHEETS" 0)
;;Seul le contenu du document courant
(setvar "LWUNITS" 1)
;;unité mm pour les épaisseurs de polyligne

  
;;;		VARIABLES ENREGISTREES DANS LE DESSIN				

(setq lay0 (vla-item (vla-get-layers *AcDoc*) "0"))
(vla-put-lock lay0 :vlax-false)
(and (equal :vlax-true (vla-get-freeze lay0)) (vla-put-freeze lay0 :vlax-false))
(vla-put-layeron (vla-item (vla-get-layers *AcDoc*) "0") :vlax-true)
(setq lstvar (list "ANNOALLVISIBLE"      "CECOLOR"
		   "CELTYPE"	    "CELWEIGHT"	     "CLAYER"
		   "GRIDMODE"	    "LAYEREVAL"	     "LAYERNOTIFY"
		   "LWDISPLAY"	    "MEASUREMENT"    "MSLTSCALE"
		   "PSLTSCALE" 
		  )
      lstvdvar '(0 "BYLAYER" "ByLayer" -1 "0" 0 1 4 1 1 0 0)
      lstvarmodif nil
)



(repeat (setq n (length lstvar))  
  (if (not (equal (setq x (getvar (setq y (nth (setq n (1- n)) lstvar)))) (setq z (nth n lstvdvar))))
    (progn
      (setq lstvarmodif (cons (strcat "(setvar \"" y "\" " (vl-prin1-to-string x) ")") lstvarmodif))
      (setvar y z)
  )))


;;;		  PASSE LE CALQUE 0 EN ROUGE (TRUECOLOR)			

(setq lstlay0 (entget (tblobjname "layer" "0")))
(if (assoc 420 lstlay0)
  (entmod
    (subst (cons 420 16711680) (assoc 420 lstlay0) lstlay0)
  )
  (entmod (append lstlay0 (list (cons 420 16711680))))
)


;;;				CHARGEMENT DES LISP				

(load "0_General")

(load "2_ST")


);defun vd

;;;										
;;;				CREATION D'ALIAS				
;;;										

(defun c:pgp() (command "ai_editcustfile" "acad.pgp"))

(defun c:mp() (command "corresprop"))  

(defun c:rc() (command "rectang"))  

(defun c:mep () (command "misenpage"))

(defun c:ql () (command "qleader"))

(defun c:cl () (command "laymcur"))

(defun c:cmt () (command "_TXT2MTXT"))

(defun c:f () (command "_-vports"))

(defun c:pc () (command "publishcollate"))

(defun c:nr () (command "nuagerev"))

(defun c:di () (command "dist"))

(defun c:ed () (command "_textedit"))

(defun c:qs () (command "_qselect"))

(defun c:uf () (command "updatefield"))


;;;										
;;;				CHARGEMENT CONFIG VD				
;;;										

(c:vd)

