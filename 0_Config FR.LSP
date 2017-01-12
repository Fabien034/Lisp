;;;*****************************************************************************
;;;*****************************************************************************
;;;************	        	CONFIGUE AUTOCAD FABIEN 	****************
;;;*****************************************************************************
;;;*****************************************************************************


;;;										
;;;			Réglage perso    					
;;;										

(defun c:fr (/)

;;;										
;;;			VARIABLES NON ENREGISTREES				
;;;										

  (setvar "HPDRAWORDER" 1)
  ;;place les hachures en arrière plan
  (setvar "OSNAPZ" 1)
  ;;Accrobj substitue la valeur Z du point spécifié par l'élévation courante
  (setvar "modemacro" "Config FR")
  ;;Affiche une chaîne de caractères sur la ligne d'état


;;;										
;;;		VARIABLES ENREGISTREES DANS LE REGISTRE				
;;;										


  ;; GESTION DU ALT TAB  
  (setvar "EDGEMODE" 0)
  ;; PAS d'Intersection projetées pour les commandes ajuster et prolonger
  (setvar "FIELDDISPLAY" 0)
  ;; Masque l'arrière plan des champs
  (setvar "DYNPICOORDS" 0)
  ;; Coordonnées relatives pour la saisie du second point
  (setvar "PROXYSHOW" 0)
  ;; N'affiche pas les graphiques externes
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
  (setvar "osmode" 6655)
  ;; Définit les accrochages aux objets actifs
  (setvar "OSNAPNODELEGACY" 0)
  ;; L'accrochage aux objets Noeud des texte multiligne.
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
  (setvar "PICKADD" 2)
					;choix du mode de sélection
  (setvar "PICKBOX" 4)
  ;;Définit, en pixels, la hauteur de la cible de sélection d'objets.
  (setvar "PUBLISHALLSHEETS" 0)
  ;;Seul le contenu du document courant
  (setvar "LWUNITS" 1)
  ;;unité mm pour les épaisseurs de polyligne


;;;										
;;;		  PASSE LE CALQUE 0 EN ROUGE (TRUECOLOR)			
;;;										

  (setq lstlay0 (entget (tblobjname "layer" "0")))
  (if (assoc 420 lstlay0)
    (entmod
      (subst (cons 420 16711680) (assoc 420 lstlay0) lstlay0)
    )
    (entmod (append lstlay0 (list (cons 420 16711680))))
  )

)					;defun FR


(c:fr)

;;;										
;;;				CHARGEMENT DES LISP				
;;;										



(load "0_General")
(load "2_CVC")
(load "2_DES")


;;;				C:LI	C:LO	C:LP				
;;;										
;;;				Commandes de cotation				
;;;										

(defun c:li (/ *error*)

  (defun *error* (msg)
    (if	(= msg "Fonction annulée")
      (progn
	(princ (strcat "\nErreur: " msg))
	(setvar "clayer" clayer)
	(vla-endundomark *acdoc*)
	(princ)
      )
    )
  )


  (vla-StartUndoMark *AcDoc*)
  (setq	clayer	(getvar "clayer")
	cecolor	(getvar "cecolor")
  )
  (MCUR	(strcat	"0 "
		entreprise
		" "
		(nth 2 (gc:str2lst clayer " "))
		" PRO P - Cotation G"
	)
	253
  )
  (command "_dimlinear" pause pause pause)
  (setvar "clayer" clayer)
  (setvar "cecolor" cecolor)
  (vla-EndUndoMark *AcDoc*)
)


(defun c:lo (/ *error*)

  (defun *error* (msg)
    (if	(= msg "Fonction annulée")
      (progn
	(princ (strcat "\nErreur: " msg))
	(setvar "clayer" clayer)
	(vla-endundomark *acdoc*)
	(command "_undo")
	(princ)
      )
    )
  )

  (setq	clayer	(getvar "clayer")
	cecolor	(getvar "cecolor")
  )
  (MCUR	(strcat	"0 "
		entreprise
		" "
		(nth 2 (gc:str2lst clayer " "))
		" PRO P - Cotation G"
	)
	253
  )
  (command "_dimaligned" pause pause pause)
  (setvar "clayer" clayer)
  (setvar "cecolor" cecolor)
  (vla-EndUndoMark *AcDoc*)
)


(defun c:lp (/ *error*)

  (defun *error* (msg)
    (if	(= msg "Fonction annulée")
      (progn
	(princ (strcat "\nErreur: " msg))
	(setvar "clayer" clayer)
	(vla-endundomark *acdoc*)
	(princ)
      )
    )
  )

  (vla-StartUndoMark *AcDoc*)
  (setq	clayer	(getvar "clayer")
	cecolor	(getvar "cecolor")
  )
  (MCUR	(strcat	"0 "
		entreprise
		" "
		(nth 2 (gc:str2lst clayer " "))
		" PRO P - Cotation G"
	)
	253
  )
  (command "_dimcontinue"
	   (while (not (zerop (getvar "cmdactive")))
	     (command pause)
	   )
  )
  (setvar "clayer" clayer)
  (setvar "cecolor" cecolor)
  (vla-EndUndoMark *AcDoc*)
)



;;;										
;;;				CREATION D'ALIAS				
;;;										

(defun c:pgp () (command "ai_editcustfile" "acad.pgp"))

(defun c:cr () (command "corresprop"))

(defun c:ql () (command "qleader"))

(defun c:cl () (command "laymcur"))

(defun c:f () (command "_-vports"))

(defun c:pc () (command "publishcollate"))

(defun c:nr () (command "nuagerev"))

(defun c:di () (command "dist"))

(defun c:ca () (command "CALCRAPIDE"))

(defun c:dv () (command "decaler" "e" "o"))

(defun c:dc () (command "decaler" "e" "n"))

(defun c:f1 () (command "_vscurrent" "2"))
(defun c:f2 () (command "_vscurrent" "c"))

(defun c:hi () (command "_isolateobjects"))
(defun c:hr () (command "_unisolateobjects"))
(defun c:hh () (command "_hideobjects"))

(defun c:v5 ()
  (command "-vue" "g" "h")
  (c:f1)
  (setq	layHaut	  "0 Xref 2D Haut"
	layDroite "0 Xref 2D Droite"
	layFace	  "0 Xref 2D Face"
  )
  (if (tblsearch "LAYER" layHaut)
    (vla-put-freeze (vla-item *layers* layHaut) :vlax-false)
  )
  (if (tblsearch "LAYER" layDroite)
    (vla-put-freeze (vla-item *layers* layDroite) :vlax-True)
  )
  (if (tblsearch "LAYER" layFace)
    (vla-put-freeze (vla-item *layers* layFace) :vlax-True)
  )
  (vla-regen *acdoc* acActiveViewport )
)
(defun c:v6 ()
  (command "-vue" "g" "d")
  (c:f1)
  (setq	layHaut	  "0 Xref 2D Haut"
	layDroite "0 Xref 2D Droite"
	layFace	  "0 Xref 2D Face"
  )
  (if (tblsearch "LAYER" layHaut)
    (vla-put-freeze (vla-item *layers* layHaut) :vlax-True)
  )
  (if (tblsearch "LAYER" layDroite)
    (vla-put-freeze (vla-item *layers* layDroite) :vlax-false)
  )
  (if (tblsearch "LAYER" layFace)
    (vla-put-freeze (vla-item *layers* layFace) :vlax-True)
  )
  (vla-regen *acdoc* acActiveViewport )
)
(defun c:v2 ()
  (command "-vue" "g" "a")
  (c:f1)
  (setq	layHaut	  "0 Xref 2D Haut"
	layDroite "0 Xref 2D Droite"
	layFace	  "0 Xref 2D Face"
  )
  (if (tblsearch "LAYER" layHaut)
    (vla-put-freeze (vla-item *layers* layHaut) :vlax-True)
  )
  (if (tblsearch "LAYER" layDroite)
    (vla-put-freeze (vla-item *layers* layDroite) :vlax-True)
  )
  (if (tblsearch "LAYER" layFace)
    (vla-put-freeze (vla-item *layers* layFace) :vlax-false)
  )
  (vla-regen *acdoc* acActiveViewport )
)
(defun c:v3 ()
  (command "-vue" "_swiso")
  (c:f2)
  (setq	layHaut	  "0 Xref 2D Haut"
	layDroite "0 Xref 2D Droite"
	layFace	  "0 Xref 2D Face"
  )
  (if (tblsearch "LAYER" layHaut)
    (vla-put-freeze (vla-item *layers* layHaut) :vlax-True)
  )
  (if (tblsearch "LAYER" layDroite)
    (vla-put-freeze (vla-item *layers* layDroite) :vlax-True)
  )
  (if (tblsearch "LAYER" layFace)
    (vla-put-freeze (vla-item *layers* layFace) :vlax-True)
  )
  (vla-regen *acdoc* acActiveViewport )
)



















































































































































































