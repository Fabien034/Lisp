;;;*****************************************************************************
;;;*****************************************************************************
;;;************	        	VARIABLE AUTOCAD GENERAL 	****************
;;;*****************************************************************************
;;;*****************************************************************************


;;; Variable général
(setq entreprise "OTEIS")

;;; VARIABLE NORMALISATION DES CLAQUES

;;; Normalisation d'un calque de type:
;;; Numero Entreprise Lot Statut Vue - Designation01 Designation02 TypeEntitée	
;;;   0         1      2     3    4  5       6             7           (FIN) 	

(or calque-lot
    (setq calque-lot (list "ARC" "CVC" "DES" "Pb" "ST" "VRD" "EL" "FM" "SSI"))    
)
(or calque-statut
    (setq calque-statut (list "Pro" "Demol" "Edl"))    
)
(or calque-vue
    (setq calque-vue (list "Plan" "Coupe" "Schema"))    
)
(or calque-type-entite
    (setq calque-type-entite (list "Graphique" "Hachure" "Texte" "Cotation" "Débit"))    
)
(or nb-take-type-entite
    (setq nb-take-type-entite 3)
)
(or calque-presentation
    (setq calque-presentation
	   (list (list "00 Window" (list (cons 62 51)))
		 (list "00 Cadre" (list (cons 62 7)))
	   )
    )
)




;;; VARIABLE LOT GENIE CLIMATIQUE					
;;; Définition des listes de variables					
;;; garder l'ordre initial des listes, ajouter à la fin des listes	
;;; sous liste dans l'orde "Abréviation" "Dénomination" "DXF"		


;;; Variables Ventilation
;;; SYSTEME
(or cvc-syst-ventil
    (setq cvc-syst-ventil
	   (list (list "CTA" "Centrale Traitement Air" (list (cons 62 150)))
		 (list "DF" "Double Flux" (list (cons 62 150)))
		 (list "UTA" "Unitée Traitement Air" (list (cons 62 140)))
		 (List "VC" "Ventilo Convecteur" (list (cons 62 130)))
		 (list "VMC"
		       "Ventilation Mécanique Controlée"
		       (list (cons 62 90))
		 )
		 (list "DES" "Désenfumage" (list (cons 62 30)))
		 (list "EXT" "Extracteur" (list (cons 62 80)))
		 (list "HOT" "Hotte" (list (cons 62 80)))
	   )
    )
)
;;; TYPE DE SYSTEME
(or cvc-type-syst-ventil
    (setq cvc-type-syst-ventil
	   (list (list "AE" "Extraction" (list (cons 62 91)))
		 (list "AR" "Reprise" (list (cons 62 71)))
		 (list "RE" "Rejet" (list (cons 62 101)))
		 (list "AS" "Soufflage" (list (cons 62 151)))
		 (list "AN" "AirNeuf" (list (cons 62 141)))
		 (list "AI" "Induction" (list (cons 62 161)))
		 (list "AC" "Compensation" (list (cons 62 131)))
		 (list "S" "Sans type")
	   )
    )
)
;;; Variables Hydraulique
;;; SYSTEME
(or cvc-syst-hydro
    (setq cvc-syst-hydro
	   (list (List "PAC" "Pompe A Chaleur" (list (cons 62 120)))
		 (List "DRV"
		       "Débit Réfrigérant Variable"
		       (list (cons 62 180))
		 )
		 (List "DD"
		       "Détente directe"
		       (list (cons 62 180))
		 )
		 (List "CH" "Chaudiere" (list (cons 62 20)))
		 (List "GEG" "Groupe Eau Glacee" (list (cons 62 130)))
		 (List "Rad" "Radiateur" (list (cons 62 10)))
		 (List "PSL" "Panneau Solaire" (list (cons 62 40)))
		 (List "PC" "Plancher Chauffant" (list (cons 62 30)))
		 (List "PCR"
		       "Plancher Chauffant Rafraichissant"
		       (list (cons 62 30))
		 )		 
	   )
    )
)
;;; TYPE DE SYSTEME
(or cvc-type-syst-hydro
    (setq cvc-type-syst-hydro
	   (list (list "EC" "Eau Chaude" (list (cons 62 31)))
		 (list "EF" "Eau Froide" (list (cons 62 151)))
		 (list "EG" "Eau Glacée" (list (cons 62 131)))
		 (list "CO" "Change-Over" (list (cons 62 71)))
		 (list "FR" "Fluide Frigorifique" (list (cons 62 161)))
		 (list "S" "Sans type")
	   )
    )
)


;;; ELEMENT SYSTEME
(or cvc-element-syst
    (setq cvc-element-syst
	   (list (list "T" "Terminaux")
		 (list "D" "Distribution")
		 (list "P" "Production")
		 (list "E" "Equipement")
		 (list "C" "CCF" (list (cons 62 7)))
		 (list "AX" "Axe" (list (cons 62 252)(cons 6 "AXES")))
		 (list "AN" "Annotation" (list (cons 62 7)))
		 (list "I" "Isolant")
		 (list "S" "Sans type")
	   )
    )
)

;;; Eapaiseur type de trait
(or epaisseur-trait
    (setq epaisseur-trait
	   (list (list "VU" "Vu")
		 (list "CA" "Cache")
		 (list "CO" "Coupe")
		 (list "F" "Fin")
		 (list "M" "Moyen")
		 (list "E" "Epais")
	   )
    )
)


















































































































