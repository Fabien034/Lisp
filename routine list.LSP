;; gc:butLast
;; Retourne la liste priv�e du dernier �l�ment
;;
;; Argument
;; l : une liste
(defun gc:butLast (l) (reverse (cdr (reverse l))))

;; gc:massoc
;; Retourne la liste de toutes les valeurs pour le code sp�cifi� dans une liste d'association
;;
;; Arguments
;; key : la cl� � rechercher dans la liste
;; alst : une liste d'association
(defun gc:massoc (key alst)
  (if (setq alst (member (assoc key alst) alst))
    (cons (cdar alst) (gc:massoc key (cdr alst)))
  )
)

;; gc:breakAt
;; Retourne une liste de deux sous listes,
;; la premi�re contenant les n premiers �l�ments, la seconde les �l�ments restants
;;
;; Arguments
;; n : le nombre d'�l�ments pour la premi�re sous liste
;; l : une liste
(defun gc:breakAt (n l / r)
  (while (and l (< 0 n))
    (setq r (cons (car l) r)
	  l (cdr l)
	  n (1- n)
    )
  )
  (list (reverse r) l)
)

;; gc:take
;; Retourne les n premiers �l�ments de la liste
;;
;; Arguments
;; n : le nombre d'�l�ments
;; l : une liste
(defun gc:take (n l)
  (if (and l (< 0 n))
    (cons (car l) (gc:take (1- n) (cdr l)))
  )
)

;; gc:skip
;; Retourne la liste moins les n premiers �l�ments
;;
;; Arguments
;; n : le nombre d'�l�ments
;; l : une liste
(defun gc:skip (n l)
  (if (and l (< 0 n))
    (gc:skip (1- n) (cdr l))
    l
  )
)

;; gc:split
;; Divise une liste en sous-listes de la longueur sp�cifi�e
;;
;; Arguments
;; n : le nombre d'�l�ments de chaque sous liste
;; l : une liste
(defun gc:split	(n l / s)
  (if (and l (setq s (gc:breakat n l)))
    (cons (car s) (gc:split n (cadr s)))
  )
)

;; gc:split2
;; Convertit une liste de coordonn�es 2D en liste de points 2D
;;
;; Argument
;; l : une liste
(defun gc:split2 (l)
  (if l
    (cons (list (car l) (cadr l))
	  (gc:split2 (cddr l))
    )
  )
)

;; gc:split3
;; Convertit une liste de coordonn�es 3D en liste de points
;;
;; Argument
;; l : une liste
(defun gc:split3 (l)
  (if l
    (cons (list (car l) (cadr l) (caddr l))
	  (gc:split3 (cdddr lst))
    )
  )
)

;; gc:sublist
;; Retourne la sous liste de n �l�ments � partir de i
;;
;; Arguments
;; i : index du premier �l�ment
;; n : nombre d'�l�ments
;; l : une liste
(defun gc:sublist (i n l)
  (gc:take n (gc:skip i l))
)

;; gc:intersect
;; Retourne la liste des �l�ments communs � l1 et l2
;;
;; Arguments
;; l1 : une liste
;; l2 : une liste
(defun gc:intersect (l1 l2)
  (if l1
    (if	(member (car l1) l2)
      (cons (car l1) (gc:intersect (cdr l1) l2))
      (gc:intersect (cdr l1) l2)
    )
  )
)

;; gc:substract
;; Retourne une liste contenant les �l�ments appartenant exclusivement � l1
;;
;; Arguments
;; l1 : une liste
;; l2 : une liste
(defun gc:substract (l1 l2)
  (if l1
    (if	(member (car l1) l2)
      (gc:substract (cdr l1) l2)
      (cons (car l1) (gc:substract (cdr l1) l2))
    )
  )
)

;; gc:distinct
;; Suprime tous les doublons d'une liste
;;
;; Argument
;; l : une liste
(defun gc:distinct (l)
  (if l
    (cons (car l) (gc:distinct (vl-remove (car l) l)))
  )
)

;; gc:removeFirst
;; Retourne la liste sans la premi�re occurence de l'expression
;;
;; Arguments
;; ele : l'�l�ment � supprimer
;; lst : la liste
(defun gc:removeFirst (ele lst)
  (if (equal ele (car lst))
    (cdr lst)
    (cons (car lst) (gc:removeFirst ele (cdr lst)))
  )
)

;; gc:insertAt
;; Ins�re l'�l�ment dans la liste � l'indice
;;
;; Arguments
;; ele : l'�l�ment � ins�rer
;; ind : l'index auquel ins�rer l'�l�ment
;; lst : la liste
(defun gc:insertAt (ele ind lst)
  (cond
    ((null lst) (list ele))
    ((zerop ind) (cons ele lst))
    ((cons (car lst) (gc:insertAt ele (1- ind) (cdr lst))))
  )
)

;; gc:insertRange
;; Ins�re les �l�ments dans la liste � partir l'indice
;;
;; Arguments
;; new : la liste d'�l�ments � ins�rer
;; ind : l'index auquel ins�rer l'�l�ment
;; lst : la liste
(defun gc:insertRange (new ind lst)
  (cond
    ((null lst) new)
    ((zerop ind) (append new lst))
    ((cons (car lst) (gc:insertRange new (1- ind) (cdr lst))))
  )
)

;; gc:removeAt
;; Retourne la liste priv�e de l'�l�ment � l'indice sp�cifi�
;;
;; Arguments
;; ind : l'index de l'�l�ment � supprimer
;; lst : la liste
(defun gc:removeAt (ind lst)
  (if (or (zerop ind) (null lst))
    (cdr lst)
    (cons (car lst) (gc:removeAt (1- ind) (cdr lst)))
  )
)

;; gc:removeRange
;; Supprime le nombre d'�l�ments de la liste � partir de l'indice
;;
;; Arguments
;; ind : l'index � partir duquel supprimer les �l�ments
;; cnt : le nombre d'�l�ments � supprimer
;; lst : la liste
(defun gc:removeRange (from cnt lst)
  (cond
    ((or (null lst) (zerop cnt)) lst)
    ((< 0 from)
     (cons (car lst) (gc:removeRange (1- from) cnt (cdr lst)))
    )
    ((gc:removeRange from (1- cnt) (cdr lst)))
  )
)

;; gc:substAt
;; Remplace l'�l�ment � l'indice dans la liste
;;
;; Arguments
;; ele : l'�l�ment � substituer
;; ind : l'index auquel substituer l'�l�ment
;; lst : la liste
(defun gc:SubstAt (ele ind lst)
  (cond
    ((null lst) nil)
    ((zerop ind) (cons ele (cdr lst)))
    ((cons (car lst) (gc:substAt ele (1- ind) (cdr lst))))
  )
)

;; gc:substRange
;; Remplace les �l�ments de la liste � partir de l'indice
;;
;; Arguments
;; new : la liste d'�l�ments � substituer
;; ind : l'index � partir duquel substituer les �l�ments
;; lst : la liste
(defun gc:substRange (new ind lst)
  (cond
    ((or (null lst) (null new)) lst)
    ((zerop ind)
     (cons (car new) (gc:substRange (cdr new) ind (cdr lst)))
    )
    ((cons (car lst) (gc:substRange new (1- ind) (cdr lst))))
  )
)

;; gc:trunc
;; Retourne la liste tronqu�e � partir de la premi�re occurrence
;; de l'expression (liste compl�mentaire de celle retourn�e par MEMBER)
;;
;; Arguments
;; expr : l'expression recherch�e
;; lst : la liste
(defun gc:trunc	(expr lst)
  (if (and lst
	   (not (equal (car lst) expr))
      )
    (cons (car lst) (gc:trunc expr (cdr lst)))
  )
)

;; gc:truncIf
;; Retourne la liste tronqu�e � partir de la premi�re occurrence qui
;; retourne T � la fonction (compl�mentaire de celle retourn�e par VL-MEMBER-IF)
;;
;; Arguments
;; fun : la fonction pr�dicat
;; lst : la liste
(defun gc:truncIf (fun lst)
  (if (and lst
	   (not ((eval fun) (car lst)))
      )
    (cons (car lst) (gc:Trunctf fun (cdr lst)))
  )
)

;; gc:truncFuzz
;; Comme gc:Trunc avec une tol�rance dans la comparaison
;;
;; Arguments
;; expr : l'expression recherch�e
;; lst : la liste
;; fuzz : la tol�rance
(defun gc:truncFuzz (expr lst fuzz)
  (if (and lst
	   (not (equal (car lst) expr))
      )
    (cons (car lst) (gc:truncFuzz expr (cdr lst) fuzz))
  )
)

;;; gc:memberFuzz
;; Comme member avec une tol�rance dans la comparaison
;;
;; Arguments
;; expr : l'expression recherch�e
;; lst : la liste
;; fuzz : la tol�rance
(defun gc:memberFuzz (expr lst fuzz)
  (while (and lst (not (equal (car lst) expr fuzz)))
    (setq lst (cdr lst))
  )
  lst
)

;; gc:str2lst
;; Transforme un chaine avec s�parateur en liste de chaines
;;
;; Arguments
;; str : la cha�ne
;; sep : le s�parateur
(defun gc:str2lst (str sep / pos)
  (if (setq pos (vl-string-search sep str))
    (cons (substr str 1 pos)
	  (gc:str2lst (substr str (+ (strlen sep) pos 1)) sep)
    )
    (list str)
  )
)

;; gc:lst2str
;; Concat�ne une liste de cha�nes et un s�parateur en une chaine
;;
;; Arguments
;; lst : la liste
;; sep : le s�parateur
(defun gc:lst2str (lst sep)
  (apply 'strcat
	 (cons (car lst)
	       (mapcar (function (lambda (x) (strcat sep x))) (cdr lst))
	 )
  )
)

;; gc:fold
;; Retourne l'�tat final d'un accumulateur dont l'�tat initial est modifi�
;; par l'application d'une fonction � chacun des �l�ments d'une liste
;;
;; Arguments
;; fun : la fonction � appliquer � chaque �l�ment
;; acc : l'accumulateur
;; lst : la liste
(defun gc:fold (fun acc lst)
  (setq fun (eval fun))
  (foreach n lst (setq acc (fun acc n)))
)

;; gc:unfold
;; G�n�re une liste � partir d'une fonction de calcul qui prend un �tat
;; et le modifie pour produire chaque �l�ment suivant de la s�quence
;;
;; Arguments
;; fun : la fonction de calculer � chaque �l�ment
;; state : l'�tat initial
(defun gc:unfold (fun state)
  ((lambda (pair)
     (if pair
       (cons (car pair) (gc:unfold fun (cdr pair)))
     )
   )
    ((eval fun) state)
  )
)

;; gc:groupBy
;; Regroupe les �l�ments d'une liste selon la cl� g�n�r�e par la fonction sp�cifi�e.
;; Retourne un liste de sous listes dont le premier �l�ment est la cl�.
;;
;; Arguments
;; fun : la fonction g�n�rayrice de cl�
;; lst : la liste
(defun gc:groupBy (fun lst / key sub res)
  (setq fun (eval fun))
  (mapcar
    '(lambda (l) (cons (car l) (reverse (cdr l))))
    (foreach l lst
      (setq res
	     (if (setq sub (assoc (setq key (fun l)) res))
	       (subst (vl-list* key l (cdr sub)) sub res)
	       (cons (list key l) res)
	     )
      )
    )
  )
)
