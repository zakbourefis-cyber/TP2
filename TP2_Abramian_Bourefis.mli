(* La décision finale pour un document *)
type tdecision = Oui | Non

(* type document *)
type tdoc = Cdoc of string * tdoc | Cdoc_vide

(* document d'apprentissage avec sa décision *)
type tdoc_apprentissage = Cdoc_apprentissage of tdoc * tdecision

(* liste de documents d'apprentissage *)
type tens_doc = Cens_doc of tdoc_apprentissage * tens_doc | Cens_doc_vide

(* l'arbre de décision *)
type tarbre = Feuille of tdecision | Noeud of string * tarbre * tarbre

(* crée un document vide *)
val cree_doc_vide : unit -> tdoc

(* ajoute un mot en tête du document *)
val add_mot : string -> tdoc -> tdoc

(* vérifie si un document est vide *)
val est_vide_doc : tdoc -> bool

(* renvoie le premier mot du document *)
val get_prem_mot : tdoc -> string

(* renvoie le document privé de son premier mot *)
val get_reste_doc : tdoc -> tdoc

(* description : associe un document à une décision *)
val cree_doc_apprentissage : tdoc -> tdecision -> tdoc_apprentissage

(* prend le document *)
val s_doc : tdoc_apprentissage -> tdoc

(* prend la décision associée au document *)
val s_decision : tdoc_apprentissage -> tdecision

(* crée un ensemble vide *)
val cree_ens_vide : unit -> tens_doc

(* ajoute un document d'apprentissage à la liste *)
val add_doc_ens : tdoc_apprentissage -> tens_doc -> tens_doc

(* vérifie si la liste est vide *)
val est_vide_ens : tens_doc -> bool

(* renvoie le premier document d'apprentissage de la liste *)
val get_prem_ens : tens_doc -> tdoc_apprentissage

(* renvoie la liste privée de son premier document d'apprentissage *)
val get_reste_ens : tens_doc -> tens_doc

(*vérifie si un arbre est une feuille *)
val est_feuille : tarbre -> bool

(* prend la décision contenue dans une feuille *)
val s_decision_feuille : tarbre -> tdecision

(* prend le mot contenu dans un noeud *)
val s_mot_noeud : tarbre -> string

(*renvoie le sous-arbre gauche *)
val get_branche_gauche : tarbre -> tarbre

(*renvoie le sous-arbre droit *)
val get_branche_droite : tarbre -> tarbre

(* on parcourt le document pour vérifier si un mot donné y est présent *)
val appartient_doc : string -> tdoc -> bool


val classer_doc : tarbre -> tdoc -> tdecision

val separateur_test : string

val doc1 : tdoc

val doc_appr1 : tdoc_apprentissage

val test_prem_mot : string

val test_decision_doc : tdecision

val arbre_test : tarbre

val test_est_feuille : bool

val test_mot_racine : string

val sous_arbre_gauche : tarbre

val test_decision_gauche : tdecision

val decision_test : tdecision

