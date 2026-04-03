(*TP2 Programmation Fonctionnelle SAE S4*)
(*Kévin ABRAMIAN & Zakaria BOUREFIS*)

(* ==================== *)
(* Définition des types *)
(* ==================== *)

(* La décision finale pour un document *)
type tdecision = Oui | Non;;

(* type document *)
type tdoc = Cdoc of string * tdoc | Cdoc_vide;;

(* document d'apprentissage avec sa décision *)
type tdoc_apprentissage = Cdoc_apprentissage of tdoc * tdecision;;

(* liste de documents d'apprentissage *)
type tens_doc = Cens_doc of tdoc_apprentissage * tens_doc | Cens_doc_vide;;

(* L'arbre de décision : 
   - Un Noeud contient un mot et deux sous-arbres (gauche = oui, droite = non)
   - Une Feuille a une décision
    *)
type tarbre = 
  | Feuille of tdecision 
  | Noeud of string * tarbre * tarbre;;

(* On laisse ca de coté on sait jamais on a besoin de représenter par +/-
let (affiche_decision : tdecision -> string) = function
  | Oui -> "+"
  | Non -> "-";;
*)

(* =========================== *)
(* Constructeurs et sélecteurs *)
(* =========================== *)

(* documents *)
(* ========= *)

(* crée un document vide *)
let (cree_doc_vide : unit -> tdoc) = 
  function () -> 
    Cdoc_vide;;

(* ajoute un mot en tête du document *)
let (add_mot : string -> tdoc -> tdoc) = 
  function mot -> 
    function doc -> 
      Cdoc(mot, doc);;

(* vérifie si un document est vide *)
let (est_vide_doc : tdoc -> bool) = 
  function doc -> 
    doc = cree_doc_vide ();;

(* renvoie le premier mot du document *)
let (get_prem_mot : tdoc -> string) = 
  function Cdoc(m, _) -> m 
         | _ -> failwith "document vide !";;

(* renvoie le document privé de son premier mot *)
let (get_reste_doc : tdoc -> tdoc) = 
  function Cdoc(_, reste) -> reste 
         | _ -> failwith "document vide !";;


(* documents d'apprentissage *)
(* ========================= *)

(* description : associe un document à une décision *)
let (cree_doc_apprentissage : tdoc -> tdecision -> tdoc_apprentissage) = 
  function doc -> 
    function dec -> 
      Cdoc_apprentissage(doc, dec);;

(* prend le document *)
let (s_doc : tdoc_apprentissage -> tdoc) = 
  function Cdoc_apprentissage(doc, _) -> doc;;

(* prend la décision associée au document *)
let (s_decision : tdoc_apprentissage -> tdecision) = 
  function Cdoc_apprentissage(_, dec) -> dec;;


(* liste de documents d'apprentissage *)
(* ================================== *)

(* crée un ensemble vide *)
let (cree_ens_vide : unit -> tens_doc) = 
  function () -> Cens_doc_vide;;

(* ajoute un document d'apprentissage à la liste *)
let (add_doc_ens : tdoc_apprentissage -> tens_doc -> tens_doc) = 
  function doc_a -> 
    function ens -> 
      Cens_doc(doc_a, ens);;

(* vérifie si la liste est vide *)
let (est_vide_ens : tens_doc -> bool) = 
  function ens -> 
    ens = cree_ens_vide ();;

(* renvoie le premier document d'apprentissage de la liste *)
let (get_prem_ens : tens_doc -> tdoc_apprentissage) = 
  function Cens_doc(doc_a, _) -> doc_a 
         | _ -> failwith "ensemble vide !";;

(* renvoie la liste privé de son premier élément *)
let (get_reste_ens : tens_doc -> tens_doc) = 
  function Cens_doc(_, reste) -> reste 
         | _ -> failwith "ensemble vide !";;

(* l'arbre de décisions *)
(* ==================== *)

(* vérifie si l'arbre est une feuille *)
let (est_feuille : tarbre -> bool) = 
  function Feuille _ -> true
         | Noeud _   -> false;;

(* prend la décision contenue dans une feuille *)
let (s_decision_feuille : tarbre -> tdecision) = 
  function Feuille dec -> dec
         | Noeud _     -> failwith "Erreur !";;

(* prend le mot testé dans un noeud *)
let (s_mot_noeud : tarbre -> string) = 
  function Noeud(mot, _, _) -> mot
         | Feuille _        -> failwith "Erreur !";;

(* renvoie le sous-arbre gauche *)
let (get_branche_gauche : tarbre -> tarbre) = 
  function Noeud(_, gauche, _) -> gauche
         | Feuille _           -> failwith "Erreur !";;

(* renvoie le sous-arbre droit *)
let (get_branche_droite : tarbre -> tarbre) = 
  function Noeud(_, _, droite) -> droite
         | Feuille _           -> failwith "Erreur !";;

(* ================ *)
(* Fonctions utiles *)
(* ================ *)

(* on parcourt le document pour vérifier si un mot donné y est présent *)
let rec (appartient_doc : string -> tdoc -> bool) = 
  function mot_cible -> 
    function doc ->
      if est_vide_doc doc then
        false
      else
        let prem = get_prem_mot doc in
        if prem = mot_cible then
          true
        else
          let reste = get_reste_doc doc in
          appartient_doc mot_cible reste;;

let rec (classer_doc : tarbre -> tdoc -> tdecision) =
  function arbre -> 
    function doc ->
      if est_feuille arbre then
        s_decision_feuille arbre 
      else
        let mot_cible = s_mot_noeud arbre in
        if appartient_doc mot_cible doc then
          classer_doc (get_branche_gauche arbre) doc
        else
          classer_doc (get_branche_droite arbre) doc;;

(* ===== *)
(* Tests *)
(* ===== *)
let separateur_test = "====================================================================="
(* docs*)
let doc1 = add_mot "tournoi" (add_mot "Irlande" (add_mot "victoire" (cree_doc_vide ())));;

let doc_appr1 = cree_doc_apprentissage doc1 Oui;;

let test_prem_mot = get_prem_mot doc1;;
let test_decision_doc = s_decision doc_appr1;; 

(* arbres *)
let arbre_test = Noeud("tournoi", Feuille Oui, Feuille Non);;

let test_est_feuille = est_feuille arbre_test;;
let test_mot_racine = s_mot_noeud arbre_test;; 

let sous_arbre_gauche = get_branche_gauche arbre_test;;
let test_decision_gauche = s_decision_feuille sous_arbre_gauche;; 
let decision_test = classer_doc arbre_test doc1;; 