(*TP2 Programmation Fonctionnelle SAE S4*)
(*Kévin ABRAMIAN & Zakaria BOUREFIS*)

(*A taper avant chaque exec sur MAC pour opam*)
(*eval $(opam env)*)

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

let (cree_feuille : tdecision -> tarbre) = 
  function dec -> 
    Feuille dec;;
    
(* description : crée un noeud contenant un mot et deux sous-arbres (gauche et droit) *)
let (cree_noeud : string -> tarbre -> tarbre -> tarbre) = 
  function mot -> 
    function gauche -> 
      function droite -> 
        Noeud(mot, gauche, droite);;

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
          
(* on parcourt l'arbre de décision pour classer un document *)
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

(* =============================================================== *)
(* Décomposition pour transformer un ensemble de doc en une phrase *)
(* =============================================================== *)

(* ajoute un mot au document seulement s'il n'y est pas déjà *)
let (add_mot_unique : string -> tdoc -> tdoc) =
  function mot ->
      function doc ->
        if appartient_doc mot doc then
          doc
        else
          add_mot mot doc;;

(* verse tous les mots uniques de doc_source dans doc_dest *)
let rec (fusion_mots_doc : tdoc -> tdoc -> tdoc) =
  function doc_source ->
    function doc_dest ->
      if est_vide_doc doc_source then
        doc_dest
      else
        let mot = get_prem_mot doc_source in
        let reste = get_reste_doc doc_source in
        add_mot_unique mot (fusion_mots_doc reste doc_dest);;

(* extrait la liste de tous les mots uniques présents dans un ensemble de documents *) 
let rec (extraire_vocabulaire : tens_doc -> tdoc) =
  function ens ->
    if est_vide_ens ens then
      cree_doc_vide ()
    else
      let doc_courant = s_doc (get_prem_ens ens) in
      let reste_ens = get_reste_ens ens in
      fusion_mots_doc doc_courant (extraire_vocabulaire reste_ens);;

(* =============================================================== *)
(* Décomposition pour evaluer_arbre (Renvoie le taux de prédiction)*)
(* =============================================================== *)

(* compte le nombre total de documents dans un ensemble *)
let rec (compter_docs : tens_doc -> int) = 
  function ens ->
    if est_vide_ens ens then
      0
    else
      1 + compter_docs (get_reste_ens ens);;

(* compte les réussites *)
let rec (compter_reussites : tarbre -> tens_doc -> int) =
  function arbre ->
    function ens_test ->
      if est_vide_ens ens_test then
        0
      else
        (if classer_doc arbre (s_doc (get_prem_ens ens_test)) = s_decision (get_prem_ens ens_test) then 
           1 
         else 
           0)
        + compter_reussites arbre (get_reste_ens ens_test);;

(* renvoie le taux de prédictions correctes en pourcentage *)
let (evaluer_arbre : tarbre -> tens_doc -> float) =
  function arbre ->
    function ens_test ->
      let total = compter_docs ens_test in
      if total = 0 then
        0.0 (* On n'oublie pas pour ne pas avoir le souci de la division par 0 *)
      else
        let reussites = compter_reussites arbre ens_test in
        (* Conversion des entiers en float juste pour la division finale *)
        (float_of_int reussites /. float_of_int total) *. 100.0;;

(* =================================== *)
(* UTILISATION D'IA POUR LES TESTS CSV *)
(* =================================== *)
let rec (separer_mots_decision : string list -> tdoc * tdecision) =
  function 
    | [] -> failwith "Erreur : ligne vide"
    | [dec_str] -> 
        (* Cas de base : on est sur le dernier élément, c'est la décision *)
        let dec = if dec_str = "+" then Oui else Non in
        (cree_doc_vide (), dec)
    | mot :: reste -> 
        (* Cas récursif : on extrait le reste, et on ajoute le mot actuel au doc *)
        let (doc, dec) = separer_mots_decision reste in
        (add_mot mot doc, dec);;

let (ligne_vers_doc : string -> tdoc_apprentissage) =
  function ligne ->
    (* On découpe la ligne en liste de chaînes à chaque virgule *)
    let mots = String.split_on_char ',' ligne in
    let (doc, dec) = separer_mots_decision mots in
    cree_doc_apprentissage doc dec;;

let rec (lire_lignes : in_channel -> tens_doc) =
  function canal ->
    try
      let ligne = input_line canal in
      let doc_appr = ligne_vers_doc ligne in
      (* On ajoute le document à l'ensemble et on relance sur la ligne suivante *)
      add_doc_ens doc_appr (lire_lignes canal)
    with End_of_file ->
      (* Fin du fichier : on ferme le canal et on renvoie l'ensemble vide terminal *)
      close_in canal;
      cree_ens_vide ();;

let (charger_csv : string -> tens_doc) =
  function nom_fichier ->
    let canal = open_in nom_fichier in
    lire_lignes canal;;

(* ======================== *)
(* Construction arbre (5.1) *)
(* ======================== *)

(* vérifie si tous les documents d'un ensemble ont la même décision *)
let rec (tous_meme_decision : tdecision -> tens_doc -> bool) =
  function dec_ref ->
    function ens ->
      if est_vide_ens ens then 
        true
      else if s_decision (get_prem_ens ens) = dec_ref then
        tous_meme_decision dec_ref (get_reste_ens ens)
      else 
        false;;

(* vérifie si l'ensemble est parfaitement homogène *)
let (est_homogene : tens_doc -> bool) =
  function ens ->
    if est_vide_ens ens then 
      true
    else 
      tous_meme_decision (s_decision (get_prem_ens ens)) ens;;

(* compte le nombre de oui *)
let rec (compter_oui : tens_doc -> int) =
  function ens ->
    if est_vide_ens ens then 
      0
    else 
      (if s_decision (get_prem_ens ens) = Oui then 1 else 0) + compter_oui (get_reste_ens ens);;

(* compte le nombre de non *)
let rec (compter_non : tens_doc -> int) =
  function ens ->
    if est_vide_ens ens then 
      0
    else 
      (if s_decision (get_prem_ens ens) = Non then 1 else 0) + compter_non (get_reste_ens ens);;

(* renvoie la décision majoritaire d'un ensemble *)
let (decision_majoritaire : tens_doc -> tdecision) =
  function ens ->
    if compter_oui ens >= compter_non ens then 
      Oui 
    else 
      Non;;

(* filtre les documents contenant le mot cible (on met ca a gauche) *)
let rec (filtre_oui : string -> tens_doc -> tens_doc) =
  function mot ->
    function ens ->
      if est_vide_ens ens then 
        cree_ens_vide ()
      else
        let doc_appr = get_prem_ens ens in
        let reste = filtre_oui mot (get_reste_ens ens) in
        if appartient_doc mot (s_doc doc_appr) then 
          add_doc_ens doc_appr reste 
        else 
          reste;;

(* filtre les documents sans le mot cible (on met ca a droite)*)
let rec (filtre_non : string -> tens_doc -> tens_doc) =
  function mot ->
    function ens ->
      if est_vide_ens ens then 
        cree_ens_vide ()
      else
        let doc_appr = get_prem_ens ens in
        let reste = filtre_non mot (get_reste_ens ens) in
        if appartient_doc mot (s_doc doc_appr) then 
          reste 
        else 
          add_doc_ens doc_appr reste;;

(* construit un arbre de décision *)
let rec (construire_arbre_naif : tdoc -> tens_doc -> tarbre) =
  function liste_mots ->
    function ens_appr ->
      if est_homogene ens_appr then
        cree_feuille (s_decision (get_prem_ens ens_appr))
      else if est_vide_doc liste_mots then
        cree_feuille (decision_majoritaire ens_appr)
      else
        let mot_cible = get_prem_mot liste_mots in
        let reste_mots = get_reste_doc liste_mots in
        
        let ens_oui = filtre_oui mot_cible ens_appr in
        let ens_non = filtre_non mot_cible ens_appr in
        
        cree_noeud mot_cible 
          (if est_vide_ens ens_oui then cree_feuille (decision_majoritaire ens_appr) 
           else construire_arbre_naif reste_mots ens_oui)
          (if est_vide_ens ens_non then cree_feuille (decision_majoritaire ens_appr) 
           else construire_arbre_naif reste_mots ens_non);;
           
(* ===== *)
(* Tests *)
(* ===== *)
let ens_apprentissage_complet = charger_csv "apprentissage.csv";;
let ens_evaluation_complet = charger_csv "evaluation.csv";;

let separateur_test = "====================================================================="

(* docs*)
let doc1 = add_mot "tournoi" (add_mot "Irlande" (add_mot "victoire" (cree_doc_vide ())));;
let doc2 = add_mot "plonger" (add_mot "medaille" (add_mot "gagner" (cree_doc_vide ())));;
let doc3 = add_mot "sieste" (add_mot "canapé" (cree_doc_vide ()));;

let doc_appr1 = cree_doc_apprentissage doc1 Oui;;

let test_prem_mot = get_prem_mot doc1;;
let test_decision_doc = s_decision doc_appr1;; 

let separateur_test = "=================================== ";;

let phrase = extraire_vocabulaire ens_apprentissage_complet;;
let arbre = construire_arbre_naif phrase ens_apprentissage_complet;;

let taux = evaluer_arbre arbre ens_evaluation_complet;;