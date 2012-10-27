open Syntax
open Drsxp


type position =
        | Sujet
        | COD
        | COI
        | In
        | For;;


let op2str = function
  | Eq  ->  " " 
  | Geq  ->  "at least " 
  |Greater  ->  "more than " 
  | Leq  ->  "at more " 
  | Less  ->  "less than " 
  | Exactly  ->  "exactly " 
  | NaOp ->  " ";;

let num2str = function
  | NotCountable -> " "
  | Number 1 -> "a "
  | Number i -> (string_of_int i)^" ";;

let verb2str verb sing = 
        match verb, sing with
        | "be", Plural     -> "are "
        | "be", Singular   -> "is "
        | "be", Partitive  -> "are "
        |  a, Singular     -> a^"s "
        |  a, Plural       ->  a^" " ;;


(*
let rec checkIfPredicateSingularOrPlural drs = function
  | PredicateTransitive  (  ref ,  verb,   subject, cod, gramnbr)       -> checkIfPredicateSingularOrPlural drs (getItemByVarIntoDRS subject drs)
  | PredicateDiTransitive(  ref ,  verb,   subject, cod, coi, gramnbr)  -> checkIfPredicateSingularOrPlural drs (getItemByVarIntoDRS subject drs)
  | PredicateIntransitive(  ref ,  verb,   subject, gramnbr   )         -> checkIfPredicateSingularOrPlural drs (getItemByVarIntoDRS subject drs)
  | Object(  ref,  name,  countable,  unittype,  op, count,x,y)         ->  (match count with
      | NotCountable -> Singular
      | Number 1 ->  Singular 
      | _ -> Plural)
  | _ -> Singular ;;

(*#trace getItemByVarIntoDRS;;
#trace getItemsByVarIntoDRS;;*)

let rec paraphraser = function
  | FullDRS( varlist, Operator2(Imply, drs1 ,drs2)::drsqueue ) as drs -> 
      "if "^((paraphraser drs1)^"then "^(paraphraser drs2)^".\n"^(String.concat " " (List.map (paraphraser_atom drs false) drsqueue)))

  | FullDRS( varlist, Operator1(Command, drs1)::drsqueue ) as drs ->
     (paraphraser drs1)^".\n"^(String.concat " " (List.map (paraphraser_atom drs false) drsqueue))

  | FullDRS (varlist , lst )  as drs -> (*TODO chercher le predicate et
                                            travailler à partir de lui*)
      (String.concat " " (List.map (paraphraser_atom drs false) lst))
                     

and  paraphraser_atom drs complet = function
  | Object( Var a, Nom name,  countable,  unittype,  op, count,x,y)           ->  if complet = true then "There is "^(op2str op)^(num2str count)^name^" "^a
    else (op2str op)^(num2str count)^name^" "


  | PredicateTransitive(Var ref, Verbe verbe, Var  subject , cod, gramnbr) as verb -> 
      (paraphraseForVar drs (Var subject))^" "^(verb2str (verbe,(checkIfPredicateSingularOrPlural drs verb)))^(paraphraseForVar drs cod )^" "

  | PredicateIntransitive(Var ref, Verbe verbe, Var  subject , gramnbr) as verb -> 
      (paraphraseForVar drs (Var subject))^" "^(verb2str (verbe,(checkIfPredicateSingularOrPlural drs verb)))^" "

  | PredicateDiTransitive(Var ref, Verbe verbe, Var  subject , cod, coi, gramnbr) as verb-> 
      (paraphraseForVar drs (Var subject))^" "^(verb2str (verbe,(checkIfPredicateSingularOrPlural drs verb)))^(paraphraseForVar drs cod )^" to"^(paraphraseForVar drs coi )

  | PredicateDiTransitive(Var ref, Verbe verbe, subject , cod, coi, gramnbr) as verb-> 
      (stringOfVar drs subject)^" "^(verb2str (verbe,(checkIfPredicateSingularOrPlural drs verb)))^(paraphraseForVar drs cod )^" to"^(paraphraseForVar drs coi )

  | PredicateTransitive(Var ref, Verbe verbe,   subject , cod, gramnbr) as verb -> 
      (stringOfVar drs subject)^" "^(verb2str (verbe,(checkIfPredicateSingularOrPlural drs verb)))^(paraphraseForVar drs cod )^" "

   | PredicateIntransitive(Var ref, Verbe verbe,   subject , gramnbr) as verb -> 
      (stringOfVar drs subject)^" "^(verb2str (verbe,(checkIfPredicateSingularOrPlural drs verb)))^" "
 (*TODO généraliser la gestion du sujet COD ou var, parce que sinon on va avoir 30 000 cas*)
  |  Named a -> a
  |  String a -> a    
  |  Rien -> ""
  |  Modifier_pp(v, Preposition w , v2) -> w
  |  Modifier_Adv(v, Adv w , v2) -> w
(*  | Operator2( Imply, d1 ,d2) *)

and paraphraseForVar drs v = (paraphraser_atom drs false (getItemByVarIntoDRS v drs))
and stringOfVar drs = function
  | Var a -> a
  | ConstStr a -> a
  | Num a -> string_of_int a
  | SubAtom a -> paraphraser_atom  drs false a;; 
 
(*
  | Object(Var var, Nom(name), getClassType countable, getUnitType unittype, getOp op, getCount count,x,y)
  | Object(Var var, Nom(name), getClassType countable, getUnitType unittype, getOp op, Number count,x,y)
  | Object(Var var, Nom(name), getClassType countable, getUnitType unittype, getOp op, getCount count,x,y)
(*TODO : Vérifier ce cas, le coup de forcer le sujet sur le name, c'est peut être pas général...*)
   |  PredicateDiTransitive(atomNamedString2NamedString  ref ,Verbe (extractString verb), atomNamedString2NamedString  subject , atomNamedString2NamedString cod, atomNamedString2NamedString coi)
  | PredicateTransitive(atomNamedString2NamedString  ref ,Verbe (extractString verb), atomNamedString2NamedString  subject , atomNamedString2NamedString cod)
  |  PredicateIntransitive(atomNamedString2NamedString  ref ,Verbe (extractString verb), atomNamedString2NamedString  subject)
  | Property1Ary (atomNamedString2NamedString ref, Adj (extractString adjective), getDegree (extractString degree))
  | Property2Ary (atomNamedString2NamedString ref, Adj (extractString adjective), getDegree (extractString degree),atomNamedString2NamedString ref2)
 | Property3Ary (atomNamedString2NamedString ref, Adj (extractString adjective), atomNamedString2NamedString ref2, getDegree (extractString degree), getCompTarget (extractString comptarget), atomNamedString2NamedString ref3)
  | Relation(atomNamedString2NamedString ref1, atomNamedString2NamedString ref2)
  | Modifier_Adv( atomNamedString2NamedString ref, Adv (extractString adverb), getDegree (extractString degree))
  | Modifier_pp (atomNamedString2NamedString ref1, Preposition (extractString preposition), atomNamedString2NamedString ref2)
  | HasPart(atomNamedString2NamedString groupref,atomNamedString2NamedString memberref)
  | Query(atomNamedString2NamedString ref, getQuestionWord (extractString questionWord))
  | print_endline ("Error with atom :"^(atom2String (Atom(a,l,x,y)))^". Check you're using ACE 6.6"); Rien
 *)
let l = parsecomplete "drs([A, B, C, D], [object(A, guy, countable, na, eq, 2)-1/2, object(B, balloon, countable, na, eq, 2)-1/5, object(C, girl, countable, na, eq, 2)-1/8, predicate(D, give, A, B, C)-1/3]).";;
paraphraser_atom l  false (PredicateDiTransitive (Var "D", Verbe "give", Var "A", Var "B", Var "C",Singular));; 
(* Stratégie : 
        * On regarde ce qu'on a dans la liste : 
        *   juste object et predicate --> Phrase simple
        *   object,predicate, modifier_pp -> Phrase avec modifiers_pp
        *   If then -> Traitement Drs1, traitement Drs2
        * on regarde le verbe on voit s'il est n-transitif, n E {0,1,2}. On
 * transforme le sujet en There is ? Xn. ? X1  ?Verbe ?COD ?COI
 *
 * Voir si on peut pas reconstruire l'arbre si besoin...*)



*)

let objet_en_tant_que_sujet obj =
        match obj with
        | Object(  ref, Nom name,  countable,  unittype,  op, Number count,x,y)  ->
                        (match countable, unittype, op with
                        | Countable, Na        , o    ->  ( match o with 
                        | Eq  -> let txt = (string_of_int count)^" "^name in if count < 2 then txt^" " else txt^"s "
                        | Geq -> "At least "^(string_of_int count)^" "^name^" "
                        | Greater -> "More than "^(string_of_int count)^" "^name^" "
                        | Leq     -> "Less or equal than "^(string_of_int count)^" "^name^" "
                        | Less    -> "Less than "^(string_of_int count)^" "^name^" "
                        | Exactly -> "Exactly "^(string_of_int count)^" "^name^" "
                        | NaOp    -> " "^name^" "
                        )
                        | Dom      , Na       , NaOp  -> name^" "
                        | Mass     , Kg       , o     -> ( match o with 
                        | Eq  -> (string_of_int count)^" kg "
                        | Geq -> "At least "^(string_of_int count)^" kg "
                        | Greater -> "More than "^(string_of_int count)^" kg "
                        | Leq     -> "Less or equal than "^(string_of_int count)^" kg "
                        | Less    -> "Less than "^(string_of_int count)^" kg "
                        | Exactly -> "Exactly "^(string_of_int count)^" kg "
                        | NaOp    -> failwith "illogique Na avec une masse"
                        )
                        | _ , _ , _ -> failwith "Cas objet non traité"
                        )
         | Named nom  -> nom^" "
         | String nom -> nom^" "

;;




let paraphrase_from_verb verbe = 
        match verbe with
        | PredicateIntransitive ( ref , Verbe verb, SubAtom subject, gramnbr    )               ->  (objet_en_tant_que_sujet subject)^(verb2str verb gramnbr)

        | PredicateTransitive   ( ref , Verbe verb, SubAtom subject, SubAtom cod, gramnbr )      ->  (objet_en_tant_que_sujet subject)^(verb2str verb gramnbr)^(objet_en_tant_que_sujet cod)


        | PredicateDiTransitive ( ref , Verbe verb, SubAtom subject, SubAtom cod, SubAtom coi, gramnbr )  ->  
                        (objet_en_tant_que_sujet subject)^(verb2str verb gramnbr)^(objet_en_tant_que_sujet cod)^"to "^(objet_en_tant_que_sujet coi)
        | _ -> failwith "cas paraphrase non traité";;

let paraphrase_from_list lst = 
        let verbes = List.filter is_Predicate  lst in
        List.map paraphrase_from_verb verbes;;

let aiguillage_phrase l = match  exists_Predicate l, exists_Modifier_pp l, exists_Relation l with
|  true, false,false   -> "Phrase simple"
|  true, true,false    -> "Phrase avec modifiers_pp"
|  false, false,false  -> "Phrase nominale"
|  true, false,false   -> "Phrase intransitive (genre 'Il pleut')"
|  true, true, true    -> "Phrase simple avec modifiers_pp et relation (of)"
|  false, _ ,_         -> "Cas non géré pour le moment, ou illogique";;



let g = FullDRS ([Var "J1"; Var "K1"; Var "L1"],                                                                                              
[Object (Var "J1", Nom "time", Countable, Na, Greater, Number 2, 13, 8);                                                             
Object (Var "K1", Nom "day", Countable, Na, Eq, Number 10, 13, 13);
PredicateTransitive (Var "L1", Verbe "vote", SubAtom (Named "User1"),
Var "J1", Singular);
Modifier_pp (Var "L1", Preposition "in", Var "K1");
Modifier_pp (Var "L1", Preposition "for", SubAtom (Named "User2"))]);;
