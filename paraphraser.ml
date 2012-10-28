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
