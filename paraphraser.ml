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



let prop2str (Preposition prep) = prep;;

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
         | Object(  ref, Nom name,  countable,  unittype,  op,  notCountable,x,y)  -> name^" "
         | Named nom  -> nom^" "
         | String nom -> nom^" "
         | Property1Ary(ref, Adj happy , Pos) -> happy^" "
         | _ -> failwith "objet_en_tant_que_sujet : Element non objet"

;;


let rec paraphrase_modifiers  modif =
        match modif with
        | Modifier_pp(ref1,  preposition, SubAtom ref2)  -> (prop2str preposition)^" "^(objet_en_tant_que_sujet ref2)
        | _  -> failwith "paraphrase_modifiers : Element non Modifier_pp"


and paraphrase_from_verb verbe = 
        match verbe with
        | PredicateIntransitive ( ref , Verbe verb, SubAtom subject, gramnbr    )               ->  (objet_en_tant_que_sujet subject)^(verb2str verb gramnbr)

        | PredicateTransitive   ( ref , Verbe verb, SubAtom subject, SubAtom cod, gramnbr )      ->  (objet_en_tant_que_sujet subject)^(verb2str verb gramnbr)^(objet_en_tant_que_sujet cod)


        | PredicateDiTransitive ( ref , Verbe verb, SubAtom subject, SubAtom cod, SubAtom coi, gramnbr )  ->  
                        (objet_en_tant_que_sujet subject)^(verb2str verb gramnbr)^(objet_en_tant_que_sujet cod)^"to "^(objet_en_tant_que_sujet coi)
        | _ -> failwith "paraphrase_from_verb : cas paraphrase non traité"

and paraphrase_from_list lst = 
        let verbes = List.filter is_Predicate  lst in
        List.map paraphrase_from_verb verbes

and paraphrase_if_then cond1 cond2 =
        "if "^(String.concat " " (aiguillage_phrase cond1 []))^" then "^(String.concat " " (aiguillage_phrase cond2 []))
        (*let verbe1 = List.find is_Predicate cond1 in 
        let verbe2 = List.find is_Predicate cond2 in 
        let s1 = paraphrase_from_verb verbe1 in
        let s2 = paraphrase_from_verb verbe2 in
        "if "^s1^" then "^s2*)


and aiguillage_phrase drs lst  = 
        let _,l = Syntax.getDRSCondition drs in
        match  exists_Operator l, exists_Predicate l, exists_Modifier_pp l, exists_Relation l with
        |  false, true, false,false   -> let verbes = List.filter is_Predicate  l in (List.map paraphrase_from_verb verbes)@lst
        |  false, true, true,false    -> let verbes = List.filter is_Predicate  l in
                          let genere verbeUniq variableVerbe = 
                                  let phrasebase = paraphrase_from_verb verbeUniq in
                                  let modifiers  = List.filter is_Modifier_pp (findItemsInList l variableVerbe) in
                                  phrasebase^" "^(String.concat " " (List.map paraphrase_modifiers modifiers))  in
                          let mkPhrase verb = match verb with
                          | PredicateIntransitive ( ref , verbe_,  subject, gramnbr    )  -> genere verb ref
                          | PredicateTransitive  ( ref , verbe_,  subject,  cod, gramnbr )  -> genere verb ref
                          | PredicateDiTransitive ( ref , verb_, SubAtom subject, SubAtom cod, SubAtom coi, gramnbr )  ->  genere verb ref
                          | _  -> failwith "aiguillage_phrase : Aiguillage : non verbe"
                          in (List.map mkPhrase verbes)@lst

                                (*["Phrase avec modifiers_pp"] *)
                                (**TODO Recoller avec les variables, où on cherche les modifier_pp qui ont la même variable que le predicate*)
        |  false, false, false,false  -> List.map objet_en_tant_que_sujet l
        |  false, true, false,false   -> ["Phrase intransitive (genre 'Il pleut')"]
        |  false, true, true, true    -> ["Phrase simple avec modifiers_pp et relation (of)"]
        |  false, true, false, true   -> ["Phrase avec relation(s)"]
        |  true , _, false, _  -> 
                let operators = List.filter is_Operator l in
                let gereOP operator = match operator with
                | Operator2(Imply, drs1, drs2) -> paraphrase_if_then drs1 drs2
                                        | Operator2(Not,   drs1, drs2) -> "Operator2 Not Pas géré"
                                        | Operator2(Union, drs1, drs2) -> "Operator2 union non géré"
                                        | Operator1(Not,   drs       ) -> " Pas géré: Operator1"
                                        | _ -> failwith "aiguillage_phrase : Pas opérateur géré" in
                 (List.map gereOP operators)@lst
        |  false, false, _ ,_         -> ["Cas non géré pour le moment, ou illogique"];;



let g = FullDRS ([Var "J1"; Var "K1"; Var "L1"],                                                                                              
[Object (Var "J1", Nom "time", Countable, Na, Greater, Number 2, 13, 8);                                                             
Object (Var "K1", Nom "day", Countable, Na, Eq, Number 10, 13, 13);
PredicateTransitive (Var "L1", Verbe "vote", SubAtom (Named "User1"),
Var "J1", Singular);
Modifier_pp (Var "L1", Preposition "in", Var "K1");
Modifier_pp (Var "L1", Preposition "for", SubAtom (Named "User2"))]);;
