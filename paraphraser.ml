open Syntax
open Drsxp

module S = BatString;;

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



let hashPredicat : (Syntax.var, Syntax.atom) Hashtbl.t =  Hashtbl.create 1;;
let pc = parsecomplete;;

let cleanDRS f =
        let findElem (Var d) el =
                match el with
                | Object( Var s, _, _, _, _, _, _, _,_ )           -> s = d
                | PredicateIntransitive ( Var s, _, _,_, _ )       -> s = d
                | PredicateTransitive   ( Var s, _, _,_, _, _ )    -> s = d
                | PredicateDiTransitive ( Var s, _, _,_, _, _, _ ) -> s = d
                | _                                               -> false in
        match f with
        | FullDRS(domain,l) -> FullDRS ( [] , L.filter (fun o -> not (L.exists (fun b -> findElem b o) domain) ) l )


let prop2str (Preposition prep) = prep;;

let verb2str verb sing = 
        match verb, sing with
        | "be", Plural     -> "are "
        | "be", Singular   -> "is "
        | "be", Partitive  -> "are "
        | "have", Singular -> "has"
        |  a, Singular     -> let laststr s = String.sub s (String.length s - 1) 1 in
                                if laststr a = "s" then a^" " else a^"s "
        |  a, Plural       ->  a^" " 
        |  a, Partitive    -> a^" ";;



let objet_en_tant_que_sujet obj =
        match obj with
        | Object(  ref, Nom name,  countable,  unittype,  op, Number count,_,x,y)  ->
                        (match countable, unittype, op with
                        | Countable, Na        , o    ->  (
                                match o with 
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
         | Object(  ref, Nom name,  countable,  unittype,  op,  notCountable,_,x,y)  -> name^" "
         | Named nom  -> nom^" "
         | String nom -> nom^" "
         | Property1Ary(ref, Adj happy , Pos) -> happy^" "
         | _ -> failwith "objet_en_tant_que_sujet : Element non objet"

;;

let find_obj x = 
        try
               Hashtbl.find_all hashPredicat (Var x) |> List.hd
        with e -> failwith ("Variable "^x^" non trouvée")

let matchsub e = 
                match e with
                | SubAtom subject -> objet_en_tant_que_sujet subject
                | Var x           -> find_obj x |> objet_en_tant_que_sujet
                | List l          -> "["^(String.concat "," l)^"]" 
                | Num  nbr        -> if nbr = 1 then "one" else string_of_int nbr
                | ConstStr s      -> s
                
        

let rec paraphrase_relation   lst_relat = 
        let phrase_relat = 
                let relat = List.find is_Relation lst_relat in
                match relat with
                | Relation(elemSrc, elemDest) -> (matchsub elemSrc)^" of "^(matchsub elemDest)^" "
                | _                           -> failwith "Not a relation" in
        try
                let verbe = List.find is_Predicate lst_relat in
                let verbe_relat v = 
                        match v with
                        | PredicateIntransitive ( ref , Verbe verb,_,  subject, gramnbr    )               ->  phrase_relat^(verb2str verb gramnbr)
                        | PredicateTransitive   ( ref , Verbe verb,_, elemSubj, cod, gramnbr )      ->  phrase_relat^(verb2str verb gramnbr)^(matchsub cod)
                        | PredicateTransitive   ( ref , Verbe verb,_, subject, Num nbr, gramnbr )      ->  phrase_relat^(verb2str verb gramnbr)^(string_of_int nbr)
                        | PredicateDiTransitive ( ref , Verbe verb,_,  subject, cod,  coi, gramnbr )  ->    phrase_relat^(verb2str verb gramnbr)^(matchsub cod)^"to "^(matchsub coi)
                        | _ -> failwith "paraphrase_from_verb : cas paraphrase non traité" in
                verbe_relat verbe
        with Not_found -> phrase_relat;;
        




let paraphrase_listItem  (List l) =
        "["^(String.concat "," l)^"]"

let rec paraphrase_modifiers  modif =
        match modif with
        | Modifier_pp(ref1,  preposition,  ref2)  -> (prop2str preposition)^" "^(matchsub ref2)
        | _  -> failwith "paraphrase_modifiers : Element non Modifier_pp"


and paraphrase_from_verb verbe = 
        try
        match verbe with
        | PredicateIntransitive (ref, Verbe verb,_, elem1, gramnbr) -> (matchsub elem1)^(verb2str verb gramnbr)
        | PredicateTransitive   (ref, Verbe verb,_, elem1, elem2, gramnbr) -> (matchsub elem1)^(verb2str verb gramnbr)^(matchsub elem2)
        | PredicateDiTransitive (ref, Verbe verb,_, elem1, elem2, elem3, gramnbr) -> (matchsub elem1)^(verb2str verb gramnbr)^(matchsub elem2)^" to "^(matchsub elem3)
        | _ -> failwith "paraphrase_from_verb : cas paraphrase non traité"
        with Not_found -> failwith "ahhhhhh"

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
        let aigs d = aiguillage_phrase d [] in
        let _,l = Syntax.getDRSCondition drs in
        let _   = Syntax.makeHashDomain hashPredicat drs in
        let gereOP operator = match operator with
                | Operator2(Imply, drs1, drs2) -> paraphrase_if_then drs1 drs2
                | Operator2(Not,   drs1, drs2) -> let d1 = aigs drs1 in let d2 = aigs drs2 in d1@[" NOT "]@d2 |> S.concat " "
                | Operator2(Union, drs1, drs2) -> let d1 = aigs drs1 in let d2 = aigs drs2 in d1@[" ET "]@d2 |> S.concat " "
                | Operator1(Not,   drs       ) -> let d1 = aigs drs in  [" NOT "]@d1 |> S.concat " "
                | _ -> failwith "aiguillage_phrase : Pas opérateur géré" in
        let genere verbeUniq variableVerbe = 
                                  let phrasebase = paraphrase_from_verb verbeUniq in
                                  let modifiers  = List.filter is_Modifier_pp [stringOfVar variableVerbe |> find_obj] in
                                  phrasebase^" "^(String.concat " " (List.map paraphrase_modifiers modifiers))  in
        let mkPhrase verb = (match verb with
                          | PredicateIntransitive ( ref , verbe_,_,  subject, gramnbr    )  -> genere verb ref
                          | PredicateTransitive  ( ref , verbe_,_,  subject,  cod, gramnbr )  -> genere verb ref
                          | PredicateDiTransitive ( ref , verb_,_, SubAtom subject, SubAtom cod, SubAtom coi, gramnbr )  ->  genere verb ref
                          | _  -> failwith "aiguillage_phrase : Aiguillage : non verbe")
                in
        match  exists_Operator l, exists_Predicate l, exists_Modifier_pp l, exists_Relation l with
        |  false, false, false, false  -> lst@(List.map objet_en_tant_que_sujet l)
        |  false, false, false, true   -> lst@[paraphrase_relation l]
        |  false, false, _ ,_         ->  lst@["Cas non géré pour le moment : If dans un if"];

        |  false, true,  true,  false  -> let verbes = List.filter is_Predicate  l in
                                        (List.map mkPhrase verbes)@lst
        |  false, true, false, true   -> lst@[paraphrase_relation l]                                        
        |  false, true,  false, false  -> let verbes = List.filter is_Predicate  l in (List.map paraphrase_from_verb verbes)@lst
                                (*["Phrase avec modifiers_pp"] *)
                                (**TODO Recoller avec les variables, où on cherche les modifier_pp qui ont la même variable que le predicate*)
       (* |  false, true, false,false   -> lst@["Phrase intransitive (genre 'Il pleut')"]*)
        |  false, true, true, true    -> lst@["Phrase simple avec modifiers_pp et relation (of)"]


        |  true , _, false, _  -> 
                let operators = List.filter is_Operator l in
                (List.map gereOP operators)@lst
                (*TODO : faire une Hash de tous les prédicats, avec leur nom de variable*)
        | true, true, true, _ ->
                let verbes = List.filter is_Predicate  l in 
                let operators = List.filter is_Operator l in
                (List.map gereOP operators)@(List.map mkPhrase verbes)@lst
;;


let dispatch_sentence drs =
        let _,conditions = Syntax.getDRSCondition drs in
        let predicats    = List.filter is_Predicate conditions in
        let ifthen       = List.filter is_Operator  conditions in
        let simples = List.flatten (List.map (fun e -> aiguillage_phrase (FullDRS([],[e])) []) predicats) in
        let conds   = List.flatten (List.map (fun e -> aiguillage_phrase (FullDRS([],[e])) []) ifthen) in
        simples@conds

let g = FullDRS ([Var "J1"; Var "K1"; Var "L1"],
[Object (Var "J1", Nom "time", Countable, Na, Greater, Number 2,[], 13, 8);
Object (Var "K1", Nom "day", Countable, Na, Eq, Number 10, [], 13, 13);
PredicateTransitive (Var "L1", Verbe "vote", [], SubAtom (Named "User1"), Var "J1", Singular);
 Modifier_pp (Var "L1", Preposition "in", Var "K1");
 Modifier_pp (Var "L1", Preposition "for", SubAtom (Named "User2"))]);;


let paraphrase drs = aiguillage_phrase drs [];;
(*
#trace paraphrase_if_then;;
#trace paraphrase_from_list;;
#trace paraphrase_from_verb;;
#trace paraphrase_relation;;
#trace objet_en_tant_que_sujet;;
*)
