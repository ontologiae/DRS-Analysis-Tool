open Batteries
module L = BatList;;
module H = BatHashtbl;;

Printexc.record_backtrace true;;


let rec comb m lst =
  match m, lst with
    0, _ -> [[]]
  | _, [] -> []
  | m, x :: xs -> List.map (fun y -> x :: y) (comb (pred m) xs) @
                  comb m xs
;;

(* This first grammar is just made for the parser *)    
type
 drs = DRS of domainp * conditionsp
and
domainp = varp list
and 
conditionsp = conditionDRSp list
and 
conditionDRSp = Operatorp2 of operator * drs * drs | Operatorp1 of operator * drs | Atomicp of atomp | SubDrsp of name * drs 
and
 varp = Varp of string
and name = string
(* By defining Command, Rule, Definition, Fact and Query, we can prebuild a first help
to a semantic analysis*)
and operator = Imply | Equal | Different | Inter | Union |  Must | Can | May | Not | Naf  | Rule | Definition | Command | Query | Fact | Question

and atomp = Atom of name * term list * int * int(*Les deux positions*)

and term = Variable of string | Const of name | ConstCh of name | Nbr of int | TermAtom of atomp | Listt of string list;;
    
type toplevel_cmd =
  | Assert of drs
  | Quit;;











(* This grammar is made for analysis, each specific ACE atom are represented *)
type fulldrs = FullDRS of domain * conditions
and conditions = atom list
and domain = var list
(*and 
conditionDRS = Operator2 of operator * fulldrs * fulldrs | Operator1 of operator * fulldrs | Atomic of atom*)
and var = Var of string | SubAtom of atom | Num of int | ConstStr of name | List of string list
(*Liste des primitives du DRS*)

and adjectif = Adj of name
and adverbe = Adv of name
and preposition = Preposition of name
and verbe = Verbe of name
and noun = Nom of name

and pronom_interrogatif = Who | What | Which | How | Where | When
and classtype = Dom | Mass | Countable
and unittype = Kg | Na | Meter
and op = Eq | Geq | Greater | Leq | Less | Exactly | NaOp (*Eq stands for “equal”, Geq for “greater or equal”, and Leq for “less or equal”. *)
and degre = Pos | Pos_as | Comp | Comp_than | Sup
and compTarget = Subj | Obj
and countable = NotCountable | Number of int
and grammaticalNumber = Plural | Singular | Partitive

and alterateur = atom list
and atom = 
(* Ref A variable that stands for this relation and that is used to attach modiﬁers (i.e. adverbs and 
prepositional phrases). 
Verb The intransitive, transitive, or ditransitive verb. 
SubjRef A variable or expression that stands for the subject. 
ObjRef A variable or expression that stands for the direct object. 
IndObjRef A variable or expression that stands for the indirect object. 
*)
  Operator2 of operator * fulldrs * fulldrs 
| Operator1 of operator * fulldrs
| SubDrs    of string   * fulldrs 
| PredicateIntransitive of var * verbe * alterateur * var * grammaticalNumber(*Subject*)
| PredicateTransitive of var * verbe * alterateur * var * var * grammaticalNumber(*Subject  COD *)
| PredicateDiTransitive of var * verbe * alterateur * var * var * var * grammaticalNumber(*Subject COD COI*)

(* Ref The variable that stands for this object and that is used for references. 
Noun The noun (mass or countable) that was used to introduce the object. 
Class This is one of  {dom,mass,countable} and deﬁnes the classiﬁcation of the object. mass and countable inherit from dom
Unit If the object was introduced together with a measurement noun (e.g. “2 kg of apples”) then this entry contains the value of the measurement noun (e.g. kg). Otherwise, this entry is na. 
Op One of {eq,geq,greater,leq,less,exactly,na}. eq stands for “equal”, geq for “greater or  equal”, and leq for “less or equal”. 
Count A positive number or na. Together with Unit and Op, this deﬁnes the cardinality or extent of the object. 
 *)
| Object of var * noun * classtype * unittype * op * countable * alterateur (*Ref,Noun,Class,Unit,Op,Count*) * int * int (*Pour le repère de pos*)


(*
Ref1 The variable or expression that stands for the primary object of the property (i.e. the subject). 
Ref2 The variable or expression that stands for the secondary object of the property. 
Ref3 The variable or expression that stands for the tertiary object of the property. 

Adjective The intransitive or transitive adjective. 
        
Degree This is one of {pos,pos as,comp,comp than,sup} and it deﬁnes the degree of the adjective. Positive and comparative forms can have an additional comparison target (“as rich as ...”, “richer than ...”), and for those cases pos as and comp than are used. 

CompTarget This is one of {subj,obj} and it deﬁnes for transitive adjectives whether the comparison targets the subject 
 (“John is more fond-of Mary than Bill”) or the object (“John is more fond-of Mary than of Sue”).
 *)
| Property1Ary of var * adjectif * degre 
| Property2Ary of var * adjectif * degre * var
| Property3Ary of var * adjectif * var * degre * compTarget * var


| Relation of var * var
(* Modifiers *)
| Modifier_Adv of var * adverbe * degre 
| Modifier_pp of var * preposition * var

| HasPart of var * var
| Named of  name (**)
| String of  name

| Query of var * pronom_interrogatif
| VarElem of var (*Pour les cas où on a des nums ou des trucs du genre dans la fonction treefyElem*)

| Rien ;;


(* Some little function to help analysis of the first grammar*)
let getClassType = function
  | "dom" -> Dom
  | "countable" -> Countable
  | "mass" -> Mass
  | _ -> Dom ;;

let getOp = function
  | "eq" -> Eq 
  | "geq" -> Geq 
  | "greater" ->Greater 
  | "leq" -> Leq 
  | "less" -> Less 
  | "exactly" -> Exactly 
  | "na" -> NaOp
  | _ -> Eq;;

let getUnitType = function
  | "na" -> Na
  | "kg" -> Kg
  | "meter" -> Meter
  | _ -> Na

let getDegree = function
  | "pos" -> Pos
  | "pos_as" -> Pos_as
  | "comp" -> Comp
  | "comp_than" -> Comp_than
  | "sup" -> Sup
  | _ -> Pos


let getCompTarget = function
  | "subj" -> Subj
  | "obj" -> Obj
  | _ -> Obj

let getQuestionWord = function
  | "who" -> Who
  | "when" -> When
  | "where" -> Where
  | "which" -> Which
  | "how" -> How
  | "what" -> What
  | _ -> What

(*toString functions*)           
let getCount a = try Number (int_of_string a)
with 
  | Failure("int_of_string") -> NotCountable;;

let rec term2string = function
  | TermAtom(a) -> atom2String a
  | Variable a ->  "V("^a^")"
  | Nbr a -> string_of_int a
  | ConstCh a ->  "Cch("^a^")"
  | Const a  ->  "C("^a^")"
and  atom2String = function
  |  Atom ( a , lst, x, y) -> (a^"("^(String.concat "," (List.map term2string lst))^")");;
    
(*Conversion functions *)
let varp2var = function
  | Varp a -> Var a;;




(** Conversion drs --> FulDRS *)
let rec drs_to_fulldrs = function
  | DRS (d,c) -> FullDRS ( List.map varp2var d,  List.map conditionDRS2ConditionFullDRS c)

and  conditionDRS2ConditionFullDRS = function
  | Operatorp1(op,a) -> Operator1 (  op, drs_to_fulldrs a)
  | Operatorp2(op,a,b) -> Operator2 ( op, drs_to_fulldrs a, drs_to_fulldrs b)
  | Atomicp a ->  atom2primitives a
  | SubDrsp (v,d)  -> SubDrs (v,drs_to_fulldrs d)

(* convert ACE atoms to specific predicate*)                    
and atom2primitives = function
(*Transformations spécifiques*)
   | Atom("object", [Variable var; Const  name; Const countable; Const unittype; Const op; Nbr    count],x,y) -> 
       Object(Var var, Nom(name), getClassType countable, getUnitType unittype, getOp op, Number count, [],x,y)

   | Atom("object", [Variable var; Const  name; Const countable; Const unittype; Const op; Const  count],x,y) -> 
       Object(Var var, Nom(name), getClassType countable, getUnitType unittype, getOp op, getCount count, [],x,y)

  |  Atom("object", [Variable var; ConstCh name; Const countable; Const unittype; Const op; Nbr   count],x,y) -> 
      Object(Var var, Nom(name), getClassType countable, getUnitType unittype, getOp op, Number count, [],x,y)

  |  Atom("object", [Variable var; ConstCh name; Const countable; Const unittype; Const op; Const count],x,y) -> 
      Object(Var var, Nom(name), getClassType countable, getUnitType unittype, getOp op, getCount count, [],x,y)

  | Atom("predicate",[ ref ;  verb; subject; cod ; coi ],x,y) ->  
                  PredicateDiTransitive(atomNamedString2NamedString  ref ,Verbe (extractString verb), [], atomNamedString2NamedString  subject , atomNamedString2NamedString cod, atomNamedString2NamedString coi, Singular)

  |  Atom("predicate",[ ref ;   verb; subject; cod ],     x,y) -> 
      PredicateTransitive(atomNamedString2NamedString  ref ,Verbe (extractString verb), [], atomNamedString2NamedString  subject , atomNamedString2NamedString cod, Singular)

  |  Atom("predicate",[ ref ;  verb; subject],            x,y) ->  
      PredicateIntransitive(atomNamedString2NamedString  ref ,Verbe (extractString verb), [], atomNamedString2NamedString  subject, Singular)

  |  Atom("property",[ref;adjective;degree],              x,y) -> Property1Ary (atomNamedString2NamedString ref, Adj (extractString adjective), getDegree (extractString degree))

  |  Atom("property",[ref;adjective;degree;ref2],         x,y) -> 
      Property2Ary (atomNamedString2NamedString ref, Adj (extractString adjective), getDegree (extractString degree),atomNamedString2NamedString ref2)

(*  |  Atom("property",[ref;adjective;degree;comptarget;ref3],x,y) -> 
     Property3Ary (atomNamedString2NamedString ref, Adj (extractString adjective), atomNamedString2NamedString ref2, getDegree (extractString degree), getCompTarget (extractString comptarget), atomNamedString2NamedString ref3)*)

  |  Atom("property",[ref;adjective;ref2;degree;comptarget;ref3],x,y) -> 
     Property3Ary (atomNamedString2NamedString ref, Adj (extractString adjective), atomNamedString2NamedString ref2, getDegree (extractString degree), getCompTarget (extractString comptarget), atomNamedString2NamedString ref3)


  | Atom("relation",[ref1; Const "of";ref2],    x,y) -> 
      Relation(atomNamedString2NamedString ref1, atomNamedString2NamedString ref2)

  | Atom("modifier_adv",[ref; adverb;degree],   x,y) -> Modifier_Adv( atomNamedString2NamedString ref, Adv (extractString adverb), getDegree (extractString degree))

  | Atom("modifier_pp", [ref1;preposition;ref2],x,y) -> 
      Modifier_pp (atomNamedString2NamedString ref1, Preposition (extractString preposition), atomNamedString2NamedString ref2)

  | Atom("has_part", [groupref;memberref],      x,y) -> HasPart(atomNamedString2NamedString groupref,atomNamedString2NamedString memberref)

  | Atom("query",[ref;questionWord],            x,y) -> Query(atomNamedString2NamedString ref, getQuestionWord (extractString questionWord))

  | Atom(a, l,x,y) -> print_endline ("Error with atom :"^(atom2String (Atom(a,l,x,y)))^". Check you're using ACE 6.6"); Rien




and atomNamedString2NamedString = function
  | TermAtom(Atom("named",[ConstCh properName ],a,b)) -> SubAtom(Named properName )
  | TermAtom(Atom("named",[Const properName ],a,b)) -> SubAtom(Named properName )  
  | TermAtom(Atom("string",[ConstCh stringConst ],a,b)) -> SubAtom(String stringConst)
  | TermAtom(Atom("string",[Const stringConst ],a,b)) -> SubAtom(String stringConst)  
  | TermAtom(Atom("int",[Nbr nb],a,b)) -> Num nb
  | TermAtom( Atom(a, l,x,y)) -> print_endline ("Error with atom :"^(atom2String (Atom(a,l,x,y)))^". Check you're using ACE 6.6"); Num 6
  | Variable a -> Var a
  | Nbr a -> Num a
  | ConstCh a -> ConstStr a
  | Const a  -> ConstStr a
  | Listt sl  -> List sl
and  extractString =  function
  | ConstCh a ->  a
  | Const a  ->  a
  | Variable a -> a
  | Nbr i      -> string_of_int i
  | Listt sl  -> String.concat "," sl
  | TermAtom a -> Printexc.get_backtrace () |> print_endline ; failwith "attendu constante";;






let is_Predicate          e = match e with 
                                                | PredicateIntransitive (_, _, _,_,_)     -> true
                                                | PredicateTransitive   (_, _, _,_,_,_)   -> true
                                                | PredicateDiTransitive (_, _, _,_,_,_,_) -> true
                                                | _                                     -> false;;

let is_Modifier_pp        e = match e with | Modifier_pp(_, _, _) -> true | _ -> false;;


let is_Operator           e = match e with | Operator2(_,_,_) -> true  | Operator1(_,_) -> true   | _ -> false;;


let is_Relation           e = match e with | Relation(_, _)                        -> true | _ -> false ;;
let is_Modifier_Adv       e = match e with | Modifier_Adv(_, _, _)                 -> true | _ -> false ;;
let is_Object             e = match e with | Object(_, _, _,_,_,_,_,_,_)             -> true | _ -> false ;;
let is_Property1          e = match e with | Property1Ary (_, _, _ )               -> true | _ -> false ;;
let is_Property2          e = match e with | Property2Ary (_, _, _, _ )            -> true | _ -> false ;;
let is_Property3          e = match e with | Property3Ary (_, _, _, _, _, _)       -> true | _ -> false ;;
let is_HasPart            e = match e with | HasPart (_, _)                        -> true | _ -> false ;;
let is_Query              e = match e with | Query   (_,  _)                       -> true | _ -> false ;;    


let is_alterateur e = is_Relation e || is_Modifier_Adv e || is_Modifier_pp e || is_Property1 e || is_Property2 e || is_Property3 e

let exists_Object       l = L.exists is_Object l;;
let exists_Predicate    l = L.exists is_Predicate l;;(* Logique à généraliser TODO*)
let exists_Modifier_pp  l = L.exists is_Modifier_pp l;;
let exists_Modifier_Adv l = L.exists is_Modifier_Adv l;;
let exists_Relation     l = L.exists is_Relation l;;
let exists_Operator     l = L.exists is_Operator l;;
let exists_Property1    l = L.exists is_Property1 l;;
let exists_Property23   l = L.exists (fun e -> is_Property2 e || is_Property3 e) l;;


let rec makeHashDomain hash drs =
        let domain, lstdrs = match drs with FullDRS( d, l) -> d, l in
        let inDomain v = L.exists (fun e -> e = v) domain in
        let rec makeHashElem1 hash elem =
                match elem with
                | Object(  ref,  name,  countable,  unittype,  op, count,_,x,y)         as self -> if inDomain ref then H.add hash ref self; hash
                | PredicateTransitive( ref, verb,_,   subject , cod, gramnbr )          as self -> if inDomain ref then H.add hash ref self; hash
                | PredicateDiTransitive(  ref ,verb,_,  subject ,  cod,  coi, gramnbr)  as self -> if inDomain ref then H.add hash ref self; hash
                | PredicateIntransitive(  ref , verb,_, subject, gramnbr)               as self -> if inDomain ref then H.add hash ref self; hash
                | Property1Ary (ref,  adjective, degree)                              as self -> if inDomain ref then H.add hash ref self; hash
                | Property2Ary (ref,  adjective,  degree, ref2)                       as self -> if inDomain ref then H.add hash ref self; hash
                | Property3Ary (ref,  adjective,  ref2,  degree,  comptarget, ref3)   as self -> if inDomain ref then H.add hash ref self; hash
                | Operator2 (op, b, c)                                                as self -> let h1 = makeHashDomain hash b in makeHashDomain h1 c
                | Operator1 (op, b)                                                   as self -> makeHashDomain hash b
                | _                                                                           -> hash in
        L.fold_left makeHashElem1 hash lstdrs 




let rec maptree f  = function
        | FullDRS (a,b) -> FullDRS(a,List.map (maptreeElem f) b)

and maptreeElem f = function
  | Object(  ref,  name,  countable,  unittype,  op, count,_,x,y)         as self -> f self
  | PredicateTransitive( ref, verb, _,  subject , cod, gramnbr )          as self -> f self
  | PredicateDiTransitive(  ref ,verb,_,  subject ,  cod,  coi, gramnbr)  as self -> f self 
  | PredicateIntransitive(  ref , verb,_, subject, gramnbr)               as self -> f self
  | Property1Ary (ref,  adjective, degree)                              as self -> f self 
  | Property2Ary (ref,  adjective,  degree, ref2)                       as self -> f self 
  | Property3Ary (ref,  adjective,  ref2,  degree,  comptarget, ref3)   as self -> f self 
  | Relation( ref1,  ref2)                                              as self -> f self 
  | Modifier_Adv(  ref, adverb,  degree)                                as self -> f self 
  | Modifier_pp ( ref1,  preposition, ref2)                             as self -> f self  
  | HasPart( groupref, memberref)                                       as self -> f self 
  | Query( ref,  questionWord)                                          as self -> f self 
  | Operator2 (op, b, c)                                                as self -> Operator2(op, maptree f b, maptree f c)
  | Operator1 (op, b)                                                   as self -> Operator1(op, maptree f b) 
  | String a                                                            as self -> f self 
  | Named  a                                                            as self -> f self
  | SubDrs(a, dr)                                                       as self -> SubDrs(a, maptree f dr)
  | Rien                                                                as self -> f self ;;




let rec makeHash hash =  function
        | FullDRS (a,b) -> List.fold_left makeHashElem hash b

and makeHashElem hash = 
        function
        | Object(  ref,  name,  countable,  unittype,  op, count,_,x,y)         as self -> H.add hash ref self; hash
        | PredicateTransitive( ref, verb,_,   subject , cod, gramnbr )          as self -> H.add hash ref self; hash
        | PredicateDiTransitive(  ref ,verb,_,  subject ,  cod,  coi, gramnbr)  as self -> H.add hash ref self; hash
        | PredicateIntransitive(  ref , verb,_, subject, gramnbr)               as self -> H.add hash ref self; hash
        | Property1Ary (ref,  adjective, degree)                              as self -> H.add hash ref self; hash
        | Property2Ary (ref,  adjective,  degree, ref2)                       as self -> H.add hash ref self; hash
        | Property3Ary (ref,  adjective,  ref2,  degree,  comptarget, ref3)   as self -> H.add hash ref self; hash
        | Relation( ref1,  ref2)                                              as self -> H.add hash ref1 self; hash
        | Modifier_Adv(  ref, adverb,  degree)                                as self -> H.add hash ref self; hash
        | Modifier_pp ( ref1,  preposition, ref2)                             as self -> H.add hash ref1 self; hash
        | HasPart( groupref, memberref)                                       as self -> H.add hash groupref self; hash
        | Query( ref,  questionWord)                                          as self -> H.add hash ref self; hash
        | Operator2 (op, b, c)                                                as self -> let h1 = makeHash hash b in makeHash h1 c
        | Operator1 (op, b)                                                   as self -> makeHash hash b
        | String a                                                            as self -> hash 
        | SubDrs(a, dr)                                                       as self -> makeHash hash dr
        | Named  a                                                            as self -> hash
        | Rien                                                                        -> hash
        
let getDRSCondition g  = match g with | FullDRS(a,b) -> a,b;;

let stringOfVar  = function
  | Var a -> a
  | ConstStr a -> a
  | Num a -> string_of_int a
  | _ -> "Rien"



(*let rec treefy2 arbr = *)
        let h           = H.create 67;;
        let hpass1  : (var, atom) H.t    = H.create 34;;
        (*let hashgen     = makeHash h arbr in 1;;*)

let rec treefyAtom a = Rien


(*and firstpass h hpass1 atom  =
                if is_alterateur atom then
                match atom with
                (* On va chercher les sous objets en params*)
                | Property2Ary (ref,  adjective,  degree, ref2)                        -> if H.mem h ref2 then 
                                                                                           Property2Ary(ref,adjective, degree, SubAtom(H.find h ref2) ) |> H.add hpass1 ref
                | Property3Ary (ref,  adjective,  ref2,  degree,  comptarget, ref3)    -> if H.mem h ref2 && H.mem h ref3 then 
                                                                                           Property3Ary(ref, adjective, SubAtom (H.find h ref2), degree, comptarget,SubAtom ( H.find h ref3) ) |> H.add hpass1 ref
                                                                                          else if H.mem h ref2 && (not  (H.mem h ref3)) then
                                                                                                  Property3Ary(ref,adjective, SubAtom (H.find h ref2), degree, comptarget, ref3  ) |> H.add hpass1 ref
                                                                                          else if  (not (H.mem h ref2)) && H.mem h ref3 then
                                                                                                  Property3Ary(ref,adjective, ref2  , degree, comptarget, SubAtom (H.find h  ref3) ) |> H.add hpass1 ref
                                                                                          else ()
                | Relation( ref1,  ref2)                                               -> if H.mem h ref1 && H.mem h ref2 then
                                                                                           Relation(SubAtom (H.find h ref1), SubAtom (H.find h ref2)) |> H.add hpass1 ref1
                                                                                          else () (*Une relation a FORCÉMENT les 2 vars référencées*)
                | Modifier_pp ( ref1,  preposition, ref2)                              -> if H.mem h ref2 then Modifier_pp(ref1,preposition, SubAtom (H.find h ref2)) |> H.add hpass1 ref1
                | _                                                                    -> ()
                (*| HasPart( groupref, memberref)                                        -> H.add hash groupref self; hash*)

                else ()*)
let rec treefy_drs_pass1 drs =
        let _ = makeHash h drs in
        match drs with
        | FullDRS (a,b) -> FullDRS( a, L.map (firstpass) b)

(* On arbrifie les paramètres de *)        
and firstpass  atom  =
                if is_alterateur atom then
                match atom with
                (* On va chercher les sous objets en params*)
                | Property2Ary (ref,  adjective,  degree, ref2)                        -> if H.mem h ref2 then Property2Ary(ref,adjective, degree, SubAtom(H.find h ref2) ) else atom
                | Property3Ary (ref,  adjective,  ref2,  degree,  comptarget, ref3)    -> if H.mem h ref2 && H.mem h ref3 then 
                                                                                                Property3Ary(ref, adjective, SubAtom (H.find h ref2), degree, comptarget,SubAtom ( H.find h ref3) ) 
                                                                                          else if H.mem h ref2 && (not  (H.mem h ref3)) then
                                                                                                  Property3Ary(ref,adjective, SubAtom (H.find h ref2), degree, comptarget, ref3  ) 
                                                                                          else if  (not (H.mem h ref2)) && H.mem h ref3 then
                                                                                                  Property3Ary(ref,adjective, ref2  , degree, comptarget, SubAtom (H.find h  ref3) ) 
                                                                                          else atom
                | Relation( ref1,  ref2)                                               -> if H.mem h ref1 && H.mem h ref2 then
                                                                                           Relation(SubAtom (H.find h ref1), SubAtom (H.find h ref2)) 
                                                                                          else atom (*Une relation a FORCÉMENT les 2 vars référencées*)
                | Modifier_pp ( ref1,  preposition, ref2)                              -> print_endline "Mod ok!"; 
                                                                                          if H.mem h ref2 then Modifier_pp(ref1,preposition, SubAtom (H.find h ref2)) else failwith "pas trouvé"
                                                                                          (*TODO gérer le subDRS qui soit lui aussi*)
       
                | _                                                                    -> atom
                else 
                        match atom with
                        | Operator2(op, d1, d2)                                        -> Operator2(op, treefy_drs_pass1 d1, treefy_drs_pass1 d2)
                        | Operator1(op, d)                                             -> Operator1(op, treefy_drs_pass1 d)
                        | SubDrs(s, d)                                                 -> SubDrs(s, treefy_drs_pass1 d)
                        | _ -> atom



let termScore atom =
        match atom with
        | Object(  ref,  name,  countable,  unittype,  op, count,_,x,y)          -> 5,0
        | PredicateTransitive( ref, verb, _,  subject , cod, gramnbr )           -> 6,2
        | PredicateDiTransitive(  ref ,verb,_,  subject ,  cod,  coi, gramnbr)   -> 6,3
        | PredicateIntransitive(  ref , verb,_, subject, gramnbr)                -> 6,1
        | Property1Ary (ref,  adjective, degree)                                 -> 1,0
        | Property2Ary (ref,  adjective,  degree, ref2)                        ->  2,0
        | Property3Ary (ref,  adjective,  ref2,  degree,  comptarget, ref3)    ->  2,2
        | Relation( ref1,  ref2)                                               ->  7,2
        | Modifier_Adv(  ref, adverb,  degree)                                 ->  1,0
        | Modifier_pp ( ref1,  preposition, ref2)                              ->  1,2
        | HasPart( groupref, memberref)                                        ->  2,2
        | Query( ref,  questionWord)                                           ->  5,0
        | Operator2 (op, b, c)                                                 -> 0,0
        | Operator1 (op, b)                                                    -> 0,0
        | String a                                                             -> 0 ,0
        | Named  a                                                             -> 0,0
        | SubDrs(a, dr)                                                        -> 0,0
        | Rien                                                                 -> 0,0 ;;


let getRefAndParams atom =
        match atom with
        | Object( ref,  name,  countable,  unittype,  op, count,_,x,y)                          -> [ref]
        | PredicateTransitive( ref, verb, _,  Var subject , Var cod, gramnbr )                  -> [ref; Var subject ; Var cod]
        | PredicateTransitive( ref, verb, _,  _ , Var cod, gramnbr )                            -> [ref; Var "KAMOULOX" ; Var cod]
        | PredicateTransitive( ref, verb, _,  Var subject , _, gramnbr )                        -> [ref; Var subject ; Var "KAMOULOX"]
        | PredicateTransitive( ref, verb, _,  _ , _, gramnbr )                                  -> [ref; Var "KAMOULOX" ; Var "KAMOULOX"]

        | PredicateDiTransitive(  ref ,verb,_,  Var subject , Var cod, Var coi, gramnbr)        -> [ref; Var subject ; Var cod; Var coi]
        | PredicateDiTransitive(  ref ,verb,_,  Var subject , Var cod, _, gramnbr)              -> [ref; Var subject ; Var cod; Var "KAMOULOX"]
        | PredicateDiTransitive(  ref ,verb,_,  Var subject , _, Var coi, gramnbr)              -> [ref; Var subject ; Var "KAMOULOX"; Var coi]
        | PredicateDiTransitive(  ref ,verb,_,  _ , Var cod, Var coi, gramnbr)                  -> [ref; Var "KAMOULOX" ; Var cod; Var coi]
        | PredicateDiTransitive(  ref ,verb,_,  _ , _, Var coi, gramnbr)                        -> [ref; Var "KAMOULOX" ; Var "KAMOULOX"; Var coi]
        | PredicateDiTransitive(  ref ,verb,_,  Var subject , _, Var coi, gramnbr)              -> [ref; Var subject ; Var "KAMOULOX"; Var coi]
        | PredicateDiTransitive(  ref ,verb,_,  _ , Var cod, _, gramnbr)                        -> [ref; Var "KAMOULOX" ; Var cod; Var "KAMOULOX"]
        | PredicateDiTransitive(  ref ,verb,_,  _ , _, _, gramnbr)                              -> [ref; Var "KAMOULOX" ; Var "KAMOULOX"; Var "KAMOULOX"]
        
        | PredicateIntransitive(  ref , verb,_, Var subject, gramnbr)                           -> [ref; Var subject]
        | PredicateIntransitive(  ref , verb,_, _, gramnbr)                                     -> [ref; Var "KAMOULOX"]
        | Property1Ary (ref,  adjective, degree)                                                -> [ref]
        | Property2Ary (ref,  adjective,  degree, Var ref2)                                     -> [ref; Var ref2]
        | Property2Ary (ref,  adjective,  degree, ref2)                                         -> [ref; ref2]
        
        | Property3Ary (ref,  adjective,  Var ref2,  degree,  comptarget, Var ref3)             -> [ref; Var ref2; Var ref3]
        | Property3Ary (ref,  adjective,   ref2,  degree,  comptarget, Var ref3)                -> [ref; Var ref3]
        | Property3Ary (ref,  adjective,  Var ref2,  degree,  comptarget,  ref3)                -> [ref; Var ref2]
        | Property3Ary (ref,  adjective,  ref2,  degree,  comptarget,  ref3)                    -> [ref]
        
        
        
        | Relation( ref1, Var ref2)                                                             -> [ref1; Var ref2 ]
        | Relation( ref1,  ref2)                                                                -> [ref1]        
        | Modifier_Adv(  ref, adverb,  degree)                                                  -> [ref]
        | Modifier_pp ( ref1,  preposition, Var ref2)                                           -> [ref1; Var ref2]
        | HasPart( groupref, Var memberref)                                                     -> [groupref; Var memberref]
        | Query( ref,  questionWord)                                                            -> [ref]
        | Operator2 (op, b, c)                                                                  -> [] (*TODO : gérer ces cas*)
        | Operator1 (op, b)                                                                     -> []
        | String a                                                                              -> []
        | Named  a                                                                              -> []
        | SubDrs(a, dr)                                                                         -> [Var a]
        | Rien                                                                                  -> [] ;;


let isVar refv atom =
                match atom with
                | Object( ref,  name,  countable,  unittype,  op, count,_,x,y)                          -> ref = refv
                | PredicateTransitive( ref, verb, _,  Var subject , Var cod, gramnbr )                  -> ref = refv
                | PredicateDiTransitive(  ref ,verb,_,  Var subject , Var cod, Var coi, gramnbr)        -> ref = refv
                | PredicateIntransitive(  ref , verb,_, Var subject, gramnbr)                           -> ref = refv
                | Property1Ary (ref,  adjective, degree)                                                -> ref = refv
                | Property2Ary (ref,  adjective,  degree, Var ref2)                                     -> ref = refv
                | Property3Ary (ref,  adjective,  Var ref2,  degree,  comptarget, Var ref3)             -> ref = refv
                | Relation( ref1, Var ref2)                                                             -> ref1 = refv
                | Modifier_Adv(  ref, adverb,  degree)                                                  -> ref = refv
                | Modifier_pp ( ref1,  preposition, Var ref2)                                           -> ref1 = refv
                | HasPart( groupref, Var memberref)                                                     -> groupref = refv
                | Query( ref,  questionWord)                                                            -> ref = refv
                | _                                                                                     -> false;;


let removeVarRefInList removeOnlyAlterateur refvp l =
        let removeAlt e = ( is_alterateur e) || (not removeOnlyAlterateur) in
        L.filter (fun e -> not(isVar refvp e) && removeAlt e) l;;


let removeElementAccordinfToVar l el =
        let var = getRefAndParams el |> L.hd in (*Attention....*)
        removeVarRefInList false var l



let findRef refvp l =
        try Some (L.find (isVar refvp) l)
        with Not_found -> None


let replaceBy atom byElem onVar =
        match atom with
        | Object( ref,  name,  countable,  unittype,  op, count, l ,x,y)                          -> Object( ref,  name,  countable,  unittype,  op, count, byElem::l ,x,y)
        | PredicateTransitive( ref, verb, l,   subject ,  cod, gramnbr ) when subject = onVar     -> PredicateTransitive( ref, verb, l,  SubAtom(byElem) ,  cod, gramnbr )
        | PredicateTransitive( ref, verb, l,   subject , cod, gramnbr ) when cod = onVar          -> PredicateTransitive( ref, verb, l,   subject , SubAtom(byElem), gramnbr )

        | PredicateDiTransitive( ref ,verb,l,  subject , cod, coi, gramnbr) when subject = onVar  -> PredicateDiTransitive(  ref ,verb,l, SubAtom(byElem) ,  cod,  coi, gramnbr)
        | PredicateDiTransitive( ref ,verb,l,   subject ,  cod,  coi, gramnbr) when cod = onVar   -> PredicateDiTransitive(  ref ,verb,l,   subject , SubAtom(byElem),  coi, gramnbr)
        | PredicateDiTransitive( ref ,verb,l,   subject ,  cod,  coi, gramnbr) when coi = onVar   -> PredicateDiTransitive(  ref ,verb,l,  subject , cod, SubAtom(byElem), gramnbr)
        
        | PredicateIntransitive(  ref , verb,l, subject, gramnbr) when   subject = onVar          ->  PredicateIntransitive(  ref , verb,l, SubAtom(byElem), gramnbr)
        | Relation( ref1, ref2)          when ref1 = onVar                                        -> Relation( SubAtom(byElem), ref2)
        | Relation( ref1, ref2)          when ref2 = onVar                                        -> Relation( ref1, SubAtom(byElem))
        | Modifier_pp ( ref1,  preposition,  ref2)   when ref2 = onVar                            -> Modifier_pp ( ref1,  preposition,  SubAtom(byElem))
        (*| HasPart( groupref, Var memberref)                                                       -> groupref = refv*)
        | Query( ref,  questionWord) when ref = onVar                                             -> Query( SubAtom(byElem),  questionWord)
        | e                                                                                       -> e;;


let sameVarRef atom1 atom2 =
        let a = getRefAndParams atom1 in
        let b = getRefAndParams atom2 in
        if L.length a > 0 && L.length b > 0 then L.hd a = L.hd b else false

let sameParam1Ref atom1 atom2 =
        let a = getRefAndParams atom1 in
        let b = getRefAndParams atom2 in
        if L.length a > 0 && L.length b > 1 then L.hd a = L.at b 1 else false

let sameParam2Ref atom1 atom2 =
        let a = getRefAndParams atom1 in
        let b = getRefAndParams atom2 in
        if L.length a > 0 && L.length b > 2 then L.hd a = L.at b 2 else false

let sameParam3Ref atom1 atom2 =
        let a = getRefAndParams atom1 in
        let b = getRefAndParams atom2 in
        if L.length a > 0 && L.length b > 3 then L.hd a = L.at b 3 else false

(*Return sorted couples of term of same var from the list
 * testNoParam smallest score doens't have a param*)        
let getCouples  fctestEquiv testNoParam l =
        let lElemScore = L.map (fun e -> let point, terme = termScore e in e,point, terme) l in
        let couples = comb 2 lElemScore |> L.map (fun l -> L.hd l, L.at l 1) |> L.filter (fun (a,b) -> let r1,s1,p1 = a in
                                                                                           let r2,s2,p2 = b in
                                                                                           r1 |> fctestEquiv r2  && (testNoParam || (if s1 > s2 then p2 = 0 else p1 = 0))  && a != b
                                                                                        )
                                        |> L.map (fun ((a,p1,t1),(b,p2,t2)) -> if p1 > p2 then ((b,p2,t2),(a,p1,t1)) else ((a,p1,t1),(b,p2,t2)) ) in                                                                              
        couples



(*let rec treefy_drs_pass1 drs =
        let _ = makeHash h drs in
        match drs with
        | FullDRS (a,b) -> FullDRS( a,  pass1 b )*)

let rule1 atom1 atom2 =
        match is_Property1 atom1, atom2 with
        | true, Object( ref',  name,  countable,  unittype,  op, count, lst, x, y) -> Object( ref',  name,  countable,  unittype,  op, count, atom1::lst, x, y)
        | _ -> atom2

let rule2 atom1 atom2 =
        match is_Modifier_Adv atom1, atom2 with
        | true, PredicateTransitive( ref', verb, l,   subject ,  cod, gramnbr )    -> PredicateTransitive( ref', verb, atom1::l,   subject ,  cod, gramnbr )
        | true, PredicateDiTransitive( ref' ,verb,l,  subject , cod, coi, gramnbr) -> PredicateDiTransitive( ref' ,verb, atom1::l,  subject , cod, coi, gramnbr)
        | true, PredicateIntransitive(  ref' , verb, l, subject, gramnbr)          -> PredicateIntransitive(  ref' , verb, atom1::l, subject, gramnbr)
        | _ -> atom2

let rule3 atom1 atom2 =
        match atom1, atom2 with
        | Property2Ary (ref,  adjective, degree, ref2), Object( ref',  name,  countable,  unittype,  op, count, lst, x, y) ->
                       Property2Ary (ref,  adjective, degree, SubAtom( Object( ref',  name,  countable,  unittype,  op, count, lst, x, y)))
        | _ -> atom2

let rule4 atom1 atom2 =
          match  atom1, atom2 with
        | Modifier_pp(  ref, adverb,_) ,  Object( ref',  name,  countable,  unittype,  op, count, lst, x, y)   -> Modifier_pp(  ref, adverb, SubAtom( atom1))
        | _ -> atom1

let rule5 atom1 atom2 =
        match is_Property2 atom1, atom2 with
        | true, PredicateTransitive( ref', verb, l,   subject ,  cod, gramnbr )    -> PredicateTransitive( ref', verb, atom1::l,   subject ,  cod, gramnbr )
        | true, PredicateDiTransitive( ref' ,verb,l,  subject , cod, coi, gramnbr) -> PredicateDiTransitive( ref' ,verb, atom1::l,  subject , cod, coi, gramnbr)
        | true, PredicateIntransitive(  ref' , verb, l, subject, gramnbr)          -> PredicateIntransitive(  ref' , verb, atom1::l, subject, gramnbr)
        | _ -> atom2

let rule7 atom1 atom2 =
        match is_Property2 atom1, atom2 with
        | true, PredicateTransitive( ref', verb, l,   subject ,  cod, gramnbr )    -> PredicateTransitive( ref', verb, l,   subject ,  SubAtom(atom1), gramnbr )
        | true, PredicateDiTransitive( ref' ,verb,l,  subject , cod, coi, gramnbr) -> PredicateDiTransitive( ref' ,verb, l,  subject , SubAtom(atom1), coi, gramnbr)
        | _ -> atom2

let rule8 atom1 atom2 =
        match is_Object atom1, atom2 with
        | true, PredicateTransitive( ref', verb, l,   subject ,  cod, gramnbr )    -> PredicateTransitive( ref', verb, l,   SubAtom(atom1) ,  cod, gramnbr )
        | true, PredicateDiTransitive( ref' ,verb,l,  subject , cod, coi, gramnbr) -> PredicateDiTransitive( ref' ,verb, l, SubAtom(atom1)  , cod, coi, gramnbr)
        | true, PredicateIntransitive(  ref' , verb, l, subject, gramnbr)          -> PredicateIntransitive(  ref' , verb, l, SubAtom(atom1) , gramnbr)
        | _ -> atom2


let rule9 atom1 atom2 =
        match is_Object atom1, atom2 with
        | true, PredicateTransitive( ref', verb, l,   subject ,  cod, gramnbr )    -> PredicateTransitive( ref', verb, l, subject,  SubAtom(atom1) , gramnbr )
        | true, PredicateDiTransitive( ref' ,verb,l,  subject , cod, coi, gramnbr) -> PredicateDiTransitive( ref' ,verb, l, subject, SubAtom(atom1), coi, gramnbr)
        | _ -> atom2

let rule10 atom1 atom2 =
        match is_Object atom1, atom2 with
        | true, PredicateDiTransitive( ref' ,verb,l,  subject , cod, coi, gramnbr) -> PredicateDiTransitive( ref' ,verb, l, subject, cod, SubAtom(atom1), gramnbr)
        | _ -> atom2
        

let couplesRegle1 fctest1 fctest2 couples = L.filter (fun ((a,_,_),(b,_,_)) -> fctest1 a && fctest2 b ) couples
let replaced fcReplace couplesRegle1v =  L.map ( fun ((a,_,_),(b,_,_)) -> fcReplace a b) couplesRegle1v

let rec reglex l fctest1 fctest2 fcReplace fcTestEquiv noNeedParam =
        let couples = getCouples fcTestEquiv noNeedParam l in
        let couplesRegle1v = couplesRegle1 fctest1 fctest2 couples in (* Couples sorted by points, so always in this order*)
        let replacedv =  replaced fcReplace couplesRegle1v in
        (*Là, on supprime les remplacés dans l, donc on cherche leur variable*)
        let lok = L.fold_left removeElementAccordinfToVar l replacedv in
        replacedv, replacedv@lok


let regle1 l = reglex l is_Property1 is_Object rule1 sameVarRef false
let regle2 l = reglex l is_Modifier_Adv is_Predicate rule2 sameVarRef false
let regle3 l = reglex l is_Property2 is_Object rule3 sameParam1Ref true
let regle4 l = reglex l is_Modifier_pp is_Object rule4 sameParam1Ref true
let regle5 l = reglex l is_Property2 is_Predicate rule5 sameVarRef true
let regle6 l = reglex l is_Property2 is_Predicate rule5 sameParam1Ref true
let regle7 l = reglex l is_Property2 is_Predicate rule7 sameParam2Ref true
let regle8 l = reglex l is_Object is_Predicate rule8 sameParam1Ref true
let regle9 l = reglex l is_Object is_Predicate rule9 sameParam2Ref true
let regle10 l = reglex l is_Object is_Predicate rule10 sameParam3Ref true



let getVarsAccordingToRule atom rul =
        let addIfRightPos var  isOktoAdd =
                if isOktoAdd then Some(var) else None in
        let defvar, alterateur, param1, param2, param3 = rul in
        match atom with
        | Object( ref,  name,  countable,  unittype,  op, count,_,x,y)                          -> [addIfRightPos ref defvar]
        | PredicateTransitive( ref, verb, _,  Var subject , Var cod, gramnbr )                  -> [addIfRightPos refdefvar; addIfRightPos (Var subject) param1 ; addIfRightPos (Var cod) param2]
        (*TODO : continuer le délire*)
        | PredicateTransitive( ref, verb, _,  _ , Var cod, gramnbr )                            -> [ref; Var "KAMOULOX" ; Var cod]
        | PredicateTransitive( ref, verb, _,  Var subject , _, gramnbr )                        -> [ref; Var subject ; Var "KAMOULOX"]
        | PredicateTransitive( ref, verb, _,  _ , _, gramnbr )                                  -> [ref; Var "KAMOULOX" ; Var "KAMOULOX"]

        | PredicateDiTransitive(  ref ,verb,_,  Var subject , Var cod, Var coi, gramnbr)        -> [ref; Var subject ; Var cod; Var coi]
        | PredicateDiTransitive(  ref ,verb,_,  Var subject , Var cod, _, gramnbr)              -> [ref; Var subject ; Var cod; Var "KAMOULOX"]
        | PredicateDiTransitive(  ref ,verb,_,  Var subject , _, Var coi, gramnbr)              -> [ref; Var subject ; Var "KAMOULOX"; Var coi]
        | PredicateDiTransitive(  ref ,verb,_,  _ , Var cod, Var coi, gramnbr)                  -> [ref; Var "KAMOULOX" ; Var cod; Var coi]
        | PredicateDiTransitive(  ref ,verb,_,  _ , _, Var coi, gramnbr)                        -> [ref; Var "KAMOULOX" ; Var "KAMOULOX"; Var coi]
        | PredicateDiTransitive(  ref ,verb,_,  Var subject , _, Var coi, gramnbr)              -> [ref; Var subject ; Var "KAMOULOX"; Var coi]
        | PredicateDiTransitive(  ref ,verb,_,  _ , Var cod, _, gramnbr)                        -> [ref; Var "KAMOULOX" ; Var cod; Var "KAMOULOX"]
        | PredicateDiTransitive(  ref ,verb,_,  _ , _, _, gramnbr)                              -> [ref; Var "KAMOULOX" ; Var "KAMOULOX"; Var "KAMOULOX"]
        
        | PredicateIntransitive(  ref , verb,_, Var subject, gramnbr)                           -> [ref; Var subject]
        | PredicateIntransitive(  ref , verb,_, _, gramnbr)                                     -> [ref; Var "KAMOULOX"]
        | Property1Ary (ref,  adjective, degree)                                                -> [ref]
        | Property2Ary (ref,  adjective,  degree, Var ref2)                                     -> [ref; Var ref2]
        | Property2Ary (ref,  adjective,  degree, ref2)                                         -> [ref; ref2]
        
        | Property3Ary (ref,  adjective,  Var ref2,  degree,  comptarget, Var ref3)             -> [ref; Var ref2; Var ref3]
        | Property3Ary (ref,  adjective,   ref2,  degree,  comptarget, Var ref3)                -> [ref; Var ref3]
        | Property3Ary (ref,  adjective,  Var ref2,  degree,  comptarget,  ref3)                -> [ref; Var ref2]
        | Property3Ary (ref,  adjective,  ref2,  degree,  comptarget,  ref3)                    -> [ref]
        
        
        
        | Relation( ref1, Var ref2)                                                             -> [ref1; Var ref2 ]
        | Relation( ref1,  ref2)                                                                -> [ref1]        
        | Modifier_Adv(  ref, adverb,  degree)                                                  -> [ref]
        | Modifier_pp ( ref1,  preposition, Var ref2)                                           -> [ref1; Var ref2]
        | HasPart( groupref, Var memberref)                                                     -> [groupref; Var memberref]
        | Query( ref,  questionWord)                                                            -> [ref]
        | Operator2 (op, b, c)                                                                  -> [] (*TODO : gérer ces cas*)
        | Operator1 (op, b)                                                                     -> []
        | String a                                                                              -> []
        | Named  a                                                                              -> []
        | SubDrs(a, dr)                                                                         -> [Var a]
        | Rien                                                                                  -> [] ;;
(*TODO : une fois la liste terminée, filtrer les None et transformer les Some en Var*)

(* INTERPRÉTEUR DE RÈGLE
 *
 * 1. Décrire quel terme est relié à quel autre en fonction de la position de la variable du terme 1 et du terme 2 (ie. construction du couple)
 * 2. Description des possibilités de substitution : terme1 dans altérateur de terme2, dans param1, param2, param3. Avec possibilité multiples : atérateur uniquement, param1,2,3
 *
 * EXEMPLE :
         * Objet -> predicate si l'un des param est liés
         * Rule ( RObjet, RPredicate, LinkPos (true,false, false, false,false), DestPos (false, false, true, true, true))
         * ie : Objet recherchés = objet et prédicat, l'objet est lié à une variable du prédicat uniquement via sa variable, et le lien est fait uniquement sur les param1,2,3
         *
 * Mise en oeuvre : recherche de couples dans le bon sens. On va rendre plus intelligent le fctestEquiv dans getCouples en lui donnant Link (true,false, false, false,false), Dest (false, false, true, true, true)
 * écriture de cette fonction : pour chaque true, je relève la variable si elle existe => Falloir faire un match pour chaque cas, idem pour le dest FIXME NOTE => getVarsAccordingToRule
 * TODO: Sur les 2 termes on lance un getVarsAccordingToRule en fonction de la règle LinkPos/DestPos, et on regarde ce qui matche dans les 2 listes. A chaque fois qu'il y a match, on fait la subtstitution du 1er dans le 2nd
 * *)



 (* Principes
 * prop1ary(ref) : on le met dans ref en tant que modificateur | Algo: Si on tombe sur un objet(ref,...), on vérifie qu'on a pas un prop1ary(ref) dans le H, si oui, on le met dans l'objet
 * prop2ary(ref,x) : on remplace la variable 2 par son terme, et on le met dans ref en tant que modificateur | Algo : Si on tombe sur predicate/objet, on vérifie qu'on ait pas un prop2ary(ref,x) tel que pred/objet(...ref...). Dans le
    prop2ary, en queue, on met l'objet
 * prop3ary(ref,x,y) : on remplace la variable 2 et 3 par leur termes respectifs et on le met dans ref en tant que modificateur | idem prop2ary avec
 * relation(x,y) : deviens une "tête", on remplace x et y par leur terme | Algo : on remplace x et y par leur terme respectif, et si on tombe sur un objet ou predicate ayant un ref relation(ref,x), alors on remplace
 * modifier_pp(ref, x) : on remplace x par son terme et on met le modifier_pp dans le predicate ref en tant que modificateur | Algo : on remplace x par son terme, puis si on trouve un objet ayant ref comme variable, alors on le met à la place
 * modifier_adv(ref) : on met le modifier_adv dans le predicate ref en tant que modificateur | Algo: Si on tombe sur un objet(ref,...), on vérifie qu'on a pas un prop1ary(ref) dans le H, si oui, on le met dans l'objet
 * Réfléchir au fait que object peut devenir un groupe objet et contenir plein de trucs avec has_part
 *
 *
 *
 * I. Faire un hash de TOUT (reprendre la 1ère fonction en fait)
 * II. Faire une première passe où on remplace les paramètres second de terme secondaire comme propXary, relation, modifier_yy par leur objet. A chaque fois, faire un hash ref -> terme construit
 * III. 
 *
 * 
*)
