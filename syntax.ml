(* This first grammar is just made for the parser *)    
type
 drs = DRS of domainp * conditionsp
and
domainp = varp list
and 
conditionsp = conditionDRSp list
and 
conditionDRSp = Operatorp2 of operator * drs * drs | Operatorp1 of operator * drs | Atomicp of atomp
and
 varp = Varp of string
and name = string
(* By defining Command, Rule, Definition, Fact and Query, we can prebuild a first help
to a semantic analysis*)
and operator = Imply | Equal | Different | Inter | Union |  Must | Can | May | Not | Naf  | Rule | Definition | Command | Query | Fact

and atomp = Atom of name * term list * int * int(*Les deux positions*)

and term = Variable of string | Const of name | ConstCh of name | Nbr of int | TermAtom of atomp   ;;
    
type toplevel_cmd =
  | Assert of drs
  | Quit;;











(* This grammar is made for analysis, each specific ACE atom are represented *)
type fulldrs = FullDRS of domain * conditions
and conditions = atom list
and domain = var list
(*and 
conditionDRS = Operator2 of operator * fulldrs * fulldrs | Operator1 of operator * fulldrs | Atomic of atom*)
and var = Var of string | SubAtom of atom | Num of int | ConstStr of name
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
| PredicateIntransitive of var * verbe * var * grammaticalNumber(*Subject*)
| PredicateTransitive of var * verbe * var * var * grammaticalNumber(*Subject  COD *)
| PredicateDiTransitive of var * verbe * var * var * var * grammaticalNumber(*Subject COD COI*)

(* Ref The variable that stands for this object and that is used for references. 
Noun The noun (mass or countable) that was used to introduce the object. 
Class This is one of  {dom,mass,countable} and deﬁnes the classiﬁcation of the object. mass and countable inherit from dom
Unit If the object was introduced together with a measurement noun (e.g. “2 kg of apples”) then this entry contains the value of the measurement noun (e.g. kg). Otherwise, this entry is na. 
Op One of {eq,geq,greater,leq,less,exactly,na}. eq stands for “equal”, geq for “greater or  equal”, and leq for “less or equal”. 
Count A positive number or na. Together with Unit and Op, this deﬁnes the cardinality or extent of the object. 
 *)
| Object of var * noun * classtype * unittype * op * countable (*Ref,Noun,Class,Unit,Op,Count*) * int * int (*Pour le repère de pos*)


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
| PartOf of var * var 
| Named of  name (**)
| String of  name

| Query of var * pronom_interrogatif
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

(* convert ACE atoms to specific predicate*)                    
and atom2primitives = function
(*Transformations spécifiques*)
   | Atom("object", [Variable var; Const  name; Const countable; Const unittype; Const op; Nbr    count],x,y) -> 
       Object(Var var, Nom(name), getClassType countable, getUnitType unittype, getOp op, Number count,x,y)

   | Atom("object", [Variable var; Const  name; Const countable; Const unittype; Const op; Const  count],x,y) -> 
       Object(Var var, Nom(name), getClassType countable, getUnitType unittype, getOp op, getCount count,x,y)

  |  Atom("object", [Variable var; ConstCh name; Const countable; Const unittype; Const op; Nbr   count],x,y) -> 
      Object(Var var, Nom(name), getClassType countable, getUnitType unittype, getOp op, Number count,x,y)

  |  Atom("object", [Variable var; ConstCh name; Const countable; Const unittype; Const op; Const count],x,y) -> 
      Object(Var var, Nom(name), getClassType countable, getUnitType unittype, getOp op, getCount count,x,y)

  | Atom("predicate",[ ref ;  verb; subject; cod ; coi ],x,y) ->  
       PredicateDiTransitive(atomNamedString2NamedString  ref ,Verbe (extractString verb), atomNamedString2NamedString  subject , atomNamedString2NamedString cod, atomNamedString2NamedString coi, Singular)

  |  Atom("predicate",[ ref ;   verb; subject; cod ],     x,y) -> 
      PredicateTransitive(atomNamedString2NamedString  ref ,Verbe (extractString verb), atomNamedString2NamedString  subject , atomNamedString2NamedString cod, Singular)

  |  Atom("predicate",[ ref ;  verb; subject],            x,y) ->  
      PredicateIntransitive(atomNamedString2NamedString  ref ,Verbe (extractString verb), atomNamedString2NamedString  subject, Singular)

  |  Atom("property",[ref;adjective;degree],              x,y) -> Property1Ary (atomNamedString2NamedString ref, Adj (extractString adjective), getDegree (extractString degree))

  |  Atom("property",[ref;adjective;degree;ref2],         x,y) -> 
      Property2Ary (atomNamedString2NamedString ref, Adj (extractString adjective), getDegree (extractString degree),atomNamedString2NamedString ref2)

 |  Atom("property",[ref;adjective;degree;ref2;comptarget;ref3],x,y) -> 
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
  | TermAtom(Atom("string",[ConstCh stringConst ],a,b)) -> SubAtom(String stringConst)
  | TermAtom(Atom("int",[Nbr nb],a,b)) -> Num nb
  | TermAtom( Atom(a, l,x,y)) -> print_endline ("Error with atom :"^(atom2String (Atom(a,l,x,y)))^". Check you're using ACE 6.6"); Num 6
  | Variable a -> Var a
  | Nbr a -> Num a
  | ConstCh a -> ConstStr a
  | Const a  -> ConstStr a
and  extractString =  function
  | ConstCh a ->  a
  | Const a  ->  a
  | Variable a -> a
  | Nbr i      -> string_of_int i
  | TermAtom a -> failwith "attendu constante";; 






let is_Predicate          e = match e with 
                                                | PredicateIntransitive (_, _, _,_)     -> true
                                                | PredicateTransitive   (_, _, _,_,_)   -> true
                                                | PredicateDiTransitive (_, _, _,_,_,_) -> true
                                                | _                                     -> false;;

let is_Modifier_pp        e = match e with | Modifier_pp(_, _, _) -> true | _ -> false;;


let is_Operator           e = match e with | Operator2(_,_,_) -> true     | _ -> false;;


let is_Relation           e = match e with | Relation(_, _)                        -> true | _ -> false ;;
let is_Modifier_Adv       e = match e with | Modifier_Adv(_, _, _)                 -> true | _ -> false ;;
let is_Object             e = match e with | Object(_, _, _,_,_,_,_,_)             -> true | _ -> false ;;
let is_Property2          e = match e with | Property2Ary (_, _, _, _ )            -> true | _ -> false ;;
let is_Property3          e = match e with | Property3Ary (_, _, _, _, _, _)       -> true | _ -> false ;;
let is_HasPart            e = match e with | HasPart (_, _)                        -> true | _ -> false ;;
let is_PartOf             e = match e with | PartOf  (_, _)                        -> true | _ -> false ;;
let is_Query              e = match e with | Query   (_,  _)                       -> true | _ -> false ;;    


let exists_Object       l = List.exists is_Object l;;
let exists_Predicate    l = List.exists is_Predicate l;;(* Logique à généraliser TODO*)
let exists_Modifier_pp  l = List.exists is_Modifier_pp l;;
let exists_Modifier_Adv l = List.exists is_Modifier_Adv l;;
let exists_Relation     l = List.exists is_Relation l;;
let exists_Operator     l = List.exists is_Operator l;;





let rec maptree f  = function
                                                | FullDRS (a,b) -> FullDRS(a,List.map (maptreeElem f) b)
and maptreeElem f = function
  | Object(  ref,  name,  countable,  unittype,  op, count,x,y)         as self -> f self
  | PredicateTransitive( ref, verb,   subject , cod, gramnbr )          as self -> f self
  | PredicateDiTransitive(  ref ,verb,  subject ,  cod,  coi, gramnbr)  as self -> f self 
  | PredicateIntransitive(  ref , verb, subject, gramnbr)               as self -> f self
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
  | PartOf(a,b)                                                         as self -> f self 
  | Rien                                                                as self -> f self ;;




let rec makeHash hash =  function
        | FullDRS (a,b) -> List.fold_left makeHashElem hash b

and makeHashElem hash = function
        | Object(  ref,  name,  countable,  unittype,  op, count,x,y)         as self -> Hashtbl.add hash ref self; hash
        | PredicateTransitive( ref, verb,   subject , cod, gramnbr )          as self -> Hashtbl.add hash ref self; hash
        | PredicateDiTransitive(  ref ,verb,  subject ,  cod,  coi, gramnbr)  as self -> Hashtbl.add hash ref self; hash
        | PredicateIntransitive(  ref , verb, subject, gramnbr)               as self -> Hashtbl.add hash ref self; hash
        | Property1Ary (ref,  adjective, degree)                              as self -> Hashtbl.add hash ref self; hash
        | Property2Ary (ref,  adjective,  degree, ref2)                       as self -> Hashtbl.add hash ref self; hash
        | Property3Ary (ref,  adjective,  ref2,  degree,  comptarget, ref3)   as self -> Hashtbl.add hash ref self; hash
        | Relation( ref1,  ref2)                                              as self -> hash
        | Modifier_Adv(  ref, adverb,  degree)                                as self -> Hashtbl.add hash ref self; hash
        | Modifier_pp ( ref1,  preposition, ref2)                             as self -> Hashtbl.add hash ref1 self; hash
        | HasPart( groupref, memberref)                                       as self -> Hashtbl.add hash groupref self; hash
        | Query( ref,  questionWord)                                          as self -> Hashtbl.add hash ref self; hash
        | Operator2 (op, b, c)                                                as self -> let h1 = makeHash hash b in makeHash h1 c
        | Operator1 (op, b)                                                   as self -> makeHash hash b
        | String a                                                            as self -> hash 
        | Named  a                                                            as self -> hash
        | PartOf(a,b)                                                         as self -> hash
        | Rien                                                                        -> hash







let trouve_element_noeud lst    = List.find (fun e ->  
                                                is_Predicate e || is_Relation e || is_Operator e || is_Relation e || is_Modifier_pp e || is_PartOf e ||
                                                is_HasPart e   || is_Property2 e || is_Property3 e) lst;;
                                               
let trouve_verbe lst            = List.find  is_Predicate lst ;;

let trouve_relation_noeud lst   = List.find is_Relation lst ;;

let trouve_operator2  lst       = List.find is_Operator lst;;


let dico drs =  let hash = Hashtbl.create 128 in
                makeHash hash drs


let getItemsByVar dico var drs =
                 Hashtbl.find_all dico var;;


let getItemsByVarWithoutModifier dico var drs =
        let filtre e = (not (is_Modifier_pp e)) && (not (is_Modifier_Adv e)) && (not (is_Relation e)) &&  (not (is_HasPart e))  in
        List.filter filtre (getItemsByVar dico var drs);;



let rec getItemsByVar var = function
  | Object(  ref,  name,  countable,  unittype,  op, count,x,y)         as self -> if ref = var then [self] else []
  | PredicateTransitive( ref, verb,   subject , cod, gramnbr )          as self -> if ref = var then [self] else []
  | PredicateDiTransitive(  ref ,verb,  subject ,  cod,  coi, gramnbr)  as self -> if ref = var then [self] else []
  | PredicateIntransitive(  ref , verb, subject, gramnbr)               as self -> if ref = var then [self] else []
  | Property1Ary (ref,  adjective, degree)                              as self -> if ref = var then [self] else []
  | Property2Ary (ref,  adjective,  degree, ref2)                       as self -> if ref = var then [self] else [] 
  | Property3Ary (ref,  adjective,  ref2,  degree,  comptarget, ref3)   as self -> if ref = var then [self] else [] 
  | Relation( ref1,  ref2)                                              as self -> [] 
  | Modifier_Adv(  ref, adverb,  degree)                                as self -> if ref = var then [self] else []
  | Modifier_pp ( ref1,  preposition, ref2)                             as self -> if ref1 = var then  [self] else []
  | HasPart( groupref, memberref)                                               -> [] 
  | Query( ref,  questionWord)                                          as self -> if ref = var then [self] else []
  | Operator2 (op, b, c)                                                        ->  (getItemsByVarIntoDRS var b)::(getItemsByVarIntoDRS var c)::[]
  | Operator1 (op, b)                                                           -> [(getItemsByVarIntoDRS var b)]
  | String a                                                                    -> [] 
  | Named  a                                                                    -> [] 
  | PartOf(a,b)                                                                 -> [] 
  | Rien                                                                        -> []
and getItemsByVarIntoDRS var drs = match drs with
  | FullDRS (a,b) -> List.find (fun a -> if a = Rien then false else true) (List.flatten (List.map (getItemsByVar var) b)) 
(*and getItemByVarIntoDRS var drs = match (getItemsByVarIntoDRS var drs) with 
  | t::q  -> t
  | t::[] -> t
  | []    -> Rien*)
and findItemInList lst var = try List.find (fun e -> List.length (getItemsByVar var e) > 0) lst with e -> let r = Printexc.to_string e in print_endline r;Rien
and findItemsInList lst var = try List.filter (fun e -> List.length (getItemsByVar var e) > 0) lst with e -> let r = Printexc.to_string e in print_endline r;[];;

(*let rec substract l1 l2 = List.filter (fun elem -> not (List.mem elem l2) ) l1;;*)

let rec substract l1 l2 =
match l1,l2 with
| []     ,      _ -> []
| l      , []     -> l 
| h1::[] , h2::[] -> if h1 = h2 then [] else [h1]
| h1::t1 , l2     -> let newl2 = (substract l2 [h1] ) in if List.mem h1 l2 then substract t1 newl2 else h1::(substract t1 newl2 );;


let stringOfVar  = function
  | Var a -> a
  | ConstStr a -> a
  | Num a -> string_of_int a
  | _ -> "Rien"


let getAtomOfSubAtom (SubAtom a) = a;;

let toSubAtom drs var =
                let dico = dico drs in 
                match var with
                | Var s     as v  -> let res = getItemsByVarWithoutModifier dico v drs in
                                                        if List.length res > 0 then 
                                                                  SubAtom (List.hd res)
                                                        else SubAtom (Rien)
                | SubAtom e as s  -> s
                | Num e as s      -> s
                | ConstStr e as s -> s ;;


(*let rec dedouble_object (lstinit,res) =
        match lstinit with
        | []  -> res
        | lst -> try let noeud = trouve_relation_noeud lst in
                 (match noeud with
                 | Relation(groupref,memberref)               as relation                     -> print_endline "rel";
                                                                                                print_endline ((stringOfVar groupref)^";"^(stringOfVar memberref));
                                                                                                let source = toSubAtom drs groupref  in
                                                                                                let cible  = toSubAtom drs memberref in
                                                                                                let re     =  Relation     (source, cible) in
                                                                                                dedouble_object ((substract lst [relation]), (relation::(getAtomOfSubAtom source)::(getAtomOfSubAtom cible)::res))
                | _ -> print_endline ("dedouble_object: Cas non trouvé !!!"); res@lst)
        with e -> let e = Printexc.to_string e in
                          print_endline ("dedouble_object: Rien trouvé !! \n"^e); res@lstinit
;;*)




let treefyElement drs elem =
                 match elem with
                 | PredicateIntransitive ( ref , verb, subject, gramnbr    )   as verbe      -> 
                                                                                                let subj = toSubAtom drs subject in
                                                                                                let re   = PredicateIntransitive ( ref , verb, subj, gramnbr) in
                                                                                                print_endline (stringOfVar ref);
                                                                                                re
                 | PredicateTransitive   ( ref , verb, subject, cod, gramnbr ) as verbe  -> 
                                                                                                let subj = toSubAtom drs subject in
                                                                                                let coD  = toSubAtom drs cod in
                                                                                                let re   = PredicateTransitive(ref , verb, subj, coD, gramnbr) in
                                                                                                print_endline (stringOfVar ref);
                                                                                                re

                 | PredicateDiTransitive ( ref ,verb, subject, cod, coi, gramnbr ) as verbe ->  
                                                                                                let subj = toSubAtom drs subject in
                                                                                                let coD  = toSubAtom drs cod in
                                                                                                let coI  = toSubAtom drs coi in
                                                                                                print_endline (stringOfVar ref);
                                                                                                let re   = PredicateDiTransitive(ref , verb,  subj, coD, coI, gramnbr) in
                                                                                                re

                | Modifier_pp(  ref1,  preposition, ref2)  as modifier_pp                    -> let cible = toSubAtom drs ref2 in
                                                                                                let re    = Modifier_pp(ref1, preposition, cible) in
                                                                                                print_endline (stringOfVar ref1);
                                                                                                re

                | Property2Ary(ref,  adjective,  degree, ref2)  as prop2                     -> let cible = toSubAtom drs ref2 in
                                                                                                let re    =  Property2Ary(ref,  adjective,  degree, cible) in
                                                                                                print_endline (stringOfVar ref);
                                                                                                re

                                                                                                
                | Property3Ary(ref,  adjective,  ref2,  degree,  comptarget, ref3) as prop3  -> let cible  = toSubAtom drs ref2 in
                                                                                                let cible2 = toSubAtom drs ref3 in
                                                                                                let re    =  Property3Ary(ref,  adjective, cible, degree, comptarget, cible2) in
                                                                                                print_endline (stringOfVar ref);
                                                                                                re

                | HasPart     (groupref, memberref)  as haspart                              -> let source = toSubAtom drs groupref  in
                                                                                                let cible  = toSubAtom drs memberref in
                                                                                                let re     =  HasPart     (source, cible) in
                                                                                                print_endline ((stringOfVar groupref)^";"^(stringOfVar memberref));
                                                                                                re

                | PartOf(groupref,memberref)               as partof                         -> let source = toSubAtom drs groupref  in
                                                                                                let cible  = toSubAtom drs memberref in
                                                                                                let re     =  PartOf     (source, cible) in
                                                                                                print_endline ((stringOfVar groupref)^";"^(stringOfVar memberref));
                                                                                                re

                | Relation(groupref,memberref)               as relation                     -> let source = toSubAtom drs groupref  in
                                                                                                let cible  = toSubAtom drs memberref in
                                                                                                let re     =  Relation     (source, cible) in
                                                                                                print_endline ((stringOfVar groupref)^";"^(stringOfVar memberref));
                                                                                                re


                                                                                                
                (* étendre trouve_verbe vers étendre "centre" qui matchera des Modifier_pp et Modifier_Adv. Le but en fait est de virer les objets libres.
                 * L'idée générale est de virer les éléments "feuilles". Object est un élément feuille, car n'est pas lié à d'autres variable
                 * Les feuilles :
                         * Object
                         * Named
                         * String
                        // * Query   NONNN !!!
                        // * Modifier_Adv
                         * ---> Donc trouve_verbe, deviens trouve_element_noeud soit :
                                 * PredicateDiTransitive
                                 * PredicateTransitive
                                 * PredicateIntransitive
                                 * Modifier_pp
                                 * Property2Ary
                                 * Property3Ary
                                 * Relation
                                 * HasPart
                                 * PartOf*)
                 | _ as el -> el;;
     

     


let g = FullDRS ([Var "J1"; Var "K1"; Var "L1"],                                                                                              
   [Object (Var "J1", Nom "time", Countable, Na, Greater, Number 2, 13, 8);                                                             
    Object (Var "K1", Nom "day", Countable, Na, Eq, Number 10, 13, 13);
    PredicateTransitive (Var "L1", Verbe "vote", SubAtom (Named "User1"),
     Var "J1", Singular);
    Modifier_pp (Var "L1", Preposition "in", Var "K1");
    Modifier_pp (Var "L1", Preposition "for", SubAtom (Named "User2"))]);;


let getDRSCondition g  = match g with | FullDRS(a,b) -> a,b;;
 
let treefy_drs  drs =
        let variables,conditions = getDRSCondition drs in
        let final                = List.filter (fun e -> not (is_Object e)) conditions in
        variables,conditions, (FullDRS(variables,final));;
(* Le problème c'est que les Property1Ary sont "trouvé" avant les objets*)
(*On prend les conditions du DRS, on lui donne la phrase*)


