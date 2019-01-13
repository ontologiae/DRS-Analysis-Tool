open Batteries
module L = BatList;;
module H = BatHashtbl;;
module O = BatOption;;
module S = BatString;;

Printexc.record_backtrace true;;


let rec comb m lst =
  match m, lst with
    0, _ -> [[]]
  | _, [] -> []
  | m, x :: xs -> List.map (fun y -> x :: y) (comb (pred m) xs) @
                  comb m xs
;;

let rec intersect m = function
| []                     -> []
| t::q when List.mem t m -> t :: (intersect (L.remove m t) q)
| t :: q                 -> intersect m q;;

let rec difference  m1 m2 = match m1 with
| []                                                                                    ->[]
| t::q when List.mem t m2 -> difference q (L.remove m2 t)
| t::q                                                                          -> t::(difference  q m2);;

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


let getVarsAccordingToRule atom rul =
        let addIfRightPos var  isOktoAdd =
                if isOktoAdd then Some(var) else None in
        let defvar, alterateur, param1, param2, param3 = rul in
        let toto = not alterateur in (*Pourforecr le type de alterateur*)
        let lst =
        match atom with
        | Object( ref,  name,  countable,  unittype,  op, count,_,x,y)                          -> [addIfRightPos ref defvar]
        | PredicateTransitive( ref, verb, _,  Var subject , Var cod, gramnbr )                  -> [addIfRightPos ref defvar; addIfRightPos (Var subject) param1 ; addIfRightPos (Var cod) param2]
        | PredicateTransitive( ref, verb, _,  _ , Var cod, gramnbr )                            -> [addIfRightPos ref defvar; addIfRightPos (Var cod) param2]
        | PredicateTransitive( ref, verb, _,  Var subject , _, gramnbr )                        -> [addIfRightPos ref defvar; addIfRightPos (Var subject) param1]
        | PredicateTransitive( ref, verb, _,  _ , _, gramnbr )                                  -> [addIfRightPos ref defvar]

        | PredicateDiTransitive(  ref ,verb,_,  Var subject , Var cod, Var coi, gramnbr)        -> [addIfRightPos ref defvar; addIfRightPos (Var subject) param1; addIfRightPos (Var cod) param2; addIfRightPos (Var coi) param3]
        | PredicateDiTransitive(  ref ,verb,_,  Var subject , Var cod, _, gramnbr)              -> [addIfRightPos ref defvar; addIfRightPos (Var subject) param1; addIfRightPos (Var cod) param2]
        | PredicateDiTransitive(  ref ,verb,_,  Var subject , _, Var coi, gramnbr)              -> [addIfRightPos ref defvar; addIfRightPos (Var subject) param1; addIfRightPos (Var coi) param3]
        | PredicateDiTransitive(  ref ,verb,_,  Var subject , _, _, gramnbr)                    -> [addIfRightPos ref defvar; addIfRightPos (Var subject) param1; ]
        
        | PredicateDiTransitive(  ref ,verb,_,  _ , Var cod, Var coi, gramnbr)                  -> [addIfRightPos ref defvar; addIfRightPos (Var cod) param2; addIfRightPos (Var coi) param3]
        | PredicateDiTransitive(  ref ,verb,_,  _ , _, Var coi, gramnbr)                        -> [addIfRightPos ref defvar; addIfRightPos (Var coi) param3]
        | PredicateDiTransitive(  ref ,verb,_,  _ , Var cod, _, gramnbr)                        -> [addIfRightPos ref defvar; addIfRightPos (Var cod) param2]
        | PredicateDiTransitive(  ref ,verb,_,  _ , _, _, gramnbr)                              -> [addIfRightPos ref defvar]
        
        | PredicateIntransitive(  ref , verb,_, Var subject, gramnbr)                           -> [addIfRightPos ref defvar; addIfRightPos (Var subject) param1]
        | PredicateIntransitive(  ref , verb,_, _, gramnbr)                                     -> [addIfRightPos ref defvar]
        | Property1Ary (ref,  adjective, degree)                                                -> [addIfRightPos ref defvar]
        | Property2Ary (ref,  adjective,  degree, Var ref2)                                     -> [addIfRightPos ref defvar; addIfRightPos (Var ref2) param1]
        | Property2Ary (ref,  adjective,  degree, ref2)                                         -> [addIfRightPos ref defvar; addIfRightPos ref2 param1]
        
        | Property3Ary (ref,  adjective,  Var ref2,  degree,  comptarget, Var ref3)             -> [addIfRightPos ref defvar; addIfRightPos (Var ref2) param1;addIfRightPos (Var ref3) param2]
        | Property3Ary (ref,  adjective,  Var ref2,  degree,  comptarget,  ref3)                -> [addIfRightPos ref defvar; addIfRightPos (Var ref2) param1; addIfRightPos ref3 param2]
        | Property3Ary (ref,  adjective,   ref2,  degree,  comptarget, Var ref3)                -> [addIfRightPos ref defvar; addIfRightPos (Var ref3) param2]
        | Property3Ary (ref,  adjective,  ref2,  degree,  comptarget,  ref3)                    -> [addIfRightPos ref defvar]
        
        
        
        | Relation( ref1, Var ref2)                                                             -> [addIfRightPos  ref1 param1;addIfRightPos (Var ref2) param2 ]
        | Relation( ref1,  ref2)                                                                -> [addIfRightPos  ref1 param1]        
        | Modifier_Adv(  ref, adverb,  degree)                                                  -> [addIfRightPos ref defvar]
        | Modifier_pp ( ref1,  preposition, Var ref2)                                           -> [addIfRightPos ref1 defvar; addIfRightPos (Var ref2) param1]
        | HasPart( groupref, Var memberref)                                                     -> [addIfRightPos groupref defvar; addIfRightPos (Var memberref) param1]
        | Query( ref,  questionWord)                                                            -> [addIfRightPos ref defvar]
        | Operator2 (op, b, c)                                                                  -> [] (*TODO : gérer ces cas*)
        | Operator1 (op, b)                                                                     -> []
        | String a                                                                              -> []
        | Named  a                                                                              -> []
        | SubDrs(a, dr)                                                                         -> [addIfRightPos (Var a) defvar]
        | Rien                                                                                  -> [] in
        L.filter_map (fun x -> x) lst
        ;;

(*TODO : une fois la liste terminée, filtrer les None et transformer les Some en Var*)



let replaceAtominList lst atom =
                let varElemToReplace    = getVarsAccordingToRule atom (true,false,false,false,false) |> L.hd in (*Variable de l'atom à supprimer*)
                let isVarSearched at v  = try getVarsAccordingToRule at (true,false,false,false,false) |> L.hd  = v with e -> false in (*Renvoi vrai si on trouve la même var*)
                let lstFiltered         = L.filter (fun e -> not (isVarSearched e varElemToReplace)) lst in
                atom::lstFiltered





let resolv2Terme atomDest atomSrc rulDest rulSrc =
        let varsAtom1 = getVarsAccordingToRule atomDest rulDest in
        let varsAtom2 = getVarsAccordingToRule atomSrc rulSrc in
        let varLiees = intersect varsAtom1 varsAtom2 |> L.unique in
        if L.length varLiees > 0 then
                let defvar, alterateur, param1, param2, param3 = rulDest in
                if  L.length varLiees > 1 then Printf.eprintf "Attention !! 2 var Liées %s \n" (L.map stringOfVar varLiees |> S.concat "; ");
                let varToReplace = L.hd varLiees in (*Normalement il n'y en a qu'une !!!*)
                let replaceIfVar varlocal =
                        if varlocal = varToReplace then SubAtom(atomSrc) else varlocal in
                (*On met atomDest dans atomSrc*)
                let termeSubst = 
                        match atomDest with
                        | Object(  ref,  name,  countable,  unittype,  op, count,l,x,y)          -> Object(  ref,  name,  countable,  unittype,  op, count,(if alterateur then atomSrc::l else l),x,y)
                        | PredicateTransitive( ref, verb, l,  subject , cod, gramnbr )           -> PredicateTransitive( ref, verb, (if alterateur then atomSrc::l else l), replaceIfVar  subject ,replaceIfVar cod, gramnbr )
                        | PredicateDiTransitive(  ref ,verb,l,  subject ,  cod,  coi, gramnbr)   -> PredicateDiTransitive(  ref ,verb,(if alterateur then atomSrc::l else l),replaceIfVar subject ,replaceIfVar cod,replaceIfVar coi, gramnbr)
                        | PredicateIntransitive(  ref , verb,l, subject, gramnbr)                -> PredicateIntransitive(  ref , verb,(if alterateur then atomSrc::l else l),replaceIfVar subject, gramnbr)
                        | Property1Ary (ref,  adjective, degree)                                 -> Property1Ary (ref,  adjective, degree)
                        | Property2Ary (ref,  adjective,  degree, ref2)                          -> Property2Ary (ref,  adjective,  degree,replaceIfVar ref2)
                        | Property3Ary (ref,  adjective,  ref2,  degree,  comptarget, ref3)     ->  Property3Ary (ref,  adjective, replaceIfVar  ref2,  degree,  comptarget,replaceIfVar ref3)
                        | Relation( ref1,  ref2)                                                ->  Relation( replaceIfVar ref1, replaceIfVar ref2)
                        | Modifier_Adv(  ref, adverb,  degree)                                  ->  Modifier_Adv(  ref, adverb,  degree)
                        | Modifier_pp ( ref1,  preposition, ref2)                               ->  Modifier_pp ( ref1,  preposition,replaceIfVar ref2)
                        | HasPart( groupref, memberref)                                         ->  HasPart( groupref, replaceIfVar memberref)
                        | Query( ref,  questionWord)                                            ->  Query(replaceIfVar ref,  questionWord)
                        |      _                                                                ->  atomDest in
                [termeSubst]
        else [atomDest;atomSrc] ;;



let getCouples fctest1 fctest2 l = 
        let listePropre = L.filter (fun e -> fctest2 e || fctest1 e) l in
        let couplesBrut = L.cartesian_product listePropre listePropre in
        let couplesok   = L.filter (fun (src,dest) -> fctest1 src && fctest2 dest) couplesBrut in
        couplesok;;



let rec reglex fctest1 fctest2 ruleDest ruleSrc l =
        let couples = getCouples fctest1 fctest2  l in
        let resolutionBrutCouples = L.map (fun (src,dest) -> resolv2Terme dest src  ruleDest ruleSrc) couples in
        let resolutionOk = L.filter (fun e -> L.length e = 1 ) resolutionBrutCouples |> L.flatten in
        (L.fold_left replaceAtominList l resolutionOk)
        (*TODO supprimer les couples résolus en supprimant le atomSrc
          On ne remplace que les termes qu'on a modifiés, on ne supprime pas la source pour le moment. On le fera en cherchant les variables indépendantes*)

        
let rul1src, rul1dest = (true, false, false, false, false), (true, true, false, false, false)
let rul2src, rul2dest = (true, false, false, false, false), (true, true, false, false, false)
let rul3src, rul3dest = (true, false, false, false, false), (false, false, true, false, false)

let rul8src, rul8dest = (true, false, false, false, false), (false, false, true, true, true)

let regle1 l = reglex is_Property1 is_Object  rul1dest rul1src l
let regle2 l = reglex is_Modifier_Adv is_Predicate rul2dest rul2src l
(*let regle3 l = reglex l is_Property2 is_Object rul3dest rul3src l
let regle4 l = reglex l is_Modifier_pp is_Object rul3dest rul3src l

let regle8 l = reglex is_Object is_Predicate rul8dest rul8src l;;*)


(* TODO : les is_\w+ en param de reglex doivent UNIQUEMENT servir à déterminer le terme dest et le terme src.
 *              On aura ensuite deux rule 1 et 2 qui seront des 5-uplets de booléen disant comment se résous la règle
 *
 *              getCouples : filtrage dans la liste à partir des 2 fonctions données, 1er et 2ième en fonction de la func1 et func2, dans l'ordre
 *              On se retrouve avec plein de couples potentiels. Reste à trouver les bons
 *              Ensuite, on applique la règle avec resolv2Terme*)


(*
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
*)



(*TODO fair eune fonction à qui on donne la var à remplacer, l'atomSrc à mettre, la source, et qui renvoi id si c'est la var, et SubAtom sinon*)


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
