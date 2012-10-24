#require "Extlib";;
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
  | Variable a -> a;; 




let exists_Object      l = List.exists (fun e -> match e with | Object(_, _, _,_,_,_,_,_) -> true | _ -> false ) l;;
let exists_Predicate   l = List.exists (fun e -> match e with 
                                                | PredicateIntransitive (_, _, _,_)     -> true
                                                | PredicateTransitive   (_, _, _,_,_)   -> true
                                                | PredicateDiTransitive (_, _, _,_,_,_) -> true
                                                | _                                     -> false ) 
                                        l;;
let exists_Modifier_pp l = List.exists (fun e -> match e with | Modifier_pp(_, _, _) -> true | _ -> false ) l;;

let paraphrase_phrase_simple liste = "";;

let trouve_verbe lst     = List.find (fun e -> match e with 
                                                | PredicateIntransitive (_, _, _,_)     -> true
                                                | PredicateTransitive   (_, _, _,_,_)   -> true
                                                | PredicateDiTransitive (_, _, _,_,_,_) -> true
                                                | _ -> false )  lst;;





let rec getItemsByVar var = function
  | Object(  ref,  name,  countable,  unittype,  op, count,x,y)         as self -> if ref = var then [self] else []
  | PredicateTransitive( ref, verb,   subject , cod, gramnbr )          as self -> if ref = var then [self] else []
  | PredicateDiTransitive(  ref ,verb,  subject ,  cod,  coi, gramnbr)  as self -> if ref = var then [self] else []
  | PredicateIntransitive(  ref , verb, subject, gramnbr)               as self -> if ref = var then [self] else []
  | Property1Ary (ref,  adjective, degree)                              as self -> if ref = var then [self] else []
  | Property2Ary (ref,  adjective,  degree, ref2)                       as self -> if ref = var then [self] else []
  | Property3Ary (ref,  adjective,  ref2,  degree,  comptarget, ref3)   as self -> if ref = var then [self] else []
  | Relation( ref1,  ref2)                                                      -> [] 
  | Modifier_Adv(  ref, adverb,  degree)                                as self -> if ref = var then [self] else []
  | Modifier_pp ( ref1,  preposition, ref2)                                     -> [] 
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
and findItemsInList lst var = List.find (fun e -> List.length (getItemsByVar var e) > 0) lst;;



let rec remplace_in_list lst res =
        match lst with
        | lst -> let verbe = trouve_verbe lst in
                 (match verbe with
                 | PredicateIntransitive ( ref , verb, (Var s as subject), gramnbr    )        -> let subj = findItemsInList lst subject in 
                                                                                           let re   = PredicateIntransitive ( ref , verb, SubAtom subj, gramnbr) in
                                                                                               remplace_in_list (ExtList.List.remove lst re) (re::res)
                 | PredicateTransitive   ( ref , verb, (Var s as subject), (Var c as cod), gramnbr )  -> let subj = findItemsInList lst subject in
                                                                                           let coD  = findItemsInList lst cod in
                                                                                           let re   = PredicateTransitive(ref , verb, SubAtom subj, SubAtom coD, gramnbr) in
                                                                                                remplace_in_list (ExtList.List.remove lst re) (re::res)

                 | PredicateDiTransitive ( ref ,verb, (Var s as subject), (Var c as cod), (Var d as coi), gramnbr ) ->  
                                                                                                let subj = findItemsInList lst subject in
                                                                                                let coD  = findItemsInList lst cod in
                                                                                                let coI  = findItemsInList lst coi in
                                                                                                let re   = PredicateDiTransitive(ref , verb, SubAtom subj, SubAtom coD, SubAtom coI, gramnbr) in
                                                                                                remplace_in_list (ExtList.List.remove lst re) (re::res)



                 | PredicateIntransitive ( ref , verb,  subject, gramnbr    )  as p         -> remplace_in_list (ExtList.List.remove lst p) (p::res)



                 | PredicateTransitive   ( ref , verb,  subject, (Var c as cod), gramnbr )  -> 
                                                                                           let coD  = findItemsInList lst cod in
                                                                                           let re   = PredicateTransitive(ref , verb, subject,SubAtom coD, gramnbr) in
                                                                                                remplace_in_list (ExtList.List.remove lst re) (re::res)

                 | PredicateTransitive   ( ref , verb, (Var s as subject), cod, gramnbr )  -> 
                                                                                           let coD  = findItemsInList lst cod in
                                                                                           let re   = PredicateTransitive(ref , verb, subject,SubAtom  coD, gramnbr) in
                                                                                                remplace_in_list (ExtList.List.remove lst re) (re::res)
                 | PredicateTransitive   ( ref , verb,  subject, cod, gramnbr )  as p -> 
                                                                                                remplace_in_list (ExtList.List.remove lst p) (p::res)



                                                                                                        

                 | PredicateDiTransitive ( ref ,verb,  subject, (Var c as cod), (Var d as coi), gramnbr ) ->  
                                                                                                let coD  = findItemsInList lst cod in
                                                                                                let coI  = findItemsInList lst coi in
                                                                                                let re   = PredicateDiTransitive(ref , verb, subject, SubAtom coD,SubAtom  coI, gramnbr) in
                                                                                                remplace_in_list (ExtList.List.remove lst re) (re::res)
                 | PredicateDiTransitive ( ref ,verb, (Var s as subject),  cod, (Var d as coi), gramnbr ) ->  
                                                                                                let subj = findItemsInList lst subject in
                                                                                                let coI  = findItemsInList lst coi in
                                                                                                let re   = PredicateDiTransitive(ref , verb, SubAtom subj, cod,SubAtom coI, gramnbr) in
                                                                                                remplace_in_list (ExtList.List.remove lst re) (re::res)
                 | PredicateDiTransitive ( ref ,verb, (Var s as subject), (Var c as cod),  coi, gramnbr ) ->  
                                                                                               let subj = findItemsInList lst subject in
                                                                                                let coD  = findItemsInList lst cod in
                                                                                                let re   = PredicateDiTransitive(ref , verb, SubAtom subj, SubAtom coD,  coi, gramnbr) in
                                                                                                remplace_in_list (ExtList.List.remove lst re) (re::res)
                 | PredicateDiTransitive ( ref ,verb,  subject,  cod, (Var c as coi), gramnbr )    ->  
                                                                                                let coI  = findItemsInList lst coi in
                                                                                                let re   = PredicateDiTransitive(ref , verb, subject, cod, SubAtom coI, gramnbr) in
                                                                                                remplace_in_list (ExtList.List.remove lst re) (re::res)
                 | PredicateDiTransitive ( ref ,verb,  subject,  cod, (Var c as coi), gramnbr )    ->  
                                                                                                let coI  = findItemsInList lst coi in
                                                                                                let re   = PredicateDiTransitive(ref , verb, subject, cod,SubAtom  coI, gramnbr) in
                                                                                                remplace_in_list (ExtList.List.remove lst re) (re::res)  
                 | _ -> failwith "cas non traité")
         | []  -> [];;




let treeize_drs  = 1;;


