

(*let a = ( ("pere",[Var("X",2);Var("Y",2)]), [("fils",[Var("Y",2);Var("X",2)])] );;  
let b = ( ("pere",[Const("j");Const("b")]), ([]:(string * Syntax.term list) list))
let mygoal = [("pere",[Var("X",1);Const("b")])];;*)
 
    
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
and operator = Imply | Equal | Different | Inter | Union | Command | Must | Can | May | Not | Naf

and atomp = Atom of name * term list * int * int(*Les deux positions*)

and term = Variable of string | Const of name | ConstCh of name | Nbr of int | TermAtom of atomp   ;;
    
type toplevel_cmd =
  | Assert of drs
  | Quit;;





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
| PredicateIntransitive of var * verbe * var (*Subject*)
| PredicateTransitive of var * verbe * var * var (*Subject  COD *)
| PredicateDiTransitive of var * verbe * var * var * var (*Subject COD COI*)


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
 (“John is more fond-of Mar y than Bill”) or the object (“John is more fond-of Mar y than of Sue”).
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
(*

let l = lexer "drs([A, B], [object(A, webpage, countable, na, eq, 1)-1/8, predicate(B, be, named('MywebPage'), A)-1/4]).";;
let l = lexer "drs([A, B, C], [object(A, webpage, countable, na, eq, 2)-1/4, relation(B, of, A)-1/5, object(B, name, countable, na, eq, 1)-1/7, predicate(C, be, B, string(toto))-1/8])"

 *)
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
    

let varp2var = function
  | Varp a -> Var a;;

let rec drs_to_fulldrs = function
  | DRS (d,c) -> FullDRS ( List.map varp2var d,  List.map conditionDRS2ConditionFullDRS c)

and  conditionDRS2ConditionFullDRS = function
  | Operatorp1(op,a) -> Operator1 (  op, drs_to_fulldrs a)
  | Operatorp2(op,a,b) -> Operator2 ( op, drs_to_fulldrs a, drs_to_fulldrs b)
  | Atomicp a ->  atom2primitives a

and atom2primitives = function
(*Transformations spécifiques*)
   | Atom("object", [Variable var; Const name; Const countable; Const unittype; Const op; Nbr count],x,y) -> Object(Var var, Nom(name), getClassType countable, getUnitType unittype, getOp op, Number count,x,y)
   | Atom("object", [Variable var; Const name; Const countable; Const unittype; Const op; Const count],x,y) -> Object(Var var, Nom(name), getClassType countable, getUnitType unittype, getOp op, getCount count,x,y)
  |  Atom("object", [Variable var; ConstCh name; Const countable; Const unittype; Const op; Nbr count],x,y) -> Object(Var var, Nom(name), getClassType countable, getUnitType unittype, getOp op, Number count,x,y)
  |  Atom("object", [Variable var; ConstCh name; Const countable; Const unittype; Const op; Const count],x,y) -> Object(Var var, Nom(name), getClassType countable, getUnitType unittype, getOp op, getCount count,x,y)
(*TODO : Vérifier ce cas, le coup de forcer le sujet sur le name, c'est peut être pas général...*)
   | Atom("predicate",[ ref ;  verb; subject; cod ; coi ],x,y) ->  PredicateDiTransitive(atomNamedString2NamedString  ref ,Verbe (extractString verb), atomNamedString2NamedString  subject , atomNamedString2NamedString cod, atomNamedString2NamedString coi)
  |  Atom("predicate",[ ref ;   verb; subject; cod ],x,y) -> PredicateTransitive(atomNamedString2NamedString  ref ,Verbe (extractString verb), atomNamedString2NamedString  subject , atomNamedString2NamedString cod)
  |  Atom("predicate",[ ref ;  verb; subject ;  ],x,y) ->  PredicateIntransitive(atomNamedString2NamedString  ref ,Verbe (extractString verb), atomNamedString2NamedString  subject)
  |  Atom("property",[ref;adjective;degree],x,y) -> Property1Ary (atomNamedString2NamedString ref, Adj (extractString adjective), getDegree (extractString degree))
  |  Atom("property",[ref;adjective;degree;ref2],x,y) -> Property2Ary (atomNamedString2NamedString ref, Adj (extractString adjective), getDegree (extractString degree),atomNamedString2NamedString ref2)
 |  Atom("property",[ref;adjective;degree;ref2;comptarget;ref3],x,y) -> Property3Ary (atomNamedString2NamedString ref, Adj (extractString adjective), atomNamedString2NamedString ref2, getDegree (extractString degree), getCompTarget (extractString comptarget), atomNamedString2NamedString ref3)
  | Atom("relation",[ref1; Const "of";ref2],x,y ) -> Relation(atomNamedString2NamedString ref1, atomNamedString2NamedString ref2)
  | Atom("modifier_adv",[ref; adverb;degree],x,y) -> Modifier_Adv( atomNamedString2NamedString ref, Adv (extractString adverb), getDegree (extractString degree))
  | Atom("modifier_pp", [ref1;preposition;ref2],x,y) -> Modifier_pp (atomNamedString2NamedString ref1, Preposition (extractString preposition), atomNamedString2NamedString ref2)
  | Atom("has_part", [groupref;memberref],x,y) -> HasPart(atomNamedString2NamedString groupref,atomNamedString2NamedString memberref)
  | Atom("query",[ref;questionWord],x,y) -> Query(atomNamedString2NamedString ref, getQuestionWord (extractString questionWord))
  | Atom(a, l,x,y) -> print_endline ("Error with atom :"^(atom2String (Atom(a,l,x,y)))^". Check you're using ACE 6.6"); Rien


 (* |  Atom(;;*)

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
(*TODO : un reduce, qui va unifier les variables instanciées une seule fois*)


