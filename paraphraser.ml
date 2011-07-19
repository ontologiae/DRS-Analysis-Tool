open Syntax
open Drsxp

let op2str = function
  | Eq  ->  " " 
  | Geq  ->  "at least " 
  |Greater  ->  "more than" 
  | Leq  ->  "at more " 
  | Less  ->  "less than" 
  | Exactly  ->  "exactly" 
  | NaOp ->  "";;

let num2str = function
  | NotCountable -> " "
  | Number 1 -> "a "
  | Number i -> (string_of_int i)^" ";;

let verb2str = function
  | "be", Plural     -> "are "
  | "be", Singular   -> "is "
  | "be", Partitive  -> "are "
  |  a, Singular     -> a^"s "
  |  a, Plural       ->  a^" " ;;


(*TODO let checkGrammaticalNumber = ();; *)

let rec getItemByVar var = function
  | Object(  ref,  name,  countable,  unittype,  op, count,x,y)         as self -> if ref = var then [self] else [Rien]
  | PredicateTransitive( ref, verb,   subject , cod, gramnbr )          as self -> if ref = var then [self] else [Rien]
  | PredicateDiTransitive(  ref ,verb,  subject ,  cod,  coi, gramnbr)  as self -> if ref = var then [self] else [Rien]
  | PredicateIntransitive(  ref , verb, subject, gramnbr)               as self -> if ref = var then [self] else [Rien]
  | Property1Ary (ref,  adjective, degree)                              as self -> if ref = var then [self] else [Rien]
  | Property2Ary (ref,  adjective,  degree, ref2)                       as self -> if ref = var then [self] else [Rien]
  | Property3Ary (ref,  adjective,  ref2,  degree,  comptarget, ref3)   as self -> if ref = var then [self] else [Rien]
  | Relation( ref1,  ref2)                                                      -> [Rien] 
  | Modifier_Adv(  ref, adverb,  degree)                                as self -> if ref = var then [self] else [Rien]
  | Modifier_pp ( ref1,  preposition, ref2)                                     -> [Rien] 
  | HasPart( groupref, memberref)                                               -> [Rien] 
  | Query( ref,  questionWord)                                          as self -> if ref = var then [self] else [Rien]
  | Operator2 (op, b, c)                                                        -> List.append (getItemsByVarIntoDRS var b) (getItemsByVarIntoDRS var c)
  | Operator1 (op, b)                                                           -> (getItemsByVarIntoDRS var b)
  | String a                                                                    -> [Rien] 
  | Named  a                                                                    -> [Rien] 
  | PartOf(a,b)                                                                 -> [Rien] 
  | Rien                                                                        -> [Rien]

and getItemsByVarIntoDRS var drs = match drs with
  | FullDRS (a,b) -> List.filter (fun a -> if a = Rien then false else true) (List.flatten (List.map (getItemByVar var) b)) 
and getItemByVarIntoDRS var drs = List.hd (getItemsByVarIntoDRS var drs);;

let rec checkIfPredicateSingularOrPlural drs = function
  | PredicateTransitive  (  ref ,  verb,   subject, cod, gramnbr)       -> checkIfPredicateSingularOrPlural drs (getItemByVarIntoDRS subject drs)
  | PredicateDiTransitive(  ref ,  verb,   subject, cod, coi, gramnbr)  -> checkIfPredicateSingularOrPlural drs (getItemByVarIntoDRS subject drs)
  | PredicateIntransitive(  ref ,  verb,   subject, gramnbr   )         -> checkIfPredicateSingularOrPlural drs (getItemByVarIntoDRS subject drs)
  | Object(  ref,  name,  countable,  unittype,  op, count,x,y)         ->  (match count with
      | NotCountable -> Singular
      | Number 1 ->  Singular 
      | _ -> Plural)
  | _ -> Singular ;;




let rec paraphraser drs = function
  | Object( Var a, Nom name,  countable,  unittype,  op, count,x,y) , true            ->  "There is "^(op2str op)^(num2str count)^name^" "^a
  | Object( Var a, Nom name,  countable,  unittype,  op, count,x,y) , false           ->  (op2str op)^(num2str count)^name^" "
  | PredicateTransitive(Var ref, Verbe verbe, Var  subject , cod, gramnbr) as verb, t -> (paraphraseForVar drs (Var subject))^" "^(verb2str (verbe,(checkIfPredicateSingularOrPlural drs verb)))^(paraphraseForVar drs cod )^" "
  | PredicateIntransitive(Var ref, Verbe verbe, Var  subject , gramnbr) as verb, t -> (paraphraseForVar drs (Var subject))^" "^(verb2str (verbe,(checkIfPredicateSingularOrPlural drs verb)))^" "
  | PredicateDiTransitive(Var ref, Verbe verbe, Var  subject , cod, coi, gramnbr) as verb, t -> (paraphraseForVar drs (Var subject))^" "^(verb2str (verbe,(checkIfPredicateSingularOrPlural drs verb)))^(paraphraseForVar drs cod )^" to"^(paraphraseForVar drs coi )
and paraphraseForVar drs v = (paraphraser drs ((getItemByVarIntoDRS v drs), false));; 
 
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
paraphraser l  ((PredicateDiTransitive (Var "D", Verbe "give", Var "A", Var "B", Var "C",Singular)),false);; 
