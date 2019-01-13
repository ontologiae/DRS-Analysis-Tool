#require "batteries";;

#load "parser.cmo";;
#load "_build/message.cmo";;
#load "_build/lexer.cmo";;
#load "_build/syntax.cmo";;
#load "_build/drsxp.cmo";;
#load "paraphraser.cmo";;

(** Error messages. *)

open Lexing
exception Fatal_error of string

(** [lexer_from_channel fn ch] returns a lexer stream which takes
    input from channel [ch]. The filename (for reporting errors) is
    set to [fn].
*)
let lexer_from_channel fn ch =
  let lex = Lexing.from_channel ch in
  let pos = lex.lex_curr_p in
    lex.lex_curr_p <- { pos with pos_fname = fn; pos_lnum = 1; } ;
    lex

(** [lexer_from_string str] returns a lexer stream which takes input
    from a string [str]. The filename (for reporting errors) is set to
    [""]. *)
let lexer_from_string str =
  let lex = Lexing.from_string str in
  let pos = lex.lex_curr_p in
    lex.lex_curr_p <- { pos with pos_fname = ""; pos_lnum = 1; } ;
    lex

let string_of_position {pos_fname=fn; pos_lnum=ln; pos_bol=bol; pos_cnum=cn} =
  let c = cn - bol in
    if fn = "" then
      "Character " ^ string_of_int c
    else
      "File \"" ^ fn ^ "\", line " ^ string_of_int ln ^ ", character " ^ string_of_int c

let string_of msg pos = string_of_position pos ^ ":\n" ^ msg

let syntax_error {lex_curr_p=pos} = string_of "Syntax error" pos

let report = print_endline;;

(*Parsing.set_trace true;;*)

let lexer = function str ->  lexer_from_string str;;
let fatal_error msg = raise (Fatal_error msg);;

let cmds lex =
	  try
	    Parser.drstart Lexer.token lex
	  with
	    | Failure("lexing: empty token")
	    | Parsing.Parse_error -> fatal_error (syntax_error lex);;


let parse = function str -> cmds (lexer str);;

let parsecomplete str = 
        let drs = Syntax.drs_to_fulldrs ( parse str) in
        drs(* , Syntax.treefy_drs drs*);;


open Syntax;;
(*#trace treefyElem;;
let h = BatHashtbl.create 789;;
let drs = parsecomplete*)

let drs = parsecomplete
"drs([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1,G1,H1],[object(A,webpage,countable,na,eq,1)-1/6,object(B,textfield,countable,na,eq,1)-1/12,object(C,label,countable,na,eq,1)-1/16,relation(C,of,B)-1/13,predicate(D,be,C,string('First name'))-1/17,object(E,id,countable,na,eq,1)-1/21,relation(E,of,B)-1/20,predicate(F,be,E,named('Fname'))-1/22,predicate(G,contain,A,B)-1/8,predicate(H,be,named('MyWebpage'),A)-1/2,object(I,textfield,countable,na,eq,1)-2/6,object(J,label,countable,na,eq,1)-2/10,relation(J,of,I)-2/7,predicate(K,be,J,string('Last name'))-2/11,object(L,id,countable,na,eq,1)-2/15,relation(L,of,I)-2/14,predicate(M,be,L,named('Lname'))-2/16,predicate(N,contain,named('MyWebpage'),I)-2/2,object(O,weblist,countable,na,eq,1)-3/6,object(P,id,countable,na,eq,1)-3/8,relation(P,of,O)-3/7,predicate(Q,be,P,named('NamesWebList'))-3/9,predicate(R,contain,named('MyWebpage'),O)-3/2,object(S,stringformat,countable,na,eq,1)-4/6,predicate(T,'is-defined-by',S,string('Hello, %s %s'))-4/10,object(U,parameters,countable,na,eq,2)-4/18,predicate(V,have,S,U)-4/14,predicate(W,be,named('FullName'),S)-4/2,object(X,parameters,countable,na,geq,2)-5/7,relation(X,of,named('FullName'))-5/4,object(A1,na,countable,na,eq,2)-5/'',has_part(A1,named('Lname'))-5/'',has_part(A1,named('Fname'))-5/'',relation(Z,of,A1)-5/11,object(Z,value,countable,na,geq,2)-5/10,predicate(Y,be,X,Z)-5/8,predicate(B1,display,named('Showname'),named('FullName'))-5/2,object(C1,button,countable,na,eq,1)-6/4,object(D1,label,countable,na,eq,1)-6/8,relation(D1,of,C1)-6/5,predicate(E1,be,D1,string('Submit'))-6/9,object(F1,id,countable,na,eq,1)-6/13,relation(F1,of,C1)-6/12,predicate(G1,be,F1,named('SubmitName'))-6/14,predicate(H1,contain,named('MyWebpage'),C1)-6/2,=&gt;(drs([],[v(drs([I1,J1,K1],[relation(I1,of,named('Fname'))-7/3,object(I1,value,countable,na,eq,1)-7/5,property(J1,empty,pos)-7/7,predicate(K1,be,I1,J1)-7/6]),drs([L1,M1,N1,O1,P1],[relation(L1,of,named('Lname'))-7/10,object(L1,value,countable,na,eq,1)-7/12,property(M1,empty,pos)-7/14,predicate(N1,be,L1,M1)-7/13,object(O1,user,countable,na,eq,1)-7/17,predicate(P1,clicks,O1)-7/20,modifier_pp(P1,on,named('SubmitName'))-7/21]))]),drs([Q1,R1,S1,T1],[object(Q1,message,countable,na,eq,1)-7/29,object(R1,text,countable,na,eq,1)-7/31,relation(R1,of,Q1)-7/30,predicate(S1,be,R1,string('One field is empty'))-7/32,predicate(T1,alert,named('MyWebpage'),Q1)-7/27])),=&gt;(drs([U1,V1,W1,X1],[relation(U1,of,named('Fname'))-8/3,object(U1,value,countable,na,eq,1)-8/5,-(drs([Y1,Z1],[property(Y1,empty,pos)-8/8,predicate(Z1,be,U1,Y1)-8/6])),relation(V1,of,named('Lname'))-8/11,object(V1,value,countable,na,eq,1)-8/13,-(drs([A2,B2],[property(A2,empty,pos)-8/16,predicate(B2,be,V1,A2)-8/14])),object(W1,user,countable,na,eq,1)-8/19,predicate(X1,clicks,W1)-8/22,modifier_pp(X1,on,named('SubmitName'))-8/23]),drs([C2,D2,E2,F2,G2,H2],[predicate(C2,enables,named('MyWebpage'),named('Showname'))-8/29,object(D2,line,countable,na,eq,1)-8/39,object(E2,parameters,countable,na,geq,2)-8/47,relation(E2,of,named('FullName'))-8/44,has_part(G2,named('Fname'))-8/'',predicate(F2,be,E2,G2)-8/48,modifier_pp(F2,on,named('NamesWebList'))-8/52,has_part(G2,named('Lname'))-8/'',object(G2,na,countable,na,eq,2)-8/'',predicate(H2,add,named('MyWebpage'),D2,named('NamesWebList'))-8/35,modifier_pp(H2,with,named('FullName'))-8/42]))]).";;

(*let h = Syntax.makeHashDomain h drs;;*)
let ld = match drs with FullDRS(_,ll) -> ll;;
let ld2 = L.filter (fun e -> is_alterateur e || is_Predicate e || is_Object e) ld;;
#print_length 10000;;
 open Paraphraser;;










let genereVar () =
        let lettre () = 65 + BatRandom.int 26 |> char_of_int in
        let gen _ = S.make 1 (lettre()) in
          S.concat "" (Array.to_list (Array.init 3 gen));;





let addNode typ defVar propList =
        let n = genereVar() |> S.lowercase in
        let makeProp c v = Printf.sprintf "%s.%s = '%s'" n c v in        
        match L.length propList with
        | 0 -> Printf.sprintf "Create (%s : %s {var : '%s'});" n typ defVar
        | _ -> Printf.sprintf "Create (%s : %s {var : '%s'}) set %s;" n typ defVar (L.map (fun (a,b) -> makeProp a b) propList |> S.concat ", " );;

let connect2Nodes ref1 ref2  nomLien properties =
        let n1 = genereVar() |> S.lowercase in
        let n2 = genereVar() |> S.lowercase in
        let findNode ref1  ref2 = Printf.sprintf "match (%s1 {var : '%s'}) Merge (%s2 {var : '%s'})" n1 ref1 n2 ref2 in
        let makeProp c v = Printf.sprintf "%s : '%s'" c v in
        match L.length properties with
        | 0 -> Printf.sprintf "%s Merge (%s1)-[:%s]->(%s2);" (findNode ref1  ref2) n1 nomLien n2
        | _ -> Printf.sprintf "%s Merge (%s1)-[:%s {%s}]->(%s2);" (findNode ref1  ref2) n1 nomLien (L.map (fun (a,b) -> makeProp a b) properties |> S.concat "," ) n2 ;;

let addPropToNode ref propList =
        let n1 = genereVar() |> S.lowercase in
        let findNode ref1  = Printf.sprintf "Merge (%s1 {var : '%s'}) " n1 ref1  in
        let makeProp c v = Printf.sprintf "%s1.%s = '%s'" n1 c v in
        match L.length propList with
        | 0 -> Printf.sprintf "" 
        | _ -> Printf.sprintf "%s on match set %s;" (findNode ref)  (L.map (fun (a,b) -> makeProp a b) propList |> S.concat ", " );;



let createNodeLinkedTo typ defVar propList refSrc nomLien properties =
        let s1 = addNode typ defVar propList in
        [s1;connect2Nodes refSrc defVar nomLien properties];;





let fromClassType = function
  | Dom -> "dom"
  | Countable -> "countable"
  | Mass -> "mass"

let fromOp = function
  | Eq -> "eq" 
  | Geq -> "geq" 
  | Greater -> "greater" 
  | Leq -> "leq" 
  | Less -> "less" 
  | Exactly -> "exactly" 
  | NaOp -> "na"

let fromUnitType = function
  | Na -> "na"
  | Kg -> "kg"
  | Meter -> "meter"

let fromDegree = function
  | Pos -> "pos"
  | Pos_as -> "pos_as"
  | Comp -> "comp"
  | Comp_than -> "comp_than"
  | Sup -> "sup"

let fromCompTarget = function
  | Subj -> "subj"
  | Obj -> "obj"

let fromQuestionWord = function
  | Who -> "who"
  | When -> "when"
  | Where -> "where"
  | Which -> "which"
  | How -> "how"
  | What -> "what"


let fromOperatorBin = function
 | Imply -> "Imply"
 | Equal -> "Equal"
 | Different -> "Different"
 | Inter -> "Inter"
 | Union -> "Union"
 |  Must -> "Must"
 | Can -> "Can"
 | May -> "May"
 | Not -> "Not"
 | Naf -> "Naf"
 | Rule -> "Rule"
 | Definition -> "Definition"
 | Command -> "Command"
 | Query -> "Query"
 | Fact -> "Fact"
 | Question -> "Question"
  


      
let rec genereCypherFullDRS var label = function
        | FullDRS (a,b) -> let va = genereVar() in (addNode "FullDRS" va [])::(connect2Nodes var va label [])::(List.map (genereCypherTreeElem [] [va] ) b |> L.flatten)
and genereCypherTreeElem lst lstToCOnnect e = 
 let subPred s src pos l=
  match s with
  | Var a -> [connect2Nodes src a pos l]
  | ConstStr a -> createNodeLinkedTo "Const" (genereVar()) ["content",a] src pos l
  | Num a -> createNodeLinkedTo "Num" (genereVar()) ["content", (string_of_int a)] src pos l
  | SubAtom (Named n) -> createNodeLinkedTo "Named" (genereVar()) ["content",n] src pos l
  | SubAtom (String n) -> createNodeLinkedTo "String" (genereVar()) ["content",n] src pos l
  | List a ->  createNodeLinkedTo "StringList" (genereVar()) ["content",S.concat ", " a] src pos l 
  | _ -> [] in
 let gramNumber g =
  match g with
  | Singular  -> "gramNumber","Singular"
  | Plural    -> "gramNumber","Plural" 
  | Partitive -> "gramNumber","Partitive" in

 match e with
  | Object(  Var ref,  Nom name,  countable,  unittype,  op, Number count,_,x,y)    -> (addNode "Object" ref ["content",name; (fromClassType countable),"true";
                                                                                                             (fromUnitType unittype),"true"; (fromOp op),"true"; 
                                                                                                             "count",(count |> string_of_int)])::
                                                                                                             (L.map (fun e -> connect2Nodes e ref "SubDRS"[]) lstToCOnnect )
                                                                                                             @lst
  | PredicateTransitive( Var ref, Verbe verb, _,  subject , cod, gramnbr )          -> (addNode "PredicateT"  ref ["Verbe",verb; gramNumber gramnbr])::(subPred subject ref "SUBJECT" [])
                                                                                       @(subPred cod ref "COD" [])@(L.map (fun e -> connect2Nodes e ref  "SubDRS"[]) lstToCOnnect )
                                                                                       @lst
  | PredicateDiTransitive(  Var ref ,Verbe verb,_,  subject ,  cod,  coi, gramnbr)  -> (addNode "Predicate2T"  ref ["Verbe",verb; gramNumber gramnbr])::(subPred subject ref "SUBJECT" [])
                                                                                       @(L.map (fun e -> connect2Nodes e ref  "SubDRS"[]) lstToCOnnect )
                                                                                       @(subPred cod ref "COD" [])@(subPred coi ref "COI" [])@lst
  | PredicateIntransitive( Var ref , Verbe verb,_, subject, gramnbr ) -> (addNode "PredicateIT"  ref ["Verbe",verb;gramNumber gramnbr])::(subPred subject ref "SUBJECT" [])
                                                                         @(L.map (fun e -> connect2Nodes e ref "SubDRS"[]) lstToCOnnect )@lst
  | Property1Ary (Var ref,  Adj adjective, degree)                        -> (addNode "Property1Ary" ref ["Adjective", adjective; "Degree", fromDegree degree])::lst
  | Property2Ary (ref,  adjective,  degree, ref2)                     -> lst (*TODO*)
  | Property3Ary (ref,  adjective,  ref2,  degree,  comptarget, ref3) -> lst (*TODO*)
  | Relation( Var ref1, Var ref2)                                     -> (subPred (Var ref1) ref2 "RELATION" [])@(L.map (fun e -> connect2Nodes e ref1  "SubDRS"[]) lstToCOnnect )@lst
  | Relation( Var ref1, SubAtom(Named(n)))                            -> let va = genereVar() in
                                                                         (addNode "Named" va ["Named",n])::
                                                                         (subPred (Var ref1) va "RELATION" [])@(L.map (fun e -> connect2Nodes e ref1  "SubDRS"[]) lstToCOnnect )@lst


  | Modifier_Adv( Var ref, Adv adverb,  degree)                       -> (addPropToNode ref ["Adverb",adverb;"Degree", fromDegree degree])
                                                                         ::(L.map (fun e -> connect2Nodes e ref "SubDRS"[]) lstToCOnnect )@lst
  | Modifier_pp ( Var ref1,  Preposition preposition, ref2)           -> (subPred ref2 ref1 "Modifier_pp" ["Preposition",preposition])
                                                                         @(L.map (fun e -> connect2Nodes e ref1 "SubDRS"[]) lstToCOnnect )@lst
  | HasPart( Var groupref, memberref)                                 -> (subPred memberref groupref "HasPart" [])@(L.map (fun e -> connect2Nodes e groupref  "SubDRS"[]) lstToCOnnect )@lst
  | Query( Var ref,  questionWord)                                    -> (createNodeLinkedTo "Query" (genereVar()) [] ref "Query" [])
                                                                         @(L.map (fun e -> connect2Nodes e ref "SubDRS"[]) lstToCOnnect )@lst

  | Operator2 (op, b, c)                                              -> let var = genereVar() in
                                                                         (addNode "Operator" var ["Operator",fromOperatorBin op])::(genereCypherFullDRS var "Gauche" b)@(genereCypherFullDRS var "Droite" c)
                                                                         @(L.map (fun e -> connect2Nodes e var  "SubDRS"[]) lstToCOnnect )@lst
  | Operator1 (op, b)                                                 -> let var = genereVar() in 
                                                                         (addNode "Operator" var ["Operator",fromOperatorBin op])::(genereCypherFullDRS var "Droiche" b)
                                                                         @(L.map (fun e -> connect2Nodes e var  "SubDRS"[]) lstToCOnnect )@lst

 (* | String a                                                          -> (addNode "String" (genereVar()) ["String",a])::lst
  | Named  a                                                          -> (addNode "String" (genereVar()) ["String",a])::lst*)
  | SubDrs(a, dr)                                                     -> let var = genereVar() in (addNode "SubDRS" var [])::(genereCypherFullDRS var "SubDRS" dr)
                                                                          @(L.map (fun e -> connect2Nodes e var  "SubDRS"[]) lstToCOnnect )@lst
  | _                                                                 -> lst;;
        


















 #trace resolv2Terme;;
 #trace getCouples;;
 #trace getVarsAccordingToRule;;



let tree2 =
  FullDRS
    ([Var "A"; Var "B"; Var "C"; Var "D"; Var "E"; Var "F"; Var "G"; Var "H";
      Var "I"; Var "J"; Var "K"; Var "L"; Var "M"; Var "N"; Var "O"; Var "P";
      Var "Q"; Var "R"; Var "S"; Var "T"; Var "U"; Var "V"; Var "W"; Var "X";
      Var "Y"; Var "Z"; Var "A1"; Var "B1"; Var "C1"; Var "D1"; Var "E1";
      Var "F1"; Var "G1"; Var "H1"; Var "I1"; Var "J1"; Var "K1"; Var "L1";
      Var "M1"; Var "N1"; Var "O1"; Var "P1"; Var "Q1"; Var "R1"; Var "S1";
      Var "T1"; Var "U1"; Var "V1"; Var "W1"; Var "X1"; Var "Y1"; Var "Z1";
      Var "A2"; Var "B2"; Var "C2"; Var "D2"; Var "E2"; Var "F2"; Var "G2";
      Var "H2"; Var "I2"; Var "J2"; Var "K2"; Var "L2"; Var "M2"; Var "N2";
      Var "O2"; Var "P2"; Var "Q2"; Var "R2"; Var "S2"; Var "T2"; Var "U2";
      Var "V2"; Var "W2"; Var "X2"; Var "Y2"; Var "Z2"; Var "A3"; Var "B3";
      Var "C3"; Var "D3"; Var "E3"; Var "F3"; Var "G3"; Var "H3"],
     [Operator1 (Can,
                 FullDRS ([Var "I3"],
                          [PredicateTransitive (Var "I3", Verbe "describe", [],
                                                SubAtom (Named "Attempto-Controlled-English"),
                                                SubAtom (Named "Attempto-Controlled-English"), Singular)]));
      Operator1 (Question,
                 FullDRS ([Var "J3"; Var "K3"; Var "L3"],
                          [Query (Var "J3", Who);
                           Object (Var "K3", Nom "controlled-natural-language", Countable, Na, Eq,
                                   Number 1, [], 2, 4);
                           PredicateTransitive (Var "L3", Verbe "need", [], Var "J3", Var "K3",
                                                Singular)]));
      Object (Var "A", Nom "user", Countable, Na, Geq, Number 2, [], 3, 4);
      PredicateTransitive (Var "B", Verbe "want", [], Var "A", Var "C", Singular);
      SubDrs ("C",
              FullDRS ([Var "M3"; Var "N3"],
                       [Object (Var "M3", Nom "method", Countable, Na, Eq, Number 1, [], 3, 11);
                        Property1Ary (Var "M3", Adj "formal", Pos);
                        PredicateTransitive (Var "N3", Verbe "use", [], Var "A", Var "M3",
                                             Singular)]));
      Operator2 (Imply,
                 FullDRS ([Var "O3"],
                          [Object (Var "O3", Nom "language", Countable, Na, Eq, Number 1, [], 3, 17);
                           Property1Ary (Var "O3", Adj "formal", Pos)]),
                 FullDRS ([],
                          [Operator1 (Not,
                                      FullDRS ([Var "P3"],
                                               [PredicateTransitive (Var "P3", Verbe "understand", [], Var "A",
                                                                     Var "O3", Singular)]))]));
      PredicateTransitive (Var "D", Verbe "prefer", [], Var "A", Var "E", Singular);
      SubDrs ("E",
              FullDRS ([Var "Q3"; Var "R3"; Var "S3"],
                       [Object (Var "Q3", Nom "method", Countable, Na, Eq, Number 1, [], 4, 7);
                        Property1Ary (Var "Q3", Adj "formal", Pos); Relation (Var "R3", Var "T3");
                        Object (Var "R3", Nom "language", Countable, Na, Eq, Number 1, [], 4, 12);
                        Property1Ary (Var "R3", Adj "natural", Pos);
                        PredicateTransitive (Var "S3", Verbe "use", [], Var "A", Var "Q3",
                                             Singular);
                        Modifier_pp (Var "S3", Preposition "in", Var "R3")]));
      Operator2 (Imply,
                 FullDRS ([Var "U3"; Var "V3"; Var "W3"; Var "X3"],
                          [Object (Var "U3", Nom "method", Countable, Na, Eq, Number 1, [], 5, 4);
                           Property1Ary (Var "U3", Adj "formal", Pos);
                           Object (Var "V3", Nom "language", Countable, Na, Eq, Number 1, [], 5, 9);
                           Property1Ary (Var "V3", Adj "natural", Pos);
                           Property2Ary (Var "W3", Adj "expressed-in", Pos, Var "V3");
                           PredicateTransitive (Var "X3", Verbe "be", [], Var "U3", Var "W3",
                                                Singular)]),
                 FullDRS ([],
                          [Operator2 (Imply,
                                      FullDRS ([Var "Y3"; Var "Z3"],
                                               [Object (Var "Y3", Nom "person", Countable, Na, Eq, Number 1, [], 5, 12);
                                                PredicateTransitive (Var "Z3", Verbe "speak", [], Var "Y3", Var "V3",
                                                                     Singular)]),
                                      FullDRS ([],
                                               [Operator1 (Can,
                                                           FullDRS ([Var "A4"],
                                                                    [PredicateTransitive (Var "A4", Verbe "use", [], Var "Y3",
                                                                                          Var "U3", Singular)]))]))]));
      Operator2 (Imply,
                 FullDRS ([Var "B4"],
                          [Object (Var "B4", Nom "controlled-natural-language", Countable, Na, Eq,
                                   Number 1, [], 6, 2)]),
                 FullDRS ([Var "C4"; Var "D4"; Var "E4"],
                          [Object (Var "C4", Nom "subset", Countable, Na, Eq, Number 1, [], 6, 5);
                           Object (Var "D4", Nom "language", Countable, Na, Eq, Number 1, [], 6, 9);
                           Property1Ary (Var "D4", Adj "natural", Pos);
                           Relation (Var "C4", Var "D4");
                           PredicateTransitive (Var "E4", Verbe "be", [], Var "B4", Var "C4",
                                                Singular)]));
      Operator2 (Imply,
                 FullDRS ([Var "F4"],
                          [Object (Var "F4", Nom "controlled-natural-language", Countable, Na, Eq,
                                   Number 1, [], 7, 3);
                           Operator1 (Can,
                                      FullDRS ([Var "G4"; Var "H4"; Var "I4"],
                                               [Object (Var "G4", Nom "language", Countable, Na, Eq, Number 1,
                                                        [], 7, 9);
                                                Property1Ary (Var "G4", Adj "logical", Pos);
                                                Property2Ary (Var "H4", Adj "translated-into", Pos, Var "G4");
                                                PredicateTransitive (Var "I4", Verbe "be", [], Var "F4", Var "H4",
                                                                     Singular)]))]),
                 FullDRS ([Var "J4"; Var "K4"],
                          [Object (Var "J4", Nom "language", Countable, Na, Eq, Number 1, [], 7, 16);
                           Property1Ary (Var "J4", Adj "formal", Pos);
                           Operator1 (Can,
                                      FullDRS ([],
                                               [Operator2 (Imply,
                                                           FullDRS ([Var "L4"; Var "M4"; Var "N4"; Var "O4"],
                                                                    [Object (Var "L4", Nom "user", Countable, Na, Eq, Number 1,
                                                                             [], 7, 24);
                                                                     PredicateTransitive (Var "O4", Verbe "underlie", [], Var "N4",
                                                                                          Var "F4", Singular);
                                                                     Property1Ary (Var "N4", Adj "natural", Pos);
                                                                     Object (Var "N4", Nom "language", Countable, Na, Eq, Number 1,
                                                                             [], 7, 29);
                                                                     Object (Var "F4", Nom "controlled-natural-language", Countable, Na,
                                                                             Eq, Number 1, [], 7, 33);
                                                                     PredicateTransitive (Var "M4", Verbe "speak", [], Var "L4",
                                                                                          Var "N4", Singular)]),
                                                           FullDRS ([Var "P4"],
                                                                    [Modifier_Adv (Var "P4", Adv "easily", Pos);
                                                                     PredicateTransitive (Var "P4", Verbe "understand", [], Var "L4",
                                                                                          Var "J4", Singular)]))]));
                           PredicateTransitive (Var "K4", Verbe "be", [], Var "F4", Var "J4",
                                                Singular)]));
      Operator2 (Imply,
                 FullDRS ([Var "Q4"; Var "R4"],
                          [Object (Var "Q4", Nom "developer", Countable, Na, Eq, Number 1, [], 8, 2);
                           Object (Var "R4", Nom "controlled-natural-language", Countable, Na, Eq,
                                   Number 1, [], 8, 5);
                           Relation (Var "Q4", Var "R4")]),
                 FullDRS ([Var "S4"; Var "T4"],
                          [PredicateTransitive (Var "S4", Verbe "believe", [], Var "Q4", Var "T4",
                                                Singular);
                           SubDrs ("T4",
                                   FullDRS ([],
                                            [Operator1 (Can,
                                                        FullDRS ([Var "U4"; Var "V4"; Var "W4"; Var "X4"; Var "Y4"; Var "Z4"],
                                                                 [Object (Var "Z4", Nom "na", Countable, Na, Eq, Number 2, [], 8, 0);
                                                                  HasPart (Var "Z4", Var "X4"); HasPart (Var "Z4", Var "W4");
                                                                  Property1Ary (Var "W4", Adj "formal", Pos);
                                                                  Relation (Var "V4", Var "Z4");
                                                                  Object (Var "V4", Nom "formality", Countable, Na, Eq, Number 1,
                                                                          [], 8, 12);
                                                                  Object (Var "W4", Nom "language", Countable, Na, Eq, Number 1,
                                                                          [], 8, 16);
                                                                  PredicateTransitive (Var "U4", Verbe "combine", [], Var "R4",
                                                                                       Var "V4", Singular);
                                                                  Relation (Var "X4", Var "Y4");
                                                                  Property1Ary (Var "Y4", Adj "natural", Pos);
                                                                  Object (Var "Y4", Nom "language", Countable, Na, Eq, Number 1,
                                                                          [], 8, 23);
                                                                  Object (Var "X4", Nom "familiarity", Countable, Na, Eq, Number 1,
                                                                          [], 8, 19)]))]))]));
      HasPart (Var "H", SubAtom (Named "PENG"));
      Object (Var "F", Nom "controlled-natural-language", Countable, Na, Eq,
              Number 5, [], 9, 13);
      Property1Ary (Var "F", Adj "important", Pos);
      PredicateTransitive (Var "G", Verbe "be", [], Var "H", Var "F", Singular);
      HasPart (Var "H", SubAtom (Named "Common-Logic-Controlled-English"));
      HasPart (Var "H", SubAtom (Named "CPL"));
      HasPart (Var "H", SubAtom (Named "Rabbit"));
      HasPart (Var "H", SubAtom (Named "Attempto-Controlled-English"));
      Object (Var "H", Nom "na", Countable, Na, Eq, Number 5, [], 9, 0);
      Object (Var "I", Nom "controllnguage", Countable, Na, Eq, Number 1,
              [], 10, 6);
      Object (Var "J", Nom "subset", Countable, Na, Eq, Number 1, [], 10, 10);
      Relation (Var "J", SubAtom (Named "English"));
      PredicateTransitive (Var "K", Verbe "be", [], Var "I", Var "J", Singular);
      Object (Var "L", Nom "research-group", Countable, Na, Eq, Number 1,
              [], 10, 23);
      Relation (Var "L", SubAtom (Named "University-of-Zurich"));
      PredicateTransitive (Var "M", Verbe "be", [],
                           SubAtom (Named "Attempto-team"), Var "L", Singular);
      PredicateTransitive (Var "N", Verbe "develop", [],
                           SubAtom (Named "Attempto-team"), Var "I", Singular);
      PredicateTransitive (Var "O", Verbe "be", [],
                           SubAtom (Named "Attempto-Controlled-English"), Var "I", Singular);
      Object (Var "P", Nom "abbreviation", Countable, Na, Eq, Number 1, [], 11, 4);
      Relation (Var "P", SubAtom (String "Attempto-Controlled-English"));
      PredicateTransitive (Var "Q", Verbe "be", [], SubAtom (String "ACE"),
                           Var "P", Singular);
      Operator1 (Question,
                 FullDRS ([Var "A5"; Var "B5"],
                          [Query (Var "A5", Which);
                           Object (Var "A5", Nom "word", Countable, Na, Geq, Number 2, [], 12, 2);
                           PredicateTransitive (Var "B5", Verbe "support", [],
                                                SubAtom (Named "Attempto-Controlled-English"), Var "A5", Singular)]));
      Relation (Var "Z", SubAtom (Named "Attempto-Controlled-English"));
      Object (Var "Z", Nom "vocabulary", Countable, Na, Eq, Number 1, [], 13, 2);
      HasPart (Var "Y", Var "R");
      PredicateTransitive (Var "U", Verbe "consist-of", [], Var "Z", Var "Y",
                           Singular);
      HasPart (Var "Y", Var "V");
      Object (Var "Y", Nom "na", Countable, Na, Eq, Number 2, [], 13, 0);
      PredicateTransitive (Var "T", Verbe "be", [], Var "R", Var "S", Singular);
      Property1Ary (Var "S", Adj "built-in", Pos);
      Object (Var "R", Nom "function-word", Countable, Na, Geq, Number 2,
              [], 13, 7);
      PredicateTransitive (Var "X", Verbe "define", [], Var "W", Var "V", Singular);
      Object (Var "W", Nom "lexicon", Countable, Na, Eq, Number 1, [], 13, 19);
      Object (Var "V", Nom "content-word", Countable, Na, Geq, Number 2,
              [], 13, 13);
      Object (Var "A1", Nom "function-word", Countable, Na, Greater, Number 80,
              [], 14, 6);
      PredicateTransitive (Var "B1", Verbe "use", [],
                           SubAtom (Named "Attempto-Controlled-English"), Var "A1", Singular);
      Object (Var "C1", Nom "function-word", Countable, Na, Eq, Number 3,
              [], 15, 3);
      Property1Ary (Var "C1", Adj "typical", Pos);
      HasPart (Var "E1", SubAtom (String "every"));
      PredicateTransitive (Var "D1", Verbe "be", [], Var "C1", Var "E1", Singular);
      HasPart (Var "E1", SubAtom (String "or"));
      HasPart (Var "E1", SubAtom (String "is"));
      Object (Var "E1", Nom "na", Countable, Na, Eq, Number 3, [], 15, 0);
      Operator2 (Imply,
                 FullDRS ([Var "C5"],
                          [Object (Var "C5", Nom "content-word", Countable, Na, Eq, Number 1,
                                   [], 16, 2)]),
                 FullDRS ([],
                          [Operator2 (Union,
                                      FullDRS ([Var "D5"; Var "E5"],
                                               [Object (Var "D5", Nom "adverb", Countable, Na, Eq, Number 1, [], 16, 5);
                                                PredicateTransitive (Var "E5", Verbe "be", [], Var "C5", Var "D5",
                                                                     Singular)]),
                                      FullDRS ([],
                                               [Operator2 (Union,
                                                           FullDRS ([Var "F5"; Var "G5"],
                                                                    [Object (Var "F5", Nom "adjective", Countable, Na, Eq, Number 1,
                                                                             [], 16, 9);
                                                                     PredicateTransitive (Var "G5", Verbe "be", [], Var "C5", Var "F5",
                                                                                          Singular)]),
                                                           FullDRS ([],
                                                                    [Operator2 (Union,
                                                                                FullDRS ([Var "H5"; Var "I5"],
                                                                                         [Object (Var "H5", Nom "proper-name", Countable, Na, Eq,
                                                                                                  Number 1, [], 16, 13);
                                                                                          PredicateTransitive (Var "I5", Verbe "be", [], Var "C5",
                                                                                                               Var "H5", Singular)]),
                                                                                FullDRS ([],
                                                                                         [Operator2 (Union,
                                                                                                     FullDRS ([Var "J5"; Var "K5"],
                                                                                                              [Object (Var "J5", Nom "noun", Countable, Na, Eq, Number 1,
                                                                                                                       [], 16, 17);
                                                                                                               PredicateTransitive (Var "K5", Verbe "be", [], Var "C5",
                                                                                                                                    Var "J5", Singular)]),
                                                                                                     FullDRS ([],
                                                                                                              [Operator2 (Union,
                                                                                                                          FullDRS ([Var "L5"; Var "M5"],
                                                                                                                                   [Object (Var "L5", Nom "verb", Countable, Na, Eq,
                                                                                                                                            Number 1, [], 16, 21);
                                                                                                                                    PredicateTransitive (Var "M5", Verbe "be", [], Var "C5",
                                                                                                                                                         Var "L5", Singular)]),
                                                                                                                          FullDRS ([Var "N5"; Var "O5"],
                                                                                                                                   [Object (Var "N5", Nom "preposition", Countable, Na, Eq,
                                                                                                                                            Number 1, [], 16, 25);
                                                                                                                                    PredicateTransitive (Var "O5", Verbe "be", [], Var "C5",
                                                                                                                                                         Var "N5", Singular)]))]))]))]))]))]));
      Object (Var "F1", Nom "lexicon", Countable, Na, Eq, Number 1, [], 17, 4);
      Object (Var "G1", Nom "content-word", Countable, Na, Greater, Number 100000,
              [], 17, 10);
      PredicateTransitive (Var "H1", Verbe "contain", [], Var "F1", Var "G1",
                           Singular);
      PredicateTransitive (Var "I1", Verbe "use", [],
                           SubAtom (Named "Attempto-Controlled-English"), Var "F1", Singular);
      Operator2 (Imply,
                 FullDRS ([Var "P5"],
                          [Object (Var "P5", Nom "user", Countable, Na, Eq, Number 1, [], 18, 2);
                           Relation (Var "P5", SubAtom (Named "Attempto-Controlled-English"))]),
                 FullDRS ([],
                          [Operator1 (Can,
                                      FullDRS ([Var "Q5"; Var "R5"; Var "S5"; Var "T5"],
                                               [Object (Var "Q5", Nom "lexicon", Countable, Na, Eq, Number 1,
                                                        [], 18, 8);
                                                Object (Var "R5", Nom "content-word", Countable, Na, Geq, Number 2,
                                                        [], 18, 13);
                                                Property1Ary (Var "R5", Adj "additional", Pos);
                                                PredicateTransitive (Var "S5", Verbe "contain", [], Var "Q5",
                                                                     Var "R5", Singular);
                                                PredicateTransitive (Var "T5", Verbe "use", [], Var "P5", Var "Q5",
                                                                     Singular)]))]));
      Operator1 (Question,
                 FullDRS ([Var "U5"; Var "V5"; Var "W5"],
                          [Query (Var "U5", What);
                           Relation (Var "W5", SubAtom (Named "Attempto-Controlled-English"));
                           Object (Var "W5", Nom "syntax", Countable, Na, Eq, Number 1, [], 19, 4);
                           PredicateTransitive (Var "V5", Verbe "be", [], Var "U5", Var "W5",
                                                Singular)]));
      Object (Var "J1", Nom "language", Countable, Na, Eq, Number 1, [], 20, 5);
      Property1Ary (Var "J1", Adj "rich", Pos); HasPart (Var "N1", Var "K1");
      Object (Var "K1", Nom "construction-rule", Countable, Na, Less, Number 20,
              [], 20, 13);
      PredicateTransitive (Var "L1", Verbe "describe", [], Var "N1", Var "J1",
                           Singular);
      HasPart (Var "N1", Var "M1");
      Object (Var "M1", Nom "interpretation-rule", Countable, Na, Less, Number 20,
              [], 20, 18);
      Object (Var "N1", Nom "na", Countable, Na, Eq, Number 2, [], 20, 0);
      PredicateTransitive (Var "O1", Verbe "be", [],
                           SubAtom (Named "Attempto-Controlled-English"), Var "J1", Singular);
      Object (Var "R1", Nom "construction-rule", Countable, Na, Geq, Number 2,
              [], 21, 2);
      Relation (Var "Q1", SubAtom (Named "Attempto-Controlled-English"));
      Object (Var "Q1", Nom "syntax", Countable, Na, Eq, Number 1, [], 21, 5);
      PredicateTransitive (Var "P1", Verbe "describe", [], Var "R1", Var "Q1",
                           Singular);
      Object (Var "U1", Nom "interpretation-rule", Countable, Na, Geq, Number 2,
              [], 22, 2);
      Relation (Var "T1", SubAtom (Named "Attempto-Controlled-English"));
      Object (Var "T1", Nom "meaning", Countable, Na, Eq, Number 1, [], 22, 5);
      PredicateTransitive (Var "S1", Verbe "define", [], Var "U1", Var "T1",
                           Singular);
      Operator2 (Imply,
                 FullDRS ([Var "X5"],
                          [Object (Var "X5", Nom "sentence", Countable, Na, Eq, Number 1, [], 23, 3);
                           Property1Ary (Var "X5", Adj "admissible", Pos);
                           Relation (Var "X5", SubAtom (Named "Attempto-Controlled-English"))]),
                 FullDRS ([],
                          [Operator2 (Union,
                                      FullDRS ([Var "Y5"; Var "Z5"],
                                               [Object (Var "Y5", Nom "sentence", Countable, Na, Eq, Number 1,
                                                        [], 23, 9);
                                                Property1Ary (Var "Y5", Adj "declarative", Pos);
                                                PredicateTransitive (Var "Z5", Verbe "be", [], Var "X5", Var "Y5",
                                                                     Singular)]),
                                      FullDRS ([],
                                               [Operator2 (Union,
                                                           FullDRS ([Var "A6"; Var "B6"],
                                                                    [Object (Var "A6", Nom "questionn", Countable, Na, Eq, Number 1,
                                                                             [], 23, 13);
                                                                     PredicateTransitive (Var "B6", Verbe "be", [], Var "X5", Var "A6",
                                                                                          Singular)]),
                                                           FullDRS ([Var "C6"; Var "D6"],
                                                                    [Object (Var "C6", Nom "commandd", Countable, Na, Eq, Number 1,
                                                                             [], 23, 17);
                                                                     PredicateTransitive (Var "D6", Verbe "be", [], Var "X5", Var "C6",
                                                                                          Singular)]))]))]));
      Operator2 (Imply,
                 FullDRS ([Var "E6"],
                          [Object (Var "E6", Nom "sentence", Countable, Na, Eq, Number 1, [], 24, 3);
                           Property1Ary (Var "E6", Adj "declarative", Pos)]),
                 FullDRS ([Var "F6"; Var "G6"],
                          [Object (Var "F6", Nom "period", Countable, Na, Eq, Number 1, [], 24, 6);
                           PredicateTransitive (Var "G6", Verbe "end-with", [], Var "E6", Var "F6",
                                                Singular);
                           Operator2 (Union,
                                      FullDRS ([Var "H6"; Var "I6"],
                                               [Object (Var "H6", Nom "sentence", Countable, Na, Eq, Number 1,
                                                        [], 24, 12);
                                                Property1Ary (Var "H6", Adj "simple", Pos);
                                                PredicateTransitive (Var "I6", Verbe "be", [], Var "E6", Var "H6",
                                                                     Singular)]),
                                      FullDRS ([Var "J6"; Var "K6"],
                                               [Object (Var "J6", Nom "sentence", Countable, Na, Eq, Number 1,
                                                        [], 24, 17);
                                                Property1Ary (Var "J6", Adj "composite", Pos);
                                                PredicateTransitive (Var "K6", Verbe "be", [], Var "E6", Var "J6",
                                                                     Singular)]))]));
      Operator2 (Imply,
                 FullDRS ([Var "L6"],
                          [Object (Var "L6", Nom "sentence", Countable, Na, Eq, Number 1, [], 25, 3);
                           Property1Ary (Var "L6", Adj "simple", Pos)]),
                 FullDRS ([],
                          [Operator2 (Union,
                                      FullDRS ([Var "M6"; Var "N6"; Var "O6"; Var "P6"],
                                               [Object (Var "M6", Nom "noun-phrase", Countable, Na, Eq, Number 1,
                                                        [], 25, 6);
                                                HasPart (Var "P6", Var "M6");
                                                PredicateTransitive (Var "N6", Verbe "consist-of", [], Var "L6",
                                                                     Var "P6", Singular);
                                                Object (Var "O6", Nom "verb-phrase", Countable, Na, Eq, Number 1,
                                                        [], 25, 9);
                                                HasPart (Var "P6", Var "O6");
                                                Object (Var "P6", Nom "na", Countable, Na, Eq, Number 2, [], 25, 0)]),
                                      FullDRS ([Var "Q6"; Var "R6"; Var "S6"],
                                               [HasPart (Var "S6", SubAtom (String "there is/are"));
                                                PredicateTransitive (Var "Q6", Verbe "consist-of", [], Var "L6",
                                                                     Var "S6", Singular);
                                                Object (Var "R6", Nom "noun-phrase", Countable, Na, Eq, Number 1,
                                                        [], 25, 15);
                                                HasPart (Var "S6", Var "R6");
                                                Object (Var "S6", Nom "na", Countable, Na, Eq, Number 2, [], 25, 0)]))]));
      Object (Var "V1", Nom "example", Countable, Na, Eq, Number 1, [], 26, 4);
      Object (Var "W1", Nom "sentence", Countable, Na, Eq, Number 1, [], 26, 8);
      Property1Ary (Var "W1", Adj "simple", Pos); Relation (Var "V1", Var "W1");
      PredicateTransitive (Var "X1", Verbe "be", [],
                           SubAtom (String "John works."), Var "V1", Singular);
      Operator2 (Imply,
                 FullDRS ([Var "T6"],
                          [Object (Var "T6", Nom "sentence", Countable, Na, Eq, Number 1, [], 27, 3);
                           Property1Ary (Var "T6", Adj "composite", Pos)]),
                 FullDRS ([Var "U6"; Var "V6"],
                          [Object (Var "U6", Nom "constructor", Countable, Na, Geq, Number 1,
                                   [], 27, 11);
                           Modifier_Adv (Var "V6", Adv "recursively", Pos);
                           PredicateTransitive (Var "V6", Verbe "form", [], Var "U6", Var "T6",
                                                Singular)]));
      Operator2 (Imply,
                 FullDRS ([Var "W6"],
                          [Object (Var "W6", Nom "constructor", Countable, Na, Eq, Number 1,
                                   [], 28, 2)]),
                 FullDRS ([],
                          [Operator2 (Union,
                                      FullDRS ([Var "X6"; Var "Y6"],
                                               [Object (Var "X6", Nom "coordination", Countable, Na, Eq, Number 1,
                                                        [], 28, 5);
                                                PredicateTransitive (Var "Y6", Verbe "be", [], Var "W6", Var "X6",
                                                                     Singular)]),
                                      FullDRS ([],
                                               [Operator2 (Union,
                                                           FullDRS ([Var "Z6"; Var "A7"],
                                                                    [Object (Var "Z6", Nom "subordination", Countable, Na, Eq,
                                                                             Number 1, [], 28, 9);
                                                                     PredicateTransitive (Var "A7", Verbe "be", [], Var "W6", Var "Z6",
                                                                                          Singular)]),
                                                           FullDRS ([Var "B7"; Var "C7"],
                                                                    [Object (Var "B7", Nom "quantification", Countable, Na, Eq,
                                                                             Number 1, [], 28, 13);
                                                                     PredicateTransitive (Var "C7", Verbe "be", [], Var "W6", Var "B7",
                                                                                          Singular)]))]))]));
      Object (Var "Y1", Nom "example", Countable, Na, Eq, Number 1, [], 29, 4);
      Object (Var "Z1", Nom "sentence", Countable, Na, Eq, Number 1, [], 29, 8);
      Property1Ary (Var "Z1", Adj "composite", Pos); Relation (Var "Y1", Var "Z1");
      PredicateTransitive (Var "A2", Verbe "be", [],
                           SubAtom (String "If John works then Mary does not wait."), Var "Y1",
                           Singular);
      Object (Var "B2", Nom "example", Countable, Na, Eq, Number 1, [], 30, 4);
      Object (Var "C2", Nom "yes-no-question", Countable, Na, Eq, Number 1,
              [], 30, 7);
      Relation (Var "B2", Var "C2");
      PredicateTransitive (Var "D2", Verbe "be", [],
                           SubAtom (String "Does John wait?"), Var "B2", Singular);
      Object (Var "E2", Nom "example", Countable, Na, Eq, Number 1, [], 30, 12);
      Object (Var "F2", Nom "wh-question", Countable, Na, Eq, Number 1, [], 30, 15);
      Relation (Var "E2", Var "F2");
      PredicateTransitive (Var "G2", Verbe "be", [], SubAtom (String "Who waits?"),
                           Var "E2", Singular);
      Operator2 (Imply,
                 FullDRS ([Var "D7"],
                          [Object (Var "D7", Nom "commandd", Countable, Na, Eq, Number 1, [], 31, 2)]),
                 FullDRS ([Var "E7"; Var "F7"; Var "G7"; Var "H7"],
                          [Object (Var "E7", Nom "addressee", Countable, Na, Eq, Number 1, [], 31, 5);
                           PredicateTransitive (Var "F7", Verbe "start-with", [], Var "D7",
                                                Var "E7", Singular);
                           Object (Var "G7", Nom "exclamation-mark", Countable, Na, Eq, Number 1,
                                   [], 31, 9);
                           PredicateTransitive (Var "H7", Verbe "end-with", [], Var "D7", Var "G7",
                                                Singular)]));
      Object (Var "H2", Nom "example", Countable, Na, Eq, Number 1, [], 32, 4);
      Object (Var "I2", Nom "commandd", Countable, Na, Eq, Number 1, [], 32, 7);
      Relation (Var "H2", Var "I2");
      PredicateTransitive (Var "J2", Verbe "be", [],
                           SubAtom (String "John, wait!"), Var "H2", Singular);
      Operator2 (Imply,
                 FullDRS ([Var "I7"],
                          [Object (Var "I7", Nom "reference", Countable, Na, Eq, Number 1, [], 33, 3);
                           Property1Ary (Var "I7", Adj "anaphoric", Pos)]),
                 FullDRS ([],
                          [Operator2 (Union,
                                      FullDRS ([Var "J7"; Var "K7"],
                                               [Object (Var "J7", Nom "noun-phrase", Countable, Na, Eq, Number 1,
                                                        [], 33, 7);
                                                Property1Ary (Var "J7", Adj "definite", Pos);
                                                PredicateTransitive (Var "K7", Verbe "be", [], Var "I7", Var "J7",
                                                                     Singular)]),
                                      FullDRS ([],
                                               [Operator2 (Union,
                                                           FullDRS ([Var "L7"; Var "M7"],
                                                                    [Object (Var "L7", Nom "pronoun", Countable, Na, Eq, Number 1,
                                                                             [], 33, 11);
                                                                     PredicateTransitive (Var "M7", Verbe "be", [], Var "I7", Var "L7",
                                                                                          Singular)]),
                                                           FullDRS ([],
                                                                    [Operator2 (Union,
                                                                                FullDRS ([Var "N7"; Var "O7"],
                                                                                         [Object (Var "N7", Nom "variable", Countable, Na, Eq, Number 1,
                                                                                                  [], 33, 15);
                                                                                          PredicateTransitive (Var "O7", Verbe "be", [], Var "I7",
                                                                                                               Var "N7", Singular)]),
                                                                                FullDRS ([Var "P7"; Var "Q7"],
                                                                                         [Object (Var "P7", Nom "proper-name", Countable, Na, Eq,
                                                                                                  Number 1, [], 33, 19);
                                                                                          PredicateTransitive (Var "Q7", Verbe "be", [], Var "I7",
                                                                                                               Var "P7", Singular)]))]))]))]));
      Operator2 (Imply,
                 FullDRS ([Var "R7"],
                          [Object (Var "R7", Nom "reference", Countable, Na, Eq, Number 1, [], 34, 3);
                           Property1Ary (Var "R7", Adj "anaphoric", Pos)]),
                 FullDRS ([Var "S7"; Var "T7"; Var "U7"],
                          [Object (Var "S7", Nom "noun-phrase", Countable, Na, Eq, Number 1,
                                   [], 34, 6);
                           PredicateTransitive (Var "T7", Verbe "precede", [], Var "S7", Var "R7",
                                                Singular);
                           PredicateTransitive (Var "U7", Verbe "refer-to", [], Var "R7", Var "S7",
                                                Singular)]));
      Operator1 (Not,
                 FullDRS ([Var "V7"; Var "W7"],
                          [Property1Ary (Var "V7", Adj "ambiguous", Pos);
                           PredicateTransitive (Var "W7", Verbe "be", [],
                                                SubAtom (Named "Attempto-Controlled-English"), Var "V7", Singular)]));
      Relation (Var "K2", SubAtom (Named "Attempto-Controlled-English"));
      Object (Var "K2", Nom "user", Countable, Na, Geq, Number 2, [], 36, 7);
      PredicateTransitive (Var "L2", Verbe "understand", [], Var "K2",
                           SubAtom (Named "Attempto-Controlled-English"), Singular);
      Object (Var "M2", Nom "experiment", Countable, Na, Eq, Number 1, [], 37, 4);
      Object (Var "S2", Nom "na", Countable, Na, Eq, Number 2, [], 37, 0);
      HasPart (Var "S2", Var "P2");
      PredicateTransitive (Var "R2", Verbe "be", [], Var "P2", Var "Q2", Singular);
      Property2Ary (Var "Q2", Adj "comparable-to", Pos,
                    SubAtom (Named "Attempto-Controlled-English"));
      HasPart (Var "S2", SubAtom (Named "Attempto-Controlled-English"));
      Property1Ary (Var "P2", Adj "formal", Pos); Relation (Var "O2", Var "S2");
      Object (Var "O2", Nom "understandability", Countable, Na, Eq, Number 1,
              [], 37, 8);
      Object (Var "P2", Nom "language", Countable, Na, Eq, Number 1, [], 37, 14);
      PredicateTransitive (Var "N2", Verbe "test", [], Var "M2", Var "O2",
                           Singular);
      Relation (Var "V2", Var "W2");
      Object (Var "V2", Nom "result", Countable, Na, Eq, Number 1, [], 38, 2);
      PredicateTransitive (Var "T2", Verbe "show", [], Var "V2", Var "U2",
                           Singular);
      SubDrs ("U2",
              FullDRS
                ([Var "X7"; Var "Y7"; Var "Z7"; Var "A8"; Var "B8"; Var "C8"; Var "D8"],
                 [Property2Ary (Var "X7", Adj "understandable", Comp_than, Var "Z7");
                  PredicateTransitive (Var "Y7", Verbe "be", [],
                                       SubAtom (Named "Attempto-Controlled-English"), Var "X7", Singular);
                  Relation (Var "D8", SubAtom (Named "Attempto-Controlled-English"));
                  Object (Var "D8", Nom "learning-time", Countable, Na, Eq, Number 1,
                          [], 38, 19);
                  Property2Ary (Var "A8", Adj "short", Comp_than, Var "C8");
                  PredicateTransitive (Var "B8", Verbe "be", [], Var "D8", Var "A8",
                                       Singular);
                  Relation (Var "C8", Var "Z7"); Property1Ary (Var "Z7", Adj "formal", Pos);
                  Object (Var "C8", Nom "learning-time", Countable, Na, Eq, Number 1,
                          [], 38, 26);
                  Object (Var "Z7", Nom "language", Countable, Na, Eq, Number 1, [], 38, 30)]));
      Object (Var "W2", Nom "experiment", Countable, Na, Eq, Number 1, [], 38, 5);
      Operator1 (Question,
                 FullDRS ([Var "E8"; Var "F8"],
                          [Query (Var "E8", Which);
                           Object (Var "E8", Nom "tool", Countable, Na, Geq, Number 2, [], 39, 2);
                           PredicateTransitive (Var "F8", Verbe "use", [], Var "E8",
                                                SubAtom (Named "Attempto-Controlled-English"), Singular)]));
      Object (Var "X2", Nom "tool", Countable, Na, Geq, Number 2, [], 40, 4);
      PredicateTransitive (Var "Y2", Verbe "use", [], Var "X2",
                           SubAtom (Named "Attempto-Controlled-English"), Singular);
      Operator2 (Imply,
                 FullDRS ([Var "G8"; Var "H8"; Var "I8"],
                          [Object (Var "G8", Nom "tool", Countable, Na, Eq, Number 1, [], 41, 2);
                           PredicateTransitive (Var "H8", Verbe "use", [], Var "G8",
                                                SubAtom (Named "Attempto-Controlled-English"), Singular);
                           PredicateTransitive (Var "I8", Verbe "develop", [],
                                                SubAtom (Named "Attempto-team"), Var "G8", Singular)]),
                 FullDRS ([Var "J8"; Var "K8"],
                          [Property2Ary (Var "J8", Adj "licensed-under", Pos, SubAtom (Named "LGPL"));
                           PredicateTransitive (Var "K8", Verbe "be", [], Var "G8", Var "J8",
                                                Singular)]));
      Object (Var "Z2", Nom "open-source-license", Countable, Na, Eq, Number 1,
              [], 42, 4);
      PredicateTransitive (Var "A3", Verbe "be", [], SubAtom (Named "LGPL"),
                           Var "Z2", Singular);
      Object (Var "B3", Nom "tool", Countable, Na, Eq, Number 5, [], 43, 2);
      Property1Ary (Var "C3", Adj "presented", Pos);
      PredicateTransitive (Var "D3", Verbe "be", [], Var "B3", Var "C3", Singular);
      Modifier_Adv (Var "D3", Adv "here", Pos);
      Object (Var "E3", Nom "parser", Countable, Na, Eq, Number 1, [], 44, 4);
      PredicateTransitive (Var "F3", Verbe "be", [],
                           SubAtom (Named "Attempto-Parsing-Engine"), Var "E3", Singular);
      Object (Var "G3", Nom "abbreviation", Countable, Na, Eq, Number 1, [], 45, 4);
      Relation (Var "G3", SubAtom (String "Attempto-Parsing-Engine"));
      PredicateTransitive (Var "H3", Verbe "be", [], SubAtom (String "APE"),
                           Var "G3", Singular);
      Operator2 (Imply,
                 FullDRS ([Var "L8"; Var "M8"; Var "N8"],
                          [Object (Var "L8", Nom "ACE-text", Countable, Na, Eq, Number 1, [], 46, 3);
                           Object (Var "M8", Nom "error", Countable, Na, Eq, Number 1, [], 46, 7);
                           Property1Ary (Var "M8", Adj "syntactical", Pos);
                           PredicateTransitive (Var "N8", Verbe "contain", [], Var "L8", Var "M8",
                                                Singular)]),
                 FullDRS ([Var "O8"; Var "P8"; Var "Q8"; Var "R8"; Var "S8"; Var "T8"],
                          [Object (Var "O8", Nom "error-message", Countable, Na, Eq, Number 1,
                                   [], 46, 12);
                           Relation (Var "Q8", Var "M8");
                           Object (Var "Q8", Nom "location", Countable, Na, Eq, Number 1, [], 46, 16);
                           PredicateTransitive (Var "P8", Verbe "point-to", [], Var "O8", Var "Q8",
                                                Singular);
                           Relation (Var "S8", Var "M8");
                           Property1Ary (Var "S8", Adj "possible", Pos);
                           Object (Var "S8", Nom "cause", Countable, Na, Eq, Number 1, [], 46, 25);
                           PredicateTransitive (Var "R8", Verbe "describe", [], Var "O8", Var "S8",
                                                Singular);
                           PredicateTransitive (Var "T8", Verbe "generate", [],
                                                SubAtom (Named "Attempto-Parsing-Engine"), Var "O8", Singular)]));
      Operator2 (Imply,
                 FullDRS ([Var "U8"; Var "V8"; Var "W8"; Var "X8"],
                          [Relation (Var "W8", Var "X8");
                           Object (Var "X8", Nom "ACE-text", Countable, Na, Eq, Number 1, [], 47, 6);
                           Object (Var "W8", Nom "syntax", Countable, Na, Eq, Number 1, [], 47, 3);
                           Property1Ary (Var "U8", Adj "correct", Pos);
                           PredicateTransitive (Var "V8", Verbe "be", [], Var "W8", Var "U8",
                                                Singular)]),
                 FullDRS ([Var "Y8"; Var "Z8"],
                          [Object (Var "Y8", Nom "discourse-representation-structure", Countable, Na,
                                   Eq, Number 1, [], 47, 16);
                           PredicateTransitive (Var "Z8", Verbe "translate", [],
                                                SubAtom (Named "Attempto-Parsing-Engine"), Var "X8", Singular);
                           Modifier_pp (Var "Z8", Preposition "into", Var "Y8")]));
      Operator2 (Imply,
                 FullDRS ([Var "A9"],
                          [Object (Var "A9", Nom "discourse-representation-structure", Countable, Na,
                                   Eq, Number 1, [], 48, 2)]),
                 FullDRS ([Var "B9"; Var "C9"; Var "D9"],
                          [Object (Var "B9", Nom "formula", Countable, Na, Eq, Number 1, [], 48, 6);
                           Relation (Var "B9", SubAtom (Named "first-order-logic"));
                           Property2Ary (Var "C9", Adj "equivalent-to", Pos, Var "B9");
                           PredicateTransitive (Var "D9", Verbe "be", [], Var "A9", Var "C9",
                                                Singular)]))]);;

 
let ll = 
        let l1 = match tree2 with FullDRS(_,ll) -> ll in L.filter (fun e -> is_alterateur e || is_Predicate e || is_Object e) l1


let l = [Object (Var "A", Nom "user", Countable, Na, Geq, Number 2, [], 3, 4);
 PredicateTransitive (Var "B", Verbe "want", [], Var "A", Var "C", Singular);
 PredicateTransitive (Var "D", Verbe "prefer", [], Var "A", Var "E", Singular);
 Object (Var "F", Nom "controlled-natural-language", Countable, Na, Eq,
  Number 5, [], 9, 13);
 Property1Ary (Var "F", Adj "important", Pos);
 PredicateTransitive (Var "G", Verbe "be", [], Var "H", Var "F", Singular);
 Object (Var "H", Nom "na", Countable, Na, Eq, Number 5, [], 9, 0);
 Object (Var "I", Nom "controllnguage", Countable, Na, Eq, Number 1, [], 10, 6);
 Object (Var "J", Nom "subset", Countable, Na, Eq, Number 1, [], 10, 10);
 Relation (Var "J", SubAtom (Named "English"));
 PredicateTransitive (Var "K", Verbe "be", [], Var "I", Var "J", Singular);
 Object (Var "L", Nom "research-group", Countable, Na, Eq, Number 1,
  [], 10, 23);
 Relation (Var "L", SubAtom (Named "University-of-Zurich"));
 PredicateTransitive (Var "M", Verbe "be", [], SubAtom (Named "Attempto-team"),
  Var "L", Singular);
 PredicateTransitive (Var "N", Verbe "develop", [],
  SubAtom (Named "Attempto-team"), Var "I", Singular);
 PredicateTransitive (Var "O", Verbe "be", [],
  SubAtom (Named "Attempto-Controlled-English"), Var "I", Singular);
 Object (Var "P", Nom "abbreviation", Countable, Na, Eq, Number 1, [], 11, 4);
 Relation (Var "P", SubAtom (String "Attempto-Controlled-English"));
 PredicateTransitive (Var "Q", Verbe "be", [], SubAtom (String "ACE"),
  Var "P", Singular);];;

 let bigrawdrs =
 "drs([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,A2,B2,C2,D2,E2,F2,G2,H2,I2,J2,K2,L2,M2,N2,O2,P2,Q2,R2,S2,T2,U2,V2,W2,X2,Y2,Z2,A3,B3,C3,D3,E3,F3,G3,H3,I3,J3,K3,L3,M3,N3,O3,P3,Q3,R3,S3,T3,U3,V3,W3,X3,Y3,Z3,A4,B4,C4,D4,E4,F4,G4,H4,I4,J4,K4,L4,M4,N4,O4,P4,Q4,R4,S4,T4,U4,V4,W4,X4,Y4,Z4,A5,B5,C5,D5,E5,F5,G5,H5,I5,J5,K5,L5,M5,N5,O5,P5,Q5,R5,S5,T5,U5,V5,W5],[can(drs([X5],[predicate(X5,describe,named('Attempto-Controlled-English'),named('Attempto-Controlled-English'))-1/4])),question(drs([Y5,Z5,A6],[query(Y5,who)-2/1,object(Z5,'controlled-natural-language',countable,na,eq,1)-2/4,predicate(A6,need,Y5,Z5)-2/2])),object(A,user,countable,na,geq,2)-3/4,predicate(B,want,A,C)-3/6,C:drs([B6,C6],[object(B6,method,countable,na,eq,1)-3/11,property(B6,formal,pos)-3/10,predicate(C6,use,A,B6)-3/8]),=&gt;(drs([D6],[object(D6,language,countable,na,eq,1)-3/17,property(D6,formal,pos)-3/16]),drs([],[-(drs([E6],[predicate(E6,understand,A,D6)-3/14]))])),predicate(D,prefer,A,E)-4/2,E:drs([F6,G6,H6],[object(F6,method,countable,na,eq,1)-4/7,property(F6,formal,pos)-4/6,relation(G6,of,I6)-4/9,object(G6,language,countable,na,eq,1)-4/12,property(G6,natural,pos)-4/11,predicate(H6,use,A,F6)-4/4,modifier_pp(H6,in,G6)-4/8]),=&gt;(drs([J6,K6,L6,M6],[object(J6,method,countable,na,eq,1)-5/4,property(J6,formal,pos)-5/3,object(K6,language,countable,na,eq,1)-5/9,property(K6,natural,pos)-5/8,property(L6,'expressed-in',pos,K6)-5/6,predicate(M6,be,J6,L6)-5/5]),drs([],[=&gt;(drs([N6,O6],[object(N6,person,countable,na,eq,1)-5/12,predicate(O6,speak,N6,K6)-5/14]),drs([],[can(drs([P6],[predicate(P6,use,N6,J6)-5/19]))]))])),=&gt;(drs([Q6],[object(Q6,'controlled-natural-language',countable,na,eq,1)-6/2]),drs([R6,S6,T6],[object(R6,subset,countable,na,eq,1)-6/5,object(S6,language,countable,na,eq,1)-6/9,property(S6,natural,pos)-6/8,relation(R6,of,S6)-6/6,predicate(T6,be,Q6,R6)-6/3])),=&gt;(drs([U6],[object(U6,'controlled-natural-language',countable,na,eq,1)-7/3,can(drs([V6,W6,X6],[object(V6,language,countable,na,eq,1)-7/9,property(V6,logical,pos)-7/8,property(W6,'translated-into',pos,V6)-7/6,predicate(X6,be,U6,W6)-7/4]))]),drs([Y6,Z6],[object(Y6,language,countable,na,eq,1)-7/16,property(Y6,formal,pos)-7/15,can(drs([],[=&gt;(drs([A7,B7,C7,D7],[object(A7,user,countable,na,eq,1)-7/24,predicate(D7,underlie,C7,U6)-7/31,property(C7,natural,pos)-7/28,object(C7,language,countable,na,eq,1)-7/29,object(U6,'controlled-natural-language',countable,na,eq,1)-7/33,predicate(B7,speak,A7,C7)-7/26]),drs([E7],[modifier_adv(E7,easily,pos)-7/20,predicate(E7,understand,A7,Y6)-7/21]))])),predicate(Z6,be,U6,Y6)-7/13])),=&gt;(drs([F7,G7],[object(F7,developer,countable,na,eq,1)-8/2,object(G7,'controlled-natural-language',countable,na,eq,1)-8/5,relation(F7,of,G7)-8/3]),drs([H7,I7],[predicate(H7,believe,F7,I7)-8/6,I7:drs([],[can(drs([J7,K7,L7,M7,N7,O7],[object(O7,na,countable,na,eq,2)-8/'',has_part(O7,M7)-8/'',has_part(O7,L7)-8/'',property(L7,formal,pos)-8/15,relation(K7,of,O7)-8/13,object(K7,formality,countable,na,eq,1)-8/12,object(L7,language,countable,na,eq,1)-8/16,predicate(J7,combine,G7,K7)-8/10,relation(M7,of,N7)-8/20,property(N7,natural,pos)-8/22,object(N7,language,countable,na,eq,1)-8/23,object(M7,familiarity,countable,na,eq,1)-8/19]))])])),has_part(H,named('PENG'))-9/'',object(F,'controlled-natural-language',countable,na,eq,5)-9/13,property(F,important,pos)-9/12,predicate(G,be,H,F)-9/10,has_part(H,named('Common-Logic-Controlled-English'))-9/'',has_part(H,named('CPL'))-9/'',has_part(H,named('Rabbit'))-9/'',has_part(H,named('Attempto-Controlled-English'))-9/'',object(H,na,countable,na,eq,5)-9/'',object(I,'controlled-natural-language',countable,na,eq,1)-10/4,object(J,subset,countable,na,eq,1)-10/8,relation(J,of,named('English'))-10/9,predicate(K,be,I,J)-10/6,object(L,'research-group',countable,na,eq,1)-10/21,relation(L,of,named('University-of-Zurich'))-10/22,predicate(M,be,named('Attempto-team'),L)-10/19,predicate(N,develop,named('Attempto-team'),I)-10/14,predicate(O,be,named('Attempto-Controlled-English'),I)-10/2,object(P,abbreviation,countable,na,eq,1)-11/4,relation(P,of,string('Attempto-Controlled-English'))-11/5,predicate(Q,be,string('ACE'),P)-11/2,question(drs([P7,Q7],[query(P7,which)-12/1,object(P7,word,countable,na,geq,2)-12/2,predicate(Q7,support,named('Attempto-Controlled-English'),P7)-12/4])),relation(Z,of,named('Attempto-Controlled-English'))-13/3,object(Z,vocabulary,countable,na,eq,1)-13/2,has_part(Y,R)-13/'',predicate(U,'consist-of',Z,Y)-13/5,has_part(Y,V)-13/'',object(Y,na,countable,na,eq,2)-13/'',predicate(T,be,R,S)-13/9,property(S,'built-in',pos)-13/10,object(R,'function-word',countable,na,geq,2)-13/7,predicate(X,define,W,V)-13/16,object(W,lexicon,countable,na,eq,1)-13/19,object(V,'content-word',countable,na,geq,2)-13/13,object(A1,'function-word',countable,na,greater,80)-14/6,predicate(B1,use,named('Attempto-Controlled-English'),A1)-14/2,object(C1,'function-word',countable,na,eq,3)-15/3,property(C1,typical,pos)-15/2,has_part(E1,string(every))-15/'',predicate(D1,be,C1,E1)-15/4,has_part(E1,string(or))-15/'',has_part(E1,string(is))-15/'',object(E1,na,countable,na,eq,3)-15/'',=&gt;(drs([R7],[object(R7,'content-word',countable,na,eq,1)-16/2]),drs([],[v(drs([S7,T7],[object(S7,adverb,countable,na,eq,1)-16/5,predicate(T7,be,R7,S7)-16/3]),drs([],[v(drs([U7,V7],[object(U7,adjective,countable,na,eq,1)-16/9,predicate(V7,be,R7,U7)-16/7]),drs([],[v(drs([W7,X7],[object(W7,'proper-name',countable,na,eq,1)-16/13,predicate(X7,be,R7,W7)-16/11]),drs([],[v(drs([Y7,Z7],[object(Y7,noun,countable,na,eq,1)-16/17,predicate(Z7,be,R7,Y7)-16/15]),drs([],[v(drs([A8,B8],[object(A8,verb,countable,na,eq,1)-16/21,predicate(B8,be,R7,A8)-16/19]),drs([C8,D8],[object(C8,preposition,countable,na,eq,1)-16/25,predicate(D8,be,R7,C8)-16/23]))]))]))]))]))])),object(F1,lexicon,countable,na,eq,1)-17/4,object(G1,'content-word',countable,na,greater,100000)-17/10,predicate(H1,contain,F1,G1)-17/6,predicate(I1,use,named('Attempto-Controlled-English'),F1)-17/2,=&gt;(drs([E8],[object(E8,user,countable,na,eq,1)-18/2,relation(E8,of,named('Attempto-Controlled-English'))-18/3]),drs([],[can(drs([F8,G8,H8,I8],[object(F8,lexicon,countable,na,eq,1)-18/8,object(G8,'content-word',countable,na,geq,2)-18/13,property(G8,additional,pos)-18/12,predicate(H8,contain,F8,G8)-18/10,predicate(I8,use,E8,F8)-18/6]))])),question(drs([J8,K8,L8],[query(J8,what)-19/1,relation(L8,of,named('Attempto-Controlled-English'))-19/5,object(L8,syntax,countable,na,eq,1)-19/4,predicate(K8,be,J8,L8)-19/2])),object(J1,language,countable,na,eq,1)-20/5,property(J1,rich,pos)-20/4,[has_part(N1,K1)-20/'',object(K1,'construction-rule',countable,na,less,20)-20/13],predicate(L1,describe,N1,J1)-20/8,[has_part(N1,M1)-20/'',object(M1,'interpretation-rule',countable,na,less,20)-20/18],object(N1,na,countable,na,eq,2)-20/'',predicate(O1,be,named('Attempto-Controlled-English'),J1)-20/2,object(R1,'construction-rule',countable,na,geq,2)-21/2,relation(Q1,of,named('Attempto-Controlled-English'))-21/6,object(Q1,syntax,countable,na,eq,1)-21/5,predicate(P1,describe,R1,Q1)-21/3,object(U1,'interpretation-rule',countable,na,geq,2)-22/2,relation(T1,of,named('Attempto-Controlled-English'))-22/6,object(T1,meaning,countable,na,eq,1)-22/5,predicate(S1,define,U1,T1)-22/3,=&gt;(drs([M8],[object(M8,sentence,countable,na,eq,1)-23/3,property(M8,admissible,pos)-23/2,relation(M8,of,named('Attempto-Controlled-English'))-23/4]),drs([],[v(drs([N8,O8],[object(N8,sentence,countable,na,eq,1)-23/9,property(N8,declarative,pos)-23/8,predicate(O8,be,M8,N8)-23/6]),drs([],[v(drs([P8,Q8],[object(P8,questionn,countable,na,eq,1)-23/13,predicate(Q8,be,M8,P8)-23/11]),drs([R8,S8],[object(R8,commandd,countable,na,eq,1)-23/17,predicate(S8,be,M8,R8)-23/15]))]))])),=&gt;(drs([T8],[object(T8,sentence,countable,na,eq,1)-24/3,property(T8,declarative,pos)-24/2]),drs([U8,V8],[object(U8,period,countable,na,eq,1)-24/6,predicate(V8,'end-with',T8,U8)-24/4,v(drs([W8,X8],[object(W8,sentence,countable,na,eq,1)-24/12,property(W8,simple,pos)-24/11,predicate(X8,be,T8,W8)-24/9]),drs([Y8,Z8],[object(Y8,sentence,countable,na,eq,1)-24/17,property(Y8,composite,pos)-24/16,predicate(Z8,be,T8,Y8)-24/14]))])),=&gt;(drs([A9],[object(A9,sentence,countable,na,eq,1)-25/3,property(A9,simple,pos)-25/2]),drs([],[v(drs([B9,C9,D9,E9],[object(B9,'noun-phrase',countable,na,eq,1)-25/6,has_part(E9,B9)-25/'',predicate(C9,'consist-of',A9,E9)-25/4,object(D9,'verb-phrase',countable,na,eq,1)-25/9,has_part(E9,D9)-25/'',object(E9,na,countable,na,eq,2)-25/'']),drs([F9,G9,H9],[has_part(H9,string('there is/are'))-25/'',predicate(F9,'consist-of',A9,H9)-25/11,object(G9,'noun-phrase',countable,na,eq,1)-25/15,has_part(H9,G9)-25/'',object(H9,na,countable,na,eq,2)-25/'']))])),object(V1,example,countable,na,eq,1)-26/4,object(W1,sentence,countable,na,eq,1)-26/8,property(W1,simple,pos)-26/7,relation(V1,of,W1)-26/5,predicate(X1,be,string('John works.'),V1)-26/2,=&gt;(drs([I9],[object(I9,sentence,countable,na,eq,1)-27/3,property(I9,composite,pos)-27/2]),drs([J9,K9],[object(J9,constructor,countable,na,geq,1)-27/11,modifier_adv(K9,recursively,pos)-27/5,predicate(K9,form,J9,I9)-27/6])),=&gt;(drs([L9],[object(L9,constructor,countable,na,eq,1)-28/2]),drs([],[v(drs([M9,N9],[object(M9,coordination,countable,na,eq,1)-28/5,predicate(N9,be,L9,M9)-28/3]),drs([],[v(drs([O9,P9],[object(O9,subordination,countable,na,eq,1)-28/9,predicate(P9,be,L9,O9)-28/7]),drs([Q9,R9],[object(Q9,quantification,countable,na,eq,1)-28/13,predicate(R9,be,L9,Q9)-28/11]))]))])),object(Y1,example,countable,na,eq,1)-29/4,object(Z1,sentence,countable,na,eq,1)-29/8,property(Z1,composite,pos)-29/7,relation(Y1,of,Z1)-29/5,predicate(A2,be,string('If John works then Mary does not wait.'),Y1)-29/2,object(B2,example,countable,na,eq,1)-30/4,object(C2,'yes-no-question',countable,na,eq,1)-30/7,relation(B2,of,C2)-30/5,predicate(D2,be,string('Does John wait?'),B2)-30/2,object(E2,example,countable,na,eq,1)-30/12,object(F2,'wh-question',countable,na,eq,1)-30/15,relation(E2,of,F2)-30/13,predicate(G2,be,string('Who waits?'),E2)-30/10,=&gt;(drs([S9],[object(S9,commandd,countable,na,eq,1)-31/2]),drs([T9,U9,V9,W9],[object(T9,addressee,countable,na,eq,1)-31/5,predicate(U9,'start-with',S9,T9)-31/3,object(V9,'exclamation-mark',countable,na,eq,1)-31/9,predicate(W9,'end-with',S9,V9)-31/7])),object(H2,example,countable,na,eq,1)-32/4,object(I2,commandd,countable,na,eq,1)-32/7,relation(H2,of,I2)-32/5,predicate(J2,be,string('John, wait!'),H2)-32/2,=&gt;(drs([X9],[object(X9,reference,countable,na,eq,1)-33/3,property(X9,anaphoric,pos)-33/2]),drs([],[v(drs([Y9,Z9],[object(Y9,'noun-phrase',countable,na,eq,1)-33/7,property(Y9,definite,pos)-33/6,predicate(Z9,be,X9,Y9)-33/4]),drs([],[v(drs([A10,B10],[object(A10,pronoun,countable,na,eq,1)-33/11,predicate(B10,be,X9,A10)-33/9]),drs([],[v(drs([C10,D10],[object(C10,variable,countable,na,eq,1)-33/15,predicate(D10,be,X9,C10)-33/13]),drs([E10,F10],[object(E10,'proper-name',countable,na,eq,1)-33/19,predicate(F10,be,X9,E10)-33/17]))]))]))])),=&gt;(drs([G10],[object(G10,reference,countable,na,eq,1)-34/3,property(G10,anaphoric,pos)-34/2]),drs([H10,I10,J10],[object(H10,'noun-phrase',countable,na,eq,1)-34/6,predicate(I10,precede,H10,G10)-34/8,predicate(J10,'refer-to',G10,H10)-34/4])),-(drs([K10,L10],[property(K10,ambiguous,pos)-35/4,predicate(L10,be,named('Attempto-Controlled-English'),K10)-35/2])),relation(K2,of,named('Attempto-Controlled-English'))-36/5,object(K2,user,countable,na,geq,2)-36/7,predicate(L2,understand,K2,named('Attempto-Controlled-English'))-36/3,object(M2,experiment,countable,na,eq,1)-37/4,object(S2,na,countable,na,eq,2)-37/'',has_part(S2,P2)-37/'',predicate(R2,be,P2,Q2)-37/16,property(Q2,'comparable-to',pos,named('Attempto-Controlled-English'))-37/17,has_part(S2,named('Attempto-Controlled-English'))-37/'',property(P2,formal,pos)-37/13,relation(O2,of,S2)-37/9,object(O2,understandability,countable,na,eq,1)-37/8,object(P2,language,countable,na,eq,1)-37/14,predicate(N2,test,M2,O2)-37/6,relation(V2,of,W2)-38/3,object(V2,result,countable,na,eq,1)-38/2,predicate(T2,show,V2,U2)-38/6,U2:drs([M10,N10,O10,P10,Q10,R10,S10],[property(M10,understandable,comp_than,O10)-38/11,predicate(N10,be,named('Attempto-Controlled-English'),M10)-38/9,relation(S10,of,named('Attempto-Controlled-English'))-38/20,object(S10,'learning-time',countable,na,eq,1)-38/19,property(P10,short,comp_than,R10)-38/23,predicate(Q10,be,S10,P10)-38/22,relation(R10,of,O10)-38/27,property(O10,formal,pos)-38/29,object(R10,'learning-time',countable,na,eq,1)-38/26,object(O10,language,countable,na,eq,1)-38/30]),object(W2,experiment,countable,na,eq,1)-38/5,question(drs([T10,U10],[query(T10,which)-39/1,object(T10,tool,countable,na,geq,2)-39/2,predicate(U10,use,T10,named('Attempto-Controlled-English'))-39/3])),object(X2,tool,countable,na,geq,2)-40/4,predicate(Y2,use,X2,named('Attempto-Controlled-English'))-40/6,=&gt;(drs([V10,W10,X10],[object(V10,tool,countable,na,eq,1)-41/2,predicate(W10,use,V10,named('Attempto-Controlled-English'))-41/4,predicate(X10,develop,named('Attempto-team'),V10)-41/9]),drs([Y10,Z10],[property(Y10,'licensed-under',pos,named('LGPL'))-41/14,predicate(Z10,be,V10,Y10)-41/13])),object(Z2,'open-source-license',countable,na,eq,1)-42/4,predicate(A3,be,named('LGPL'),Z2)-42/2,object(B3,tool,countable,na,eq,5)-43/2,property(C3,presented,pos)-43/4,predicate(D3,be,B3,C3)-43/3,modifier_adv(D3,here,pos)-43/5,object(E3,parser,countable,na,eq,1)-44/4,predicate(F3,be,named('Attempto-Parsing-Engine'),E3)-44/2,object(G3,abbreviation,countable,na,eq,1)-45/4,relation(G3,of,string('Attempto-Parsing-Engine'))-45/5,predicate(H3,be,string('APE'),G3)-45/2,=&gt;(drs([A11,B11,C11],[object(A11,'ACE-text',countable,na,eq,1)-46/3,object(B11,error,countable,na,eq,1)-46/7,property(B11,syntactical,pos)-46/6,predicate(C11,contain,A11,B11)-46/4]),drs([D11,E11,F11,G11,H11,I11],[object(D11,'error-message',countable,na,eq,1)-46/12,relation(F11,of,B11)-46/17,object(F11,location,countable,na,eq,1)-46/16,predicate(E11,'point-to',D11,F11)-46/14,relation(H11,of,B11)-46/26,property(H11,possible,pos)-46/24,object(H11,cause,countable,na,eq,1)-46/25,predicate(G11,describe,D11,H11)-46/22,predicate(I11,generate,named('Attempto-Parsing-Engine'),D11)-46/10])),=&gt;(drs([J11,K11,L11,M11],[relation(L11,of,M11)-47/4,object(M11,'ACE-text',countable,na,eq,1)-47/6,object(L11,syntax,countable,na,eq,1)-47/3,property(J11,correct,pos)-47/8,predicate(K11,be,L11,J11)-47/7]),drs([N11,O11],[object(N11,'discourse-representation-structure',countable,na,eq,1)-47/16,predicate(O11,translate,named('Attempto-Parsing-Engine'),M11)-47/11,modifier_pp(O11,into,N11)-47/14])),=&gt;(drs([P11],[object(P11,'discourse-representation-structure',countable,na,eq,1)-48/2]),drs([Q11,R11,S11],[object(Q11,formula,countable,na,eq,1)-48/6,relation(Q11,of,named('first-order-logic'))-48/7,property(R11,'equivalent-to',pos,Q11)-48/4,predicate(S11,be,P11,R11)-48/3])),object(I3,paraphrase,countable,na,eq,1)-49/5,object(J3,'ACE-text',countable,na,eq,1)-49/8,relation(I3,of,J3)-49/6,modifier_adv(K3,optionally,pos)-49/2,predicate(K3,generate,named('Attempto-Parsing-Engine'),I3)-49/3,has_part(M3,named('OWL'))-49/'',modifier_adv(L3,optionally,pos)-49/10,predicate(L3,translate,named('Attempto-Parsing-Engine'),J3)-49/11,modifier_pp(L3,into,M3)-49/14,has_part(M3,named('Semantic-Web-Rule-Language'))-49/'',has_part(M3,named('TPTP'))-49/'',object(M3,na,countable,na,eq,3)-49/'',object(N3,reasoner,countable,na,eq,1)-50/4,predicate(O3,be,named('RACE'),N3)-50/2,object(P3,tool,countable,na,eq,1)-51/4,object(Q3,deduction,countable,na,geq,2)-51/9,property(Q3,logical,pos)-51/8,predicate(R3,perform,P3,Q3)-51/6,modifier_pp(R3,on,named('Attempto-Controlled-English'))-51/10,predicate(S3,be,named('RACE'),P3)-51/2,can(drs([T11,U11],[predicate(T11,show,named('RACE'),U11)-52/3,U11:drs([V11],[object(V11,'ACE-text',countable,na,eq,1)-52/6,v(drs([W11,X11],[property(W11,consistent,pos)-52/8,predicate(X11,be,V11,W11)-52/7]),drs([Y11,Z11],[property(Y11,inconsistent,pos)-52/11,predicate(Z11,be,V11,Y11)-52/10]))])])),can(drs([A12,B12],[predicate(A12,show,named('RACE'),B12)-52/15,B12:drs([C12,D12,E12,F12],[object(C12,'ACE-text',countable,na,eq,1)-52/18,object(D12,consequence,countable,na,eq,1)-52/22,property(D12,logical,pos)-52/21,object(E12,'ACE-text',countable,na,eq,1)-52/25,relation(D12,of,E12)-52/23,predicate(F12,be,C12,D12)-52/19])])),can(drs([G12,H12,I12,J12],[relation(I12,of,J12)-52/32,object(J12,'ACE-question',countable,na,eq,1)-52/34,object(I12,answer,countable,na,eq,1)-52/31,object(G12,'ACE-text',countable,na,eq,1)-52/37,predicate(H12,extract,named('RACE'),I12)-52/29,modifier_pp(H12,from,G12)-52/35])),=&gt;(drs([K12],[object(K12,input,countable,na,eq,1)-53/2,relation(K12,of,named('RACE'))-53/3]),drs([L12,M12],[property(L12,'expressed-in',pos,named('Attempto-Controlled-English'))-53/6,predicate(M12,be,K12,L12)-53/5])),=&gt;(drs([N12],[object(N12,output,countable,na,eq,1)-54/2,relation(N12,of,named('RACE'))-54/3]),drs([],[v(drs([O12,P12],[property(O12,'expressed-in',pos,named('Attempto-Controlled-English'))-54/6,predicate(P12,be,N12,O12)-54/5]),drs([Q12,R12],[property(Q12,'expressed-in',pos,named('English'))-54/10,predicate(R12,be,N12,Q12)-54/9]))])),object(T3,axiom,countable,na,geq,2)-55/7,property(T3,auxiliary,pos)-55/6,property(U3,'expressed-in',pos,named('Prolog'))-55/10,predicate(V3,be,T3,U3)-55/9,object(W3,knowledge,mass,na,na,na)-55/17,property(W3,'domain-independent',pos)-55/16,predicate(X3,represent,T3,W3)-55/14,predicate(Y3,support,T3,named('RACE'))-55/3,object(Z3,'rule-engine',countable,na,eq,1)-56/4,predicate(A4,be,named('AceRules'),Z3)-56/2,object(B4,'rule-engine',countable,na,eq,1)-57/5,property(B4,'forward-chaining',pos)-57/4,object(C4,input,countable,na,eq,1)-57/7,relation(C4,of,B4)-57/6,predicate(D4,be,C4,named('Attempto-Controlled-English'))-57/8,object(E4,output,countable,na,eq,1)-57/12,relation(E4,of,B4)-57/11,predicate(F4,be,E4,named('Attempto-Controlled-English'))-57/13,predicate(G4,be,named('AceRules'),B4)-57/2,object(H4,semantics,countable,na,eq,3)-58/4,predicate(I4,support,named('AceRules'),H4)-58/2,predicate(J4,support,named('AceRules'),named('negation-as-failure'))-59/2,predicate(K4,support,named('AceRules'),named('strong-negation'))-59/5,object(L4,'semantic-wiki',countable,na,eq,1)-60/4,predicate(M4,be,named('AceWiki'),L4)-60/2,object(N4,'semantic-wiki',countable,na,eq,1)-61/4,predicate(O4,use,N4,named('Attempto-Controlled-English'))-61/6,predicate(P4,be,named('AceWiki'),N4)-61/2,=&gt;(drs([S12],[object(S12,article,countable,na,eq,1)-62/2,relation(S12,of,named('AceWiki'))-62/3]),drs([T12,U12,V12,W12],[property(T12,'written-in',pos,named('Attempto-Controlled-English'))-62/6,predicate(U12,be,S12,T12)-62/5,property(V12,'translated-into',pos,named('OWL'))-62/10,predicate(W12,be,S12,V12)-62/9])),object(Q4,reasoner,countable,na,eq,1)-63/7,predicate(R4,be,named('Pellet'),Q4)-63/5,predicate(S4,use,named('AceWiki'),named('Pellet'))-63/2,relation(U4,of,V4)-64/5,object(V4,'knowledge-base',countable,na,eq,1)-64/8,relation(V4,of,named('AceWiki'))-64/6,object(U4,consistency,mass,na,na,na)-64/4,predicate(T4,ensure,named('AceWiki'),U4)-64/2,=&gt;(drs([X12,Y12,Z12],[object(X12,user,countable,na,eq,1)-65/3,object(Y12,questionn,countable,na,eq,1)-65/6,predicate(Z12,ask,X12,Y12)-65/4,modifier_pp(Z12,in,named('Attempto-Controlled-English'))-65/7]),drs([A13],[predicate(A13,answer,named('AceWiki'),Y12)-65/11])),object(W4,grammar,countable,na,eq,1)-66/4,object(X4,subset,countable,na,eq,1)-66/8,relation(X4,of,named('Attempto-Controlled-English'))-66/9,predicate(Y4,describe,W4,X4)-66/6,predicate(Z4,use,named('AceWiki'),W4)-66/2,object(A5,editor,countable,na,eq,1)-67/5,property(A5,predictive,pos)-67/4,predicate(B5,process,A5,W4)-67/7,predicate(C5,contain,named('AceWiki'),A5)-67/2,=&gt;(drs([B13,C13,D13],[object(B13,user,countable,na,eq,1)-68/3,predicate(C13,want,B13,D13)-68/4,D13:drs([E13,F13],[relation(F13,of,named('AceWiki'))-68/9,object(F13,content,countable,na,eq,1)-68/8,predicate(E13,extend,B13,F13)-68/6])]),drs([],[can(drs([G13],[predicate(G13,help,A5,B13)-68/16]))])),object(D5,experiment,countable,na,geq,2)-69/4,relation(F5,of,named('AceWiki'))-69/9,object(F5,usability,countable,na,eq,1)-69/8,predicate(E5,test,D5,F5)-69/6,object(G5,'ontology-editor',countable,na,eq,1)-70/4,predicate(H5,be,named('ACE View'),G5)-70/2,object(I5,'ontology-editor',countable,na,eq,1)-71/4,predicate(J5,support,I5,named('Attempto-Controlled-English'))-71/6,predicate(K5,support,I5,named('OWL'))-71/10,predicate(L5,support,I5,named('Semantic-Web-Rule-Language'))-71/14,predicate(M5,be,named('ACE View'),I5)-71/2,object(N5,example,countable,na,eq,1)-72/2,object(O5,'OWL-axiom',countable,na,eq,1)-72/5,relation(N5,of,O5)-72/3,predicate(P5,be,N5,string('$town sqsubseteq exists part^- . country$'))-72/6,object(Q5,symbol,countable,na,geq,3)-73/7,object(R5,user,countable,na,geq,2)-73/11,predicate(S5,confuse,Q5,R5)-73/9,predicate(T5,contain,N5,Q5)-73/3,property(U5,'expressed-in',pos,named('Attempto-Controlled-English'))-74/4,predicate(V5,be,N5,U5)-74/3,modifier_pp(V5,as,string('Every town is a part of a country.'))-74/6,=&gt;(drs([H13,I13],[property(H13,'expressed-in',pos,named('Attempto-Controlled-English'))-75/5,predicate(I13,be,N5,H13)-75/4]),drs([],[-(drs([J13],[predicate(J13,confuse,N5,R5)-75/12]))])),=&gt;(drs([K13,L13],[object(K13,reasoner,countable,na,eq,1)-76/2,predicate(L13,embed,named('ACE View'),K13)-76/5]),drs([],[v(drs([M13],[predicate(M13,understand,K13,named('OWL'))-76/6]),drs([N13],[predicate(N13,understand,K13,named('Semantic-Web-Rule-Language'))-76/9]))])),=&gt;(drs([O13,P13,Q13],[object(O13,user,countable,na,eq,1)-77/3,predicate(P13,use,O13,named('ACE View'))-77/5,predicate(Q13,understand,O13,named('Attempto-Controlled-English'))-77/9,-(drs([R13],[predicate(R13,understand,O13,named('OWL'))-77/13])),-(drs([S13],[predicate(S13,understand,O13,named('Semantic-Web-Rule-Language'))-77/18]))]),drs([],[can(drs([T13,U13],[object(T13,ontology,countable,na,eq,1)-77/26,v(drs([V13,W13],[property(V13,'expressed-in',pos,named('OWL'))-77/29,predicate(W13,be,T13,V13)-77/28]),drs([X13,Y13],[property(X13,'expressed-in',pos,named('Semantic-Web-Rule-Language'))-77/34,predicate(Y13,be,T13,X13)-77/33])),predicate(U13,develop,O13,T13)-77/24]))])),predicate(W5,congratulate,named('Attempto-team'),named('Michael'))-78/3,command(drs([Z13,A14],[object(Z13,birthday,countable,na,eq,1)-79/6,property(Z13,happy,pos)-79/5,predicate(A14,have,named('Michael'),Z13)-79/3]))]).";;
