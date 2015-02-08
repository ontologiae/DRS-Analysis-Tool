DRS Analysis Tool (DAT) parses and analyses DRS output from Attempto Controlled English (ACE) parser (APE).

DAT parses the DRS with a first grammar, made specifically for the parser. It is possible, then, to translate this first grammar to a second (the DAT's grammar), made for analysis purpose.
See syntax.ml for the two grammars.

For instance the sentence "Droopy is happy." is translate by APE in "drs([A, B], [property(A, happy, pos)-1/3, predicate(B, be, named('Droopy'), A)-1/2])".
A parse function gives :

   # parse "drs([A, B], [property(A, happy, pos)-1/3, predicate(B, be, named('Droopy'), A)-1/2]).";;

    - : Syntax.drs =
    Syntax.DRS ([Syntax.Varp "A"; Syntax.Varp "B"],
    [Syntax.Atomicp
      (Syntax.Atom ("property",
        [Syntax.Variable "A"; Syntax.Const "happy"; Syntax.Const "pos"], 1, 3));
     Syntax.Atomicp
      (Syntax.Atom ("predicate",
        [Syntax.Variable "B"; Syntax.Const "be";
         Syntax.TermAtom
          (Syntax.Atom ("named", [Syntax.ConstCh "Droopy"], 0, 0));
         Syntax.Variable "A"],
        1, 2))])


a parsecomplete gives :

    # parsecomplete "drs([A, B], [property(A, happy, pos)-1/3, predicate(B, be, named('Droopy'), A)-1/2]).";;

    - : Syntax.fulldrs =
    Syntax.FullDRS ([Syntax.Var "A"; Syntax.Var "B"],
    [Syntax.Property1Ary (Syntax.Var "A", Syntax.Adj "happy", Syntax.Pos);
     Syntax.PredicateTransitive (Syntax.Var "B", Syntax.Verbe "be",
      Syntax.SubAtom (Syntax.Named "Droopy"), Syntax.Var "A")])




PARAPHRASER
===========

There's an experimental (and ugly) paraphraser. It works on simple example.

	# let g = FullDRS ([Var "J1"; Var "K1"; Var "L1"],
	[Object (Var "J1", Nom "time", Countable, Na, Greater, Number 2, 13, 8);
	 Object (Var "K1", Nom "day", Countable, Na, Eq, Number 10, 13, 13);
	 PredicateTransitive (Var "L1", Verbe "vote", SubAtom (Named "User1"), Var "J1", Singular);
	 Modifier_pp (Var "L1", Preposition "in", Var "K1");
	 Modifier_pp (Var "L1", Preposition "for", SubAtom (Named "User2"))]);;

	# paraphrase g;;
	- : bytes list = ["User1 votes More than 2 time  for User2 "]

There's still syntaxes mistakes, but it almost works for a large subset of DRS.

DEPENDANCY
==========

DAT needs ocamlbuild and ocamlyacc/ocamllex. It is possible to use Mehnir, but it is not well tested.


USE
===

Type make clean and make for building.
You can play with DAT with the OCaml TopLevel, by typing : 

    $ ocaml -I _build/ parser.cmo message.cmo lexer.cmo syntax.cmo -init drsxp.ml 

drsxp.ml contains several test examples.
It also contains "parse" function, which parses a DRS's string and translate it into the first grammar.
"parsecomplete" parses the DRS's string and translate it in the DAT's grammar.

With ``make top`` you can play with this lib.
