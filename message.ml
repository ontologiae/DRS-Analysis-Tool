(** Error messages. *)

open Lexing

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

let report = print_endline

(*1. Pluton fc  Carré Jupiter/Uranus au début du bélier - Carré à la lune conjoint noeud sud - Mercure en Gémaux
 * Venu pour découvrir l'amour
 * I worked hard for that so I must win.
 * "Si on le cadre trop, il déprime."
 * Sa névrose, je pense, consiste à ne pas supporter de se tromper (Cadrer = l'autorité parentale "qui sait", enfin c'est ce qu'il croit encore pour les 2/3 ans à venir, lui montrent qu'il se trompe, donc il déprime). Pour lui se tromper, paraitre ridicule, ne pas y arriver, c'est l'horreur et ça le fait déprimer. Se tromper veut dire qu'il ne va plus être aimé.
 * Quand tu m'as dis "Si on le cadre trop, il déprime.", là on est en plein sur le complexe névrotique, cet exemple est parfait !
 * Donc, à mon sens, dans les 3 ans à venir, c'est hyper important de lui faire comprendre que c'est PAS GRAVE de se tromper, qu'il peut  se "signer le droit de se tromper", qu'il a le droit d'être ridicule 5 mn et que les gens qui
 * se foutent de lui, c'est juste des cons, c'est tout.
 * La seule chose qui importe, c'est qu'il essaye, qu'il fasse l'effort de progresser, et que ça, c'est la seule chose qui compte. Si ça marche pas c'est pas grave, autre chose marchera*)
