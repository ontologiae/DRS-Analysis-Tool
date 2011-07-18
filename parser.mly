%{
  open Syntax
%}

%token <string>VAR
%token <string>CONST
%token <int>NBR
%token <string>STRING
%token <string>STRINGALL

%token FROM
%token COMMA
%token TRUE
%token POINT
%token LPAREN RPAREN
%token GOAL
%token QUIT
%token SEMICOLON2
%token USE
%token DRS
%token GCROCHET
%token DCROCHET
%token TIRET
%token SLASH
%token QUOTE
%token NAF

%token MUST
%token MAY
%token CAN
%token IMPLY, UNION


%token EOF

%start drstart
%type <Syntax.drs> drstart

/*%right SEMICOLON2
%nonassoc POINT GOAL IMPLIED
%right COMMA
*/
%%

drstart:
   | drs POINT                                      {$1}

 /* Définition d'un DRS */
drs:
  | DRS  LPAREN domain COMMA conditions RPAREN      { DRS( $3 , $5) }

/*Un domain est une liste de variables*/
domain:
        | GCROCHET DCROCHET                          { [] }
        | GCROCHET argsdomain DCROCHET               {  $2  }

/* On gère la liste de n arguments potentiels*/
argsdomain:
        |  literalvar                                {[Varp($1)]}
        |  literalvar COMMA argsdomain               { (Varp($1) :: $3)}

  
conditions:
        | GCROCHET conditionlist DCROCHET            { $2 }
        | GCROCHET DCROCHET                          { [] }

conditionlist:
        | condition                                  { [$1] }
        | condition COMMA conditionlist              { $1::$3 }

condition:
        | drsoperateur1 LPAREN drs RPAREN            { Operatorp1($1,$3) }
        | drsoperateur2 LPAREN drs COMMA drs RPAREN  { Operatorp2($1,$3,$5) }
        | atom                                       { Atomicp $1 }

drsoperateur1:
        | CAN                                        { Can  }
        | MUST                                       { Must }
        | MAY                                        { May  }
        | TIRET                                      { Not  }
        | NAF                                        { Naf  }

drsoperateur2:
        | IMPLY                                      { Imply }
        | UNION                                      { Union }



atom:
  | CONST                                            { Atom($1, [],0,0) }
  | CONST LPAREN args RPAREN                         { Atom ($1,$3,0,0) }
  | CONST LPAREN args RPAREN TIRET NBR SLASH NBR     { Atom ($1, $3,$6,$8) }
  | CONST LPAREN args RPAREN TIRET NBR SLASH STRINGALL { Atom ($1, $3,$6,0) }
  



args:
  | literal                                          { [$1] }
  | CONST LPAREN literal RPAREN                      { [TermAtom(Atom($1,[$3],0,0))] }
  | CONST LPAREN literal RPAREN COMMA args           { (TermAtom(Atom($1,[$3],0,0))) :: $6 }
  | literal COMMA args                               { $1 :: $3 }



literalvar:
  | VAR                                              {$1}

literal:
  | STRINGALL                                        { ConstCh($1) }
  | CONST                                            { Const($1) }
  | VAR                                              { Variable ($1) }
  | NBR                                              { Nbr($1) }

 


%%
