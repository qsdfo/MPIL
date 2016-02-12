type noeud_lex = Lettre of char * bool * arbre_lex
and arbre_lex = noeud_lex  list;;

type mot = string;;
