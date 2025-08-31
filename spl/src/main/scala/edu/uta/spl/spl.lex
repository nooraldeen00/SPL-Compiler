package edu.uta.spl;

import java_cup.runtime.Symbol;

%%
%class SplLex
%public
%line
%column
%cup

%{

  private Symbol symbol ( int type, int row, int col ) {
    return new Symbol(type, row, col);
  }

  private Symbol symbol ( int type, int row, int col, Object value ) {
    return new Symbol(type, row, col, value);  }

  public void lexical_error ( String message ) {
    System.err.println("*** Lexical Error: " + message + " (line: " + (yyline+1)
                       + ", position: " + (yycolumn+1) + ")");
    System.exit(1);
  }
%}

%%

/* Keywords */
"array"         { return symbol(sym.ARRAY, yyline+1, yycolumn+1); }
"boolean"       { return symbol(sym.BOOLEAN, yyline+1, yycolumn+1); }
"by"            { return symbol(sym.BY, yyline+1, yycolumn+1); }
"def"           { return symbol(sym.DEF, yyline+1, yycolumn+1); }
"else"          { return symbol(sym.ELSE, yyline+1, yycolumn+1); }
"exit"          { return symbol(sym.EXIT, yyline+1, yycolumn+1); }
"false"         { return symbol(sym.FALSE, yyline+1, yycolumn+1); }
"float"         { return symbol(sym.FLOAT, yyline+1, yycolumn+1); }
"for"           { return symbol(sym.FOR, yyline+1, yycolumn+1); }
"if"            { return symbol(sym.IF, yyline+1, yycolumn+1); }
"int"           { return symbol(sym.INT, yyline+1, yycolumn+1); }
"loop"          { return symbol(sym.LOOP, yyline+1, yycolumn+1); }
"not"           { return symbol(sym.NOT, yyline+1, yycolumn+1); }
"or"            { return symbol(sym.OR, yyline+1, yycolumn+1); }
"print"         { return symbol(sym.PRINT, yyline+1, yycolumn+1); }
"read"          { return symbol(sym.READ, yyline+1, yycolumn+1); }
"return"        { return symbol(sym.RETURN, yyline+1, yycolumn+1); }
"string"        { return symbol(sym.STRING, yyline+1, yycolumn+1); }
"to"            { return symbol(sym.TO, yyline+1, yycolumn+1); }
"type"          { return symbol(sym.TYPE, yyline+1, yycolumn+1); }
"var"           { return symbol(sym.VAR, yyline+1, yycolumn+1); }
"while"         { return symbol(sym.WHILE, yyline+1, yycolumn+1); }
"true"          { return symbol(sym.TRUE, yyline+1, yycolumn+1); }

/* Operators */
"&&"            { return symbol(sym.AND, yyline+1, yycolumn+1); }
"/"             { return symbol(sym.DIV, yyline+1, yycolumn+1); }
"%"             { return symbol(sym.MOD, yyline+1, yycolumn+1); }
"+"             { return symbol(sym.PLUS, yyline+1, yycolumn+1); }
"-"             { return symbol(sym.MINUS, yyline+1, yycolumn+1); }
"*"             { return symbol(sym.TIMES, yyline+1, yycolumn+1); }
"<"             { return symbol(sym.LT, yyline+1, yycolumn+1); }
"<="            { return symbol(sym.LEQ, yyline+1, yycolumn+1); }
">"             { return symbol(sym.GT, yyline+1, yycolumn+1); }
">="            { return symbol(sym.GEQ, yyline+1, yycolumn+1); }
"=="            { return symbol(sym.EQ, yyline+1, yycolumn+1); }
"<>"            { return symbol(sym.NEQ, yyline+1, yycolumn+1); }
"="             { return symbol(sym.EQUAL, yyline+1, yycolumn+1); }
":"             { return symbol(sym.COLON, yyline+1, yycolumn+1); }
";"             { return symbol(sym.SEMI, yyline+1, yycolumn+1); }
","             { return symbol(sym.COMMA, yyline+1, yycolumn+1); }
"#"             { return symbol(sym.SHARP, yyline+1, yycolumn+1); }
"."             { return symbol(sym.DOT, yyline+1, yycolumn+1); }
"("             { return symbol(sym.LP, yyline+1, yycolumn+1); }
")"             { return symbol(sym.RP, yyline+1, yycolumn+1); }
"{"             { return symbol(sym.LB, yyline+1, yycolumn+1); }
"}"             { return symbol(sym.RB, yyline+1, yycolumn+1); }
"["             { return symbol(sym.LSB, yyline+1, yycolumn+1); }
"]"             { return symbol(sym.RSB, yyline+1, yycolumn+1); }

/* Identifiers and Literals */
[a-zA-Z_][a-zA-Z0-9_]*   { return symbol(sym.ID, yyline+1, yycolumn+1, yytext()); }
\"([^\"\\]|\\.)*\"       { return symbol(sym.STRING_LITERAL, yyline+1, yycolumn+1, yytext().substring(1, yytext().length() - 1)); }
[0-9]+                   { return symbol(sym.INTEGER_LITERAL, yyline+1, yycolumn+1, Integer.parseInt(yytext())); }
[0-9]+\.[0-9]*           { return symbol(sym.FLOAT_LITERAL, yyline+1, yycolumn+1, Float.parseFloat(yytext())); }

/* Whitespace and Comments */
[ \t\r\n]+               { /* Skip whitespace */ }
"//".*                   { /* Skip single-line comments */ }
"/*" .*? "*/" { /* Skip multi-line comments */ }

/* Error Handling */
. { lexical_error("Illegal character: " + yytext()); }
