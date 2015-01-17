;; -*- Hen -*-
;;
;;  The lexical analyzer for mini-ML.
;;
;;  Based on the code and paper by Xavier Leroy (2000): A modular
;;  module system. Journal of Functional Programming, 10, pp 269-303
;;  doi:10.1017/S0956796800003683
;;
;;
;; Copyright 2010-2014 Ivan Raikov
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;

upper    [A-Z]
lower    [a-z]
hex      [0-9A-Fa-f]
octal    [0-7]
binary   [0-1]
decimal  [0-9]

%%

;; Lexer rules 

("component"|"module")           (tok yyline MODULE)
("val"|"binding")                (tok yyline VALUE)


"function"         (tok yyline FUNCTION)
"fun"              (tok yyline FUNCTION)
"return"           (tok yyline RETURN)
"ret"              (tok yyline RETURN)
"let"              (tok yyline LET)
"struct"           (tok yyline STRUCT)
"end"              (tok yyline END)
"functor"          (tok yyline FUNCTOR)
"type"             (tok yyline TYPE)
"sig"              (tok yyline SIG)
"in"               (tok yyline IN)
"if"               (tok yyline IF)
"then"             (tok yyline THEN)
"else"             (tok yyline ELSE)


({lower}|{upper})({lower}|{upper}|_|\.|{decimal})*    (tok yyline IDENT (ident-create yytext))
`({lower}|{upper})({lower}|{upper}|_|\.|{decimal})*   (tok yyline LABEL (string->symbol (substring yytext 1 (string-length yytext))))

0(x|X)({hex})+                             (tok yyline NAT (string->number (substring yytext 2 (string-length yytext)) 16))
0(o|O)({octal})+                           (tok yyline NAT (string->number (substring yytext 2 (string-length yytext)) 8))
0(b|B)({binary})+                          (tok yyline NAT (string->number (substring yytext 2 (string-length yytext)) 2))
0(d|D)({decimal})+                         (tok yyline NAT (string->number (substring yytext 2 (string-length yytext)) 10))

-?(({decimal}+\.({decimal}+)?)|(\.{decimal}+))([eE]([-+])?{decimal}+)?   (tok yyline REAL (string->number yytext))

{decimal}+                                 (tok yyline NAT (string->number yytext))


\"                               (let loop ([cs '()])
                                      (let ([c (yygetc)])
                                        (cond [(eq? 'eof c)   (lexer-error "unexpected end of string constant")]
                                              [(char=? c #\\) (let ((n (yygetc)))
                                                                (loop (cons n cs)))]
                                              [(char=? c #\") (tok yyline STRING (reverse-list->string cs))] 
                                              [else (loop (cons c cs))])))


"["                       (let loop ((kont (lambda (x) (tok yyline SEXPR (make-sexpr-macro #f x))))
				     (result '(#\()))
                             (let ((c (yygetc)))
                               (cond ((eq? 'eof c)    (lexer-error "unexpected end of expression"))
                                     ((char=? #\] c)  (kont (cons #\) result)))
                                     ((char=? #\[ c)  (loop (lambda (x) (loop kont (append x result))) '(#\()))
                                     (else            (loop kont (cons c result)))
                                     )))

"@"{lower}"["            (let ((label (substring yytext 2 (- (string-length yytext) 1))))
			    (let loop ((kont (lambda (x) (tok yyline SEXPR (make-sexpr-macro label x))))
				       (result '(#\()))
                             (let ((c (yygetc)))
                               (cond ((eq? 'eof c)    (lexer-error "unexpected end of expression"))
                                     ((char=? #\] c)  (kont (cons #\) result)))
                                     ((char=? #\[ c)  (loop (lambda (x) (loop kont (append x result))) '(#\()))
                                     (else            (loop kont (cons c result)))
                                     ))
			     ))

"#["                       (let loop ((kont (lambda (x) (tok yyline QEXPR x))) 
				      (result '(#\()))
                             (let ((c (yygetc)))
                               (cond ((eq? 'eof c)    (lexer-error "unexpected end of expression"))
                                     ((char=? #\] c)  (kont (cons #\) result)))
                                     ((char=? #\[ c)  (loop (lambda (x) (loop kont (append x result))) '(#\()))
                                     (else            (loop kont (cons c result)))
                                     )))

"(*"                       (let loop ((kont yycontinue))
                             (let ((c (yygetc)))
                               (cond ((eq? 'eof c) (lexer-error "unexpected end of comment"))
                                     ((and (char=? #\* c) (char=? #\) (yygetc))) (kont))
                                     ((and (char=? #\( c) (char=? #\* (yygetc))) (loop (lambda () (loop kont))))
                                     (else (loop kont)))))

"("                                (tok yyline LPAREN)
")"                                (tok yyline RPAREN)
"."                                (tok yyline DOT)
";"                                (tok yyline SEMICOLON)
"->"                               (tok yyline ARROW)
"=>"                               (tok yyline DARROW)
"="                                (tok yyline EQUAL)
","                                (tok yyline COMMA)
"'"                                (tok yyline QUOTE)
":"                                (tok yyline COLON)
"*"                                (tok yyline STAR)
"+"                                (tok yyline PLUS)
"-"                                (tok yyline MINUS)
"/"                                (tok yyline SLASH)
"=="                               (tok yyline EQEQ)
"<>"                               (tok yyline LG)
"<"                                (tok yyline LESS)
">"                                (tok yyline GREATER)
"<="                               (tok yyline LEQ)
">="                               (tok yyline GEQ)



\10+                               (yycontinue)
\9+                                (yycontinue)
\13+                               (yycontinue)
\32+                               (yycontinue)



<<EOF>>                            '*eoi*
<<ERROR>>                          (lexer-error (conc yyline ": illegal character") (yygetc))
