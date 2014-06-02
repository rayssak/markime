structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
fun eof () = Tokens.EOF(!pos,!pos)
fun error (e,l, k) = TextIO.output (TextIO.stdOut, String.concat[
	"line ", (Int.toString l), ": ", e, "\n"
      ])

%%

%header (functor CalcLexFun(structure Tokens: Calc_TOKENS));
alpha=[ \(\)\,\.0-9A-Za-z\[\]];

bad_words = ["shit|shot |shit |shot "];

ws = [\t];
%s DEFAULT COMMENT;

%%

<INITIAL>		=> (YYBEGIN DEFAULT; continue ());
<DEFAULT>"//"		=> (YYBEGIN COMMENT; continue ());
<DEFAULT>{ws}+		=> (lex());

<DEFAULT>{bad_words}+ => (error ("ignoring bad words "^yytext,!pos,!pos);
             lex()); 

<DEFAULT>"*"		=> (Tokens.IT(!pos,!pos));
<DEFAULT>"**"		=> (Tokens.NEG(!pos,!pos));
<DEFAULT>\n		=> (Tokens.SEMI(!pos,!pos));
<DEFAULT>{alpha}+	=> (if yytext="print"
			      then Tokens.PRINT(!pos,!pos)
			    else if yytext="[link]"
			      then Tokens.LINK(!pos,!pos)                 
			    else Tokens.TXT(yytext,!pos,!pos)
			    );
<DEFAULT>"//"		=> (Tokens.COMMENT(!pos,!pos));

<COMMENT>\n		=> (YYBEGIN DEFAULT; continue ());
<COMMENT>.		=> (continue ());