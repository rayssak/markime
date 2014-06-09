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

%header (functor MarkimeLexFun(structure Tokens: Markime_TOKENS));
digit= [0-9];
alpha= [ A-Za-z];
special=[\+\,\.\;\:\~\^\!\?];
alls = {alpha}|{digit}|{special};
bad_words = [wrongword |wrongword2];

blank = [ ];
ws = [\t];
%s DEFAULT COMMENT MATH;

%%

<INITIAL>{blank}*		=> (YYBEGIN DEFAULT; continue ());
<DEFAULT>{blank}*"//"		=> (YYBEGIN COMMENT; continue ());
<DEFAULT>{blank}*"$$"		=> (YYBEGIN MATH; continue ());
<DEFAULT>{blank}*{ws}+		=> (lex());

<DEFAULT>{bad_words}+   => (error ("stop-words encontradas "^yytext,!pos,!pos);continue ()); 

<DEFAULT>{blank}*"#"		=> (Tokens.COMMAND_START(!pos,!pos));
<DEFAULT>{blank}*"##"		=> (Tokens.COMMAND_END(!pos,!pos));
<DEFAULT>{blank}*"*"		=> (Tokens.IT(!pos,!pos));
<DEFAULT>{blank}*"**"		=> (Tokens.NEG(!pos,!pos));
<DEFAULT>{blank}*"***"		=> (Tokens.ITNEG(!pos,!pos));
<DEFAULT>{blank}*"+"		=> (Tokens.CONCAT(!pos,!pos));
<DEFAULT>{blank}*"("		=> (Tokens.PAR_OPEN(!pos,!pos));
<DEFAULT>{blank}*")"		=> (Tokens.PAR_CLOSE(!pos,!pos));
<DEFAULT>{blank}*"\\it"		=> (Tokens.ITENIZE(!pos,!pos));
<DEFAULT>{blank}*"[link]"	=> (Tokens.LINK(!pos,!pos));
<DEFAULT>{blank}*"\""	        => (Tokens.ASPAS(!pos,!pos));
<DEFAULT>"--"			=> (Tokens.PARAGRAPH(yytext, !pos,!pos));
<DEFAULT>\n			=> (Tokens.SEMI(!pos,!pos));
<DEFAULT>{alls}+		=> (Tokens.TXT(yytext,!pos,!pos));

<COMMENT>\n			=> (YYBEGIN DEFAULT; continue ());
<COMMENT>.			=> (continue ());

<MATH>{blank}*"$$"		=> (YYBEGIN DEFAULT; continue ());
<MATH>{blank}*"+"		=> (Tokens.PLUS(!pos,!pos));
<MATH>{blank}*"*"		=> (Tokens.TIMES(!pos,!pos));
<MATH>{blank}*"-"		=> (Tokens.SUB(!pos,!pos));
<MATH>{blank}*"/"		=> (Tokens.DIV(!pos,!pos));
<MATH>{blank}{digit}+		=> (Tokens.TXT(yytext,!pos,!pos));    
