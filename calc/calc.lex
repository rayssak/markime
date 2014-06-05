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
bad_words = [shit|shot |shit |shot];

ws = [\t];
%s DEFAULT COMMENT;

%%

<INITIAL>		=> (YYBEGIN DEFAULT; continue ());
<DEFAULT>"//"		=> (YYBEGIN COMMENT; continue ());
<DEFAULT>{ws}+		=> (lex());

<DEFAULT>{bad_words}+   => (error ("ignoring bad words "^yytext,!pos,!pos);
             lex()); 

<DEFAULT>"#"		=> (Tokens.COMMAND_START(!pos,!pos));
<DEFAULT>"##"		=> (Tokens.COMMAND_END(!pos,!pos));
<DEFAULT>"*"		=> (Tokens.IT(!pos,!pos));
<DEFAULT>"**"		=> (Tokens.NEG(!pos,!pos));
<DEFAULT>"***"		=> (Tokens.ITNEG(!pos,!pos));
<DEFAULT>"+"		=> (Tokens.CONCAT(!pos,!pos));
<DEFAULT>"("		=> (Tokens.PAR_OPEN(!pos,!pos));
<DEFAULT>")"		=> (Tokens.PAR_CLOSE(!pos,!pos));
<DEFAULT>"\\it"		=> (Tokens.ITENIZE(!pos,!pos));
<DEFAULT>"[link]"	=> (Tokens.LINK(!pos,!pos));
<DEFAULT>"\""	        => (Tokens.ASPAS(!pos,!pos));
<DEFAULT>\n		=> (Tokens.SEMI(!pos,!pos));
<DEFAULT>{alls}+	=> (Tokens.TXT(yytext,!pos,!pos));

<COMMENT>\n		=> (YYBEGIN DEFAULT; continue ());
<COMMENT>.		=> (continue ());
