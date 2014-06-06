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

blank = [ ];
ws = [\t];
%s DEFAULT COMMENT;

%%

<INITIAL>		=> (YYBEGIN DEFAULT; continue ());
<DEFAULT>"//"		=> (YYBEGIN COMMENT; continue ());
<DEFAULT>{ws}+		=> (lex());

<DEFAULT>{bad_words}+   => (error ("ignoring bad words "^yytext,!pos,!pos);
             lex()); 

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
<DEFAULT>\n			=> (Tokens.SEMI(!pos,!pos));
<DEFAULT>{alls}+		=> (Tokens.TXT(yytext,!pos,!pos));

<COMMENT>\n		=> (YYBEGIN DEFAULT; continue ());
<COMMENT>.		=> (continue ());
