(* calc.sml *)

structure Markime : sig
	           val parse : unit -> unit
                 end = 
struct

(* 
 * We apply the functors generated from calc.lex and calc.grm to produce
 * the MarkimeParser structure.
 *)

  structure MarkimeLrVals =
    MarkimeLrValsFun(structure Token = LrParser.Token)

  structure MarkimeLex =
    MarkimeLexFun(structure Tokens = MarkimeLrVals.Tokens)

  structure MarkimeParser =
    Join(structure LrParser = LrParser
	 structure ParserData = MarkimeLrVals.ParserData
	 structure Lex = MarkimeLex)

(* 
 * We need a function which given a lexer invokes the parser. The
 * function invoke does this.
 *)

  fun invoke lexstream =
      let fun print_error (s,i:int,_) =
	      TextIO.output(TextIO.stdOut,
			    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
       in MarkimeParser.parse(0,lexstream,print_error,())
      end

  fun parse () = 
	
        let
	    val filenamein   =  "teste.in" : string
	    val filenameout  =  "teste.tex" : string
	    val inStream     =  TextIO.openIn  filenamein
            val outStream    =  TextIO.openOut filenameout;
	
	val grab : int -> string = fn 
            n => if TextIO.endOfStream inStream 
                 then ""
                 else TextIO.inputN (inStream,n);

	val lexer = MarkimeParser.makeLexer (grab)
	  val dummyEOF = MarkimeLrVals.Tokens.EOF(0,0)
	  val dummySEMI = MarkimeLrVals.Tokens.SEMI(0,0)
	  
	  val _ = TextIO.output(outStream, "\\documentclass[12pt]{article} \n\\usepackage[latin1,utf8]{inputenc} \n\\usepackage[portuges]{babel} \n\\usepackage[pdftex]{graphicx}  \n\\graphicspath{{figures/}}  \n\n")
	  val _ = TextIO.closeOut outStream;
	
	  fun loop lexer =
	      let val (result,lexer) = invoke lexer
		  val (nextToken,lexer) = MarkimeParser.Stream.get lexer
		  
		  val _ = case result
			    of SOME r =>
				()
			     | NONE => ()
	       		in 
			  if MarkimeParser.sameToken(nextToken,dummyEOF) 
				then 
				    TextIO.output(TextIO.stdOut,"\n\nLatex (" ^ filenameout ^ ") gerado a partir de (" ^ filenamein ^ ") com sucesso!!!\n\n")
		  	  else loop lexer
	      end
       in loop lexer
      end
end (* structure Markime *)
