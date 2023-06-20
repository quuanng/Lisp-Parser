(*

  This file contains definitions for the OCaml type THING, and the OCaml module
  SCANNER, both of which are needed to write the module PARSER.

*)

(* THING. Types of the usual Lisp objects. *)

type
  thing =
    Closure of thing * thing * environment |
    Cons of thing * thing |
    Nil |
    Number of int |
    Primitive of (thing -> environment -> thing) |
    Symbol of string
and
  environment = (string * thing) list ;;

(* SCANNER. Lexical scanner for Lisp from Lab 9. It also ignores comments. *)

module Scanner =
struct

(* TOKEN. A token for an expression in a subset of Lisp. *)

  type token =
    CloseParenToken |
    EndToken |
    NumberToken of int |
    OpenParenToken |
    SymbolToken of string ;;

(* MAKE SCANNER. Return a version of the scanner function NEXT TOKEN that reads
   TOKENs from a file whose pathname is the string PATH. INPUT is a channel
   connected to the file. CH holds the most recently read CHAR from INPUT. *)

  let makeScanner path =
    let input = open_in path
    in let ch = ref ' '
       in

(* NEXT CHAR. Advance CH to the next CHAR from INPUT. If there is no next CHAR,
   then set CH to '\000'. We use this CHAR to represent the end of a file. We'd
   like to give this CHAR a name, but then we couldn't MATCH on that name. *)

  let nextChar () =
    try ch := input_char input
    with End_of_file ->
           ch := '\000'
  in

(* NEXT CLOSE PAREN TOKEN. Read a CLOSE PAREN TOKEN. *)

  let nextCloseParenToken () =
    nextChar () ;
    CloseParenToken
  in

(* NEXT COMMENT. Skip a comment. It starts with a ';' and ends with a newline
   '\n' or an end of file '\000'. We skip the '\n', but not the '\000'. *)

  let rec nextComment () =
    match ! ch
    with '\000' ->
           () |
         '\n' ->
           nextChar () |
         _ ->
           nextChar () ;
           nextComment ()
  in

(* NEXT END TOKEN. Read an END TOKEN. We don't skip a CHAR because there are no
   more CHARs to skip. *)

  let nextEndToken () =
    EndToken
  in

(* NEXT NUMBER TOKEN. Read a NUMBER TOKEN that starts with PREFIX. *)

  let nextNumberToken prefix =
    let rec nextNumbering chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')' ->
             NumberToken (int_of_string chars) |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextNumbering (chars ^ otherChars)
    in nextNumbering prefix
  in

(* NEXT OPEN PAREN TOKEN. Read an OPEN PAREN TOKEN. *)

  let nextOpenParenToken () =
    nextChar () ;
    OpenParenToken
  in

(* NEXT SYMBOL TOKEN. Read a SYMBOL TOKEN that starts with PREFIX. *)

  let nextSymbolToken prefix =
    let rec nextSymboling chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')' ->
             SymbolToken chars |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextSymboling (chars ^ otherChars)
    in nextSymboling prefix
  in

(* NEXT NUMBER OR SYMBOL TOKEN. We've just read a '-', but we don't know yet if
   it starts a NUMBER TOKEN or a SYMBOL token. Skip the '-'. If we see a digit,
   then it starts a NUMBER TOKEN, otherwise it starts a SYMBOL TOKEN. *)

  let nextNumberOrSymbolToken () =
    nextChar () ;
    match ! ch
    with '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
           nextNumberToken "-" |
         _ ->
           nextSymbolToken "-"
  in

(* NEXT TOKEN. Look at CH to tell what TOKEN is coming next. Dispatch to the
   function that will read that TOKEN and return it. *)

  let rec nextToken () =
    match ! ch
    with '\000' ->
           nextEndToken () |
         ' ' | '\n' ->
           nextChar () ;
           nextToken () |
         '(' ->
           nextOpenParenToken () |
         ')' ->
           nextCloseParenToken () |
         ';' ->
           nextComment () ;
           nextToken () |
         '-' ->
           nextNumberOrSymbolToken () |
         '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
           nextNumberToken "" |
         _ ->
           nextSymbolToken ""

  in nextChar () ;
     nextToken ;;

end ;;

(* The Parsers module type declaration. It declares the exception 'Can'tParse' which is raised when parsing fails,
   and the function 'makeParser' which takes a string as an argument (representing the file path to parse) and 
   returns a thing. *)

module type Parsers =
sig
	exception Can'tParse of string
	val makeParser : string -> unit -> thing
end

(* Parser module implementing the Parsers module type. It provides the actual implementation of the 'makeParser' function. *)

module Parser : Parsers =
struct
	(* Exception that's thrown when there's a parsing error. *)
	exception Can'tParse of string;;
	
  (* The makeParser function takes a file path as a parameter, tokenizes the file content and parses the tokens into 'thing' data structures. *)
	let makeParser path = 
   (* Creating a scanner from the given file path. *)
	 let nextToken = (Scanner.makeScanner path)
	  in
     (* Initializing the token reference to EndToken. *)
	   let tok = ref Scanner.EndToken
	    in
	(* A recursive helper function that reads tokens and constructs 'thing' data structures. *)	
	let rec nextThing () =
   (* Based on the current token type, a different type of 'thing' is returned. *)
	 match !tok with
	  Scanner.NumberToken n -> tok := nextToken (); (Number n) |
	  Scanner.OpenParenToken -> tok := nextToken (); (nextThings ()) |
	  Scanner.SymbolToken "nil" -> tok := nextToken (); Nil |
	  Scanner.SymbolToken s -> tok := nextToken (); (Symbol s) |
    (* If CloseParenToken or EndToken is encountered, an exception is raised. *)
	  Scanner.CloseParenToken -> raise (Can'tParse "Unable to parse")|
	  Scanner.EndToken -> raise (Can'tParse "Unexpected end of file")
		
  (* A recursive helper function that constructs a list of 'things' until a closing parenthesis or the end of file is encountered. *)  
	and nextThings () = 
	  match !tok with
     (* If EndToken is encountered, an exception is raised. *)
	   Scanner.EndToken -> raise (Can'tParse "Unexpected end of file") |
     (* If CloseParenToken is encountered, token advances and returns Nil. *)
	   Scanner.CloseParenToken -> tok := nextToken (); Nil |
     (* For any other token, a 'thing' is constructed and added to the list. *)
	   _ -> let x = nextThing () in Cons(x, nextThings ())
  (* Initiating the first token read, and returning the constructed 'thing'. *)
	in tok := nextToken (); nextThing ;;

end;;
