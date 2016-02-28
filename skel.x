<program> ::=						Skel program
	  "program" <id> ":" <pars> "."

<pars> ::=
      <par> [";" <pars>]				parallel statements

<par> ::=
	"func" <id> <structs>                   	structured expression
|	<pars> "||" <pars>                          	parallel pipeline
|	"farm" <int> <pars>                       	task farm

<structs> ::=
	<struct> [";" <structs>]                  	statements

<struct> ::=
      <exprs>						expression
|     <structs> "•" <structs>				composition
|     "iter" <int> <structs>                    	iteration

<exprs> ::=
      <expr> ["," <exprs>]				expressions

<expr> ::=
           <int>					integer value
	|  <string>					string value
	|  <bool>					boolean value
	|  <id> [ "=" <exprs> ]			identifier/assignment
	|  "raise" <id> "=" <exprs>			raise exception
  	|  <exprs> "catch" <id> <id> ":" <exprs>	catch exception
	|  <exprs> <op> <exprs>	     	 	binary operator
	|  "(" <exprs> ")"				grouping

<op> ::=						operators
	"+" | "*" | "-" | "div"| "<"| "<=" | "==" | "!="
