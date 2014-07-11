{
--add support for list matching
--add support for char vs string
--add support for default argument values for ex fib
--add ^ for super      ( ^stmt )
--add ???? for foreign (   emtest.stmt )
--binding to a variable twice? does it work properly?
--nested choice in ()

	tk   n    = <ws> <letter>:a <ldu>*:b $(a&b):c ?(equal(n,c)) <ws>          $(c)
 spacea     = [ \t\r\n]                                                     $({})
 space      = [ \t]                                                         $({})
 eof        = <_>!
 eol        = '\r''\n'* | '\n''\r'*                                         $({})
 slashn     = '\n'
 nslashn    = <slashn>! <_>
 ns         = <eol>! <_>
 ne         = <eof>! <_>
 comtag     = '--' | '=>'                                                   $({})
 comment    = <comtag> <ns>* <eol>                                          $({})
            | <comtag> <ne>* <eof>                                          $({})
 wc         = <space> | <comment> | <eol>                                   $({})
 ws         = <wc>*                                                         $({})
 cs         = <space>*                                                      $({})
 eid        = [a-zA-Z]:a [a-zA-Z0-9_]*:b                                    $(a&b)
 squote     = '\''
 dquote     = '\"'
 nq         = <squote>! <_>
 ndq        = <dquote>! <_>
 rsbrace    = ']'
 nsb        = <squote>! <rsbrace>! <_>
 echar      = '\\t'                                                         $('\t')
            | '\\r'                                                         $('\r')
            | '\\n'                                                         $('\n')
            | '\\\''                                                        $('\'')
            | '\\\"'                                                        $('\"')
            | '\\\\'                                                        $('\\')
 dqchar     = <echar> | <ndq>
 qchar      = <echar> | <nq>
 qchar2     = <echar> | <nsb>
 mod        = '*'  $("*") --zero or more
            | '+'  $("+") --one or more
            | '!'  $("!") --not true lookahead (consumes nothing)
            | '&'  $("&") --true lookahead (consumes nothing)
 qchars     = <qchar>+
 dqchars    = <dqchar>*
 mchar      = '\'' <qchars>:chars '\''                                      $({"char",chars})
 mchar2     = '\"' <dqchars>:chars '\"'                                     $({"eqval",chars})
 rpart      = <qchar>:low '-' <qchar>:high                                  $(low&high)
            | <qchar2>:single                                               $(single&single)
 srpart     = <rpart>*:x                                                    $(x)
 rchar      = '[' <srpart>:ranges ']'                                       $({"ranges",ranges})
-- ochar    = '<' <eid>:id <hexps>*:args '>'                                $({"producer",id,args})
--  ochar   = '<' <eid>:id <hexps>*:args '>'                                $({"producer2",{id,args}})
 ochar      = '<'            <ws>               <eid>:id <ws> '>' $({       "producer",     id})
	          | '<' <tk("super")>                 <eid>:id <ws> '>' $({  "superproducer", {   id ,{}}})
	          | '<' <tk("foreign")> <eid>:p  <ws> <eid>:id                                        <ws> '>' $({"foreignproducer", {{p,id},{}}})
 					  | '<'            <ws>               <eid>:id <ws> '(' <ws> <equ_list>:args <ws> ')' <ws> '>' $({       "producer2",{   id ,args}})
	          | '<' <tk("super")>                 <eid>:id <ws> '(' <ws> <equ_list>:args <ws> ')' <ws> '>' $({  "superproducer", {   id ,args}})
	          | '<' <tk("foreign")> <eid>:p  <ws> <eid>:id <ws> '(' <ws> <equ_list>:args <ws> ')' <ws> '>' $({"foreignproducer", {{p,id},args}})
            | '<_>'                                                         $({"ranges",{{0,255}}})
--  sochar     = '{' <eid>:id '}'                                              $({"sproducer",id})
--          | '{_}'
 query      = '?' <epar>:q                                                  $({"query",q})
 hequ       = '$' <epar>:q                                                  $({"host2",q})
 sxp        = <hex_val>:h                                                   $({"char",h})
            | <int>:h                                                       $({"char",h})
            | <query>
            | <hequ>
 					  | <mchar>
            | <mchar2>
            | <ochar>
--             | <sochar>
            | <rchar>
            | <pexp>
            | <lexp> --list expression
 sxpmod     = <sxp>:exp <mod>:mod ':' <eid>:id                              $({mod,{exp},id})
            | <sxp>:exp <mod>:mod                                           $({mod,{exp}})
            | <sxp>:exp           ':' <eid>:id                              $(exp&{id})
            | <sxp>:exp '`' <call_ret>:rep ':' <eid>:id                     $({"rep",{rep,{exp}},id})
            | <sxp>:exp '`' <call_ret>:rep                                  $({"rep",{rep,{exp}}})
            | <sxp>:exp                                                     $(exp)
 pexp       = <ws> '(' <ws> <mget_exp>:x <ws> ')'                           $({"dofirst",x})
 lexp       = <ws> '{' <ws> <mget_exp>:x <ws> '}'                           $({"dolist",x})
 exps       = <exps>:exp1 <ws> <sxpmod>:exp2                                $(exp1&{exp2})
            | <ws> <sxpmod>:exp                                             $({exp})
 hexps      = <exps>:exp <host2>:host2                                      $(exp&{host2})
            | <exps>:exp                                                    $(exp)
 dqtext     = '\"' <dqchars>:text '\"'                                      $(text)
 number     = [0-9]*
 ldig       = [0-9]:a                                                       $(a-'0')
 ldigs      = <ldigs>:a <ldig>:b                                            $(a*10+b)
            | <ldig>:b                                                      $(b)
  hex_letter = [A-F]
             | [0-9]
  hex_num   = '#' <hex_letter>+
  hdig      = [0-9]:a                                                       $(a-'0')
            | [A-F]:a                                                       $(a-'A'+10)
  hdigs     = <hdigs>:a <hdig>:b                                            $(a*16+b)
            | <hdig>:b                                                      $(b)
  hex_val   = '#' <hdigs>:a $(a)
  digit     = [0-9]
  int       = '+' <ldigs>:x                                                 $(x)
            | '-' <ldigs>:x                                                 $(0-x)
            | <ldigs>:x                                                     $(x)
  num       = <int> '.' <ldigs> [eE] <int>
            | <int> '.' <ldigs>
            | <int> [eE] <int>
            | <int>
            | '.' <ldigs> [eE] <int>
            | '.' <ldigs>
  qsf       = '\"'
            | '\n'
            | '\r'
            | '\t'
  qaf       = '\''
            | '\n'
            | '\r'
            | '\t'
  qsa       = <qsf>! '\\n'                                                  $('\n')
            | <qsf>! '\\r'                                                  $('\r')
            | <qsf>! '\\t'                                                  $('\t')
            | <qsf>! '\\\\'                                                 $('\\')
            | <qsf>! '\\\"'                                                 $('\"')
            | <qsf>! '\\\''                                                 $('\'')
  qaa       = <qsf>! '\\n'                                                  $('\n')
            | <qsf>! '\\r'                                                  $('\r')
            | <qsf>! '\\t'                                                  $('\t')
            | <qsf>! '\\\\'                                                 $('\\')
            | <qsf>! '\\\"'                                                 $('\"')
            | <qsf>! '\\\''                                                 $('\'')
  qsc       = <qsa>
            | <qsf>! '\\'! <_>
  anyc      = <_>
  qac       = <qaa>:x                                                       $(x)
            | <qaf>! '\\'! <anyc>:x                                         $(x)
  mqsc      = <qsc>*
  qs        = '\"' <mqsc>:x '\"'                                            $(x)
  qa        = '\'' <qac>:x '\''                                             $(x)
  epar      = '(' <ws> <equ>:x <ws> ')'                                     $({{"return",x}} )
  call_func = <ws> <eid>:x <ws> '(' <ws> ')'                                $({"func",x,{}})
            | <ws> <eid>:x <ws> '(' <ws> <equ_list>:y <ws> ')'              $({"func",x,y})
  sub_script = <ws> '[' <ws> <equ>:s <ws> ']' <ws>                          $({s})
             | <ws> '[' <ws> <equ>:s <ws> '..' <ws> <equ>:f <ws> ']' <ws>   $({s,f})
  call_ret  = <call_func>:x                                                 $(x)
            --| <call_type>
            |  '(' <ws> <equ>:x <ws> ')'                                    $({"par",x} )
--            | <var> <ws> <sub_script>* <ws>
            | <eid>:v <ws> <sub_script>*:s <ws>                             $({"subs",{{"id",v},s}})
            | <eid>:v                                                       $({"id",v,s})
            | <num>:x                                                       $({"lit",x})
            | <hex_num>:x                                                   $({"lit",x})
            | <qs>:x                                                        $({"lit",x})
            | <qa>:x                                                        $({"lit",x})
            --| '$'                                                         -> '{\"$\"}')
            | <seq>:x                                                       $(x)
  letter    = [a-zA-Z]
  ldu       = <letter> | <digit> | '_'
  upn       = '+'   <ws> <call_ret>:x                                       $({"pos",x})
            | '-'   <ws> <call_ret>:x                                       $({"neg",x})
            | <tk("not")> <call_ret>:x                                      $({"not",x})
            | <call_ret>
  mul       = <mul>:x <ws> '*' <ws> <upn>:y                                 $({"mul",{x,y}})
            | <mul>:x <ws> '/' <ws> <upn>:y                                 $({"div",{x,y}})
            | <upn>
  add       = <add>:x <ws> '+' <ws> <mul>:y                                 $({"add",{x,y}})
            | <add>:x <ws> '-' <ws> <mul>:y                                 $({"sub",{x,y}})
            | <mul>
  concat    = <concat>:x <ws> '&' <ws> <add>:y                              $({"concat",{x,y}})
            | <add>
  cmp       = <cmp>:x <ws> '<'  <ws> <concat>:y                             $({"lt", {x,y}})
            | <cmp>:x <ws> '>'  <ws> <concat>:y                             $({"gt", {x,y}})
            | <cmp>:x <ws> '<=' <ws> <concat>:y                             $({"le",{x,y}})
            | <cmp>:x <ws> '>=' <ws> <concat>:y                             $({"ge",{x,y}})
            | <cmp>:x <ws>  '=' <ws> <concat>:y                             $({"eq" ,{x,y}})
            | <cmp>:x <ws> '!=' <ws> <concat>:y                             $({"ne",{x,y}})
            | <concat>
  logic     = <logic>:x <tk("and")> <cmp>:y                                 $({"and",{x,y}})
            | <logic>:x <tk("xor")> <cmp>:y                                 $({"xor",{x,y}})
            | <logic>:x <tk("or")> <cmp>:y                                  $({ "or",{x,y}})
            | <cmp>
  equl      = <ws> ',' <ws> <equ>:x <ws>                                    $(x)
  mequl     = <equl>*:x                                                     $(x)
  equ_list  = <ws> <equ>:x <ws> <mequl>:y  <ws>                             $({x}&y)
  seq       = '{' <ws> '}'                                                  $({"seq",{}})
            | '{' <ws> <equ_list>:x <ws> '}'                                $({"seq",x})
  equ       = <ws> <logic>:x <ws>                                           $(x)
 host2_stmt = <ws> 'return' <ws> <equ>:exp                                  $({"return",exp})
 host2_code = <ws> <host2_code>:stmt1 <ws> <host2_stmt>:stmt2               $(stmt1&{stmt2})
            | <ws> <host2_stmt>:stmt                                        $({stmt})
 host2      = <ws> '->' <ws> <host2_code>:host2 <ws> '-<'                   $({"host2",host2})
 mget_exp_old   = <mget_exp>:exp1 <ws> '|' <ws> <hexps>:exp2                $({{"choice",exp1}}&exp2)
            | <hexps>:exp                                                   $(exp)
 choices    = <ws> '|' <ws> <hexps>:exp2                                    $(exp2)
 mget_exp   = <mget_exp>:exp1 <choices>+:exp2                               $({{"choices",{exp1}&exp2}})
            | <hexps>:exp                                                   $(exp)
 eids       = <ws> <eid>
 parser     = <ws> <eids>:id <eids>*:args <ws> '=' <mget_exp>:exp           $({id,exp,args})
 sparser    = <parser>*:p                                                   $(p)
 eat        = <ws> '{' <ws> <sparser>:parsers <ws> '}' <ws>                 $(parsers)
 named_p    = <ws> <eid>:a                             <eat>:p              $({a,"",p})
            | <ws> <eid>:a <ws> '<:' <ws> <eid>:b <ws> <eat>:p              $({a,b ,p})
}