sequence tparsers

procedure get_eschar(atom schar)
	if not get_schar(schar) then
		error("expected "&schar)
	end if
end procedure

function get_lchar(sequence lchar)
	if find(get_char(),lchar) then return 1 end if
	unget_char()
	return 0
end function

function is_lower_letter(atom c)
	return c>='a' and c<='z'
end function

function is_upper_letter(atom c)
	return c>='A' and c<='Z'
end function

function is_letter(atom c)
	return is_lower_letter(c) or is_upper_letter(c)
end function

function is_number(atom c)
	return c>='0' and c<='9'
end function

function get_eid()
	atom c
	sequence ret
	if peek_char()='_' then return {get_char()} end if
	if not is_letter(peek_char()) then return 0 end if
	ret={get_char()}
	while is_letter(peek_char()) or is_number(peek_char()) or peek_char()='_' do
		ret&=get_char()
	end while
	return ret
end function

function get_qchar()
	atom char
	atom ret
	char=get_char()
	if find(char,"\'\"\r\n\t") then unget_char() error(" not expected") end if
	if char!='\\' then return char end if
	char=get_char()
	if char='t' then return '\t' end if
	if char='r' then return '\r' end if
	if char='n' then return '\n' end if
	if char='\'' then return '\'' end if
	if char='\"' then return '\"' end if
	if char='\\' then return '\\' end if
	unget_char()
	error(" expected t r n \' \" \\ ")
	return 0
end function

function get_qchars()
	sequence ret
	ret={get_qchar()}
	while peek_char()!='\'' do
		ret&=get_qchar()
	end while
	return ret
end function

function digm()
sequence ret
	ret={}
	while is_number(peek_char()) do
		ret&=get_char()
	end while
	return ret
end function

function digval(sequence a)
	atom ret
	ret=0
	for i = 1 to length(a) do
		ret*=10
		ret+=a[i]-'0'
	end for
	return ret
end function

function get_atom()
	atom sign,hit_point,ichar,mantissa,exp,hit_exp
	sequence ret,whole,part,exps
	sign=+1
	mantissa=0
	exp=0
	hit_point=0
	hit_exp=0

	if    get_schar('+') then sign=+1
	elsif get_schar('-') then sign=-1
	end if

	whole=digm()
	if get_schar('.') then
		part=digm()
		if not length(part) then error("invalid number format, expected digits after decimal point")
		end if
	else
		if not length(whole) then error("invalid number format, expected number")
		end if
		part={}
	end if
	if get_schar('E') or get_schar('E') then
		exps=digm()
		if not length(exps) then error("invalid number format, expected exponent after 'E'")
		end if
	else
		exps={}
	end if
	mantissa=digval(whole&part)
	exp=digval(exps)-length(part)
	if not exp then return mantissa end if
	if exp<0 then return mantissa/power(10,-exp) end if
	if exp>0 then return mantissa*power(10,exp) end if
end function

function get_dqchars()
	sequence ret
	ret={get_qchar()}
	while peek_char()!='\"' do
		ret&=get_qchar()
	end while
	return ret
end function

function till_char(sequence c)
sequence ret
	ret={}
	while not find(peek_char(),c&-1) do
		ret&=get_char()
	end while
	return ret
end function

procedure skip_whitespace()
sequence comment
	while 1 do
		if get_lchar(" \t\r\n") then
		elsif get_schars("--") then comment=till_char("\n")
		else
			exit
		end if
	end while
end procedure

procedure skip_space()
sequence comment
	while 1 do
		if get_lchar(" \t\r") then
		elsif get_schars("--") then comment=till_char("\n") exit
		else
			exit
		end if
	end while
end procedure

function get_exp()
sequence range,rgs
atom char1
	if get_schar('\'') then
		parsers[$][2]=append(parsers[$][2],{"char",get_qchars()})
		--print(1,parsers[$][2][$][2])
		--puts(1,32)
		get_eschar('\'')
		return 1
	elsif get_schar('[') then
		range={"ranges",{}}
		rgs={}
		while not get_schar(']') do
			char1=get_qchar()
			--print(1,range[$][$])
			if get_schar('-') then
				--puts(1,'-')
				rgs=append(rgs,{char1,get_qchar()})
				--print(1,range[$][$])
				--puts(1,32)
			else
				rgs=append(rgs,{char1,char1})
			end if
		end while
		range[2]=rgs
		if get_schar(':') then
			--puts(1," : ")
			range=append(range,get_eid())
			--puts(1,32&producer[$]&32)
		end if
		parsers[$][2]=append(parsers[$][2],range)
		return 1
	else
		--error("expression expected")
		return 0
	end if
end function

function get_host2_exp(atom pres)
	sequence ret,seq
	sequence op
	ret={}
	op="lit"
	while length(op) do
		skip_whitespace()
 		if is_letter(peek_char()) then
 			ret={"id",get_eid()}
		elsif get_schar('{') then
			seq={}
			if get_schar('}') then
			else
				while 1 do
					seq=append(seq,get_host2_exp(0))
					if not get_schar(',') then exit end if
				end while
				if not get_schar('}') then error("expected '}'") ? 1/0 end if
			end if
			ret={"seq",seq}
		elsif get_schar('\"')  then
			ret={"lit",get_dqchars()}
			get_eschar('\"')
		elsif get_schar('\'')  then
			ret={"lit",get_qchar()}
			get_eschar('\'')
		elsif find(peek_char(),"0123456789+-.") then
			if get_schars("-<") then unget_char() unget_char() exit end if
			ret={"lit",get_atom()}
		else
			exit
		end if
		if equal(op,"lit") then
			op={}
		end if
		while 1 do
			skip_whitespace()
			if    get_schar('-') then
				if pres>0 then unget_char() exit end if
				if get_schar('<') then unget_char() unget_char() exit end if
				ret={"sub",{ret,get_host2_exp(0)}}
				op={"lit"}
			elsif get_schar('+') then
				if pres>0 then unget_char() exit end if
				ret={"add",{ret,get_host2_exp(0)}}
				op={"lit"}
			elsif get_schar('*') then
				ret={"mul",{ret,get_host2_exp(1)}}
				op={"lit"}
			elsif get_schar('/') then
				ret={"div",{ret,get_host2_exp(1)}}
				op={"lit"}
			elsif get_schar('&') then
				if pres>0 then unget_char() exit end if
				ret={"concat",{ret,get_host2_exp(0)}}
				op={"lit"}
			else
				exit
			end if
		end while
	end while
	return ret
end function

function get_host2_code()
	sequence ret
	ret={}
	while 1 do
		skip_whitespace()
		if get_schars("return") then
			ret=append(ret,{"return",get_host2_exp(0)})
			exit
		else
			exit
		end if
-- 		if get_schar('(') then
-- 			ret=append(ret,{"char",'('})
-- 			ret&=get_host2_code()
-- 			skip_space()
-- 			get_eschar(')')
-- 			ret=append(ret,{"char",')'})
-- 		elsif get_schar ('*' )  then ret=append(ret,{"mul"   }) ret&=get_host_code()
-- 		elsif get_schar ('/' )  then ret=append(ret,{"div"   }) ret&=get_host_code()
-- 		elsif get_schar ('+' )  then ret=append(ret,{"add"   }) ret&=get_host_code()
-- 		elsif get_schar ('&' )  then ret=append(ret,{"concat"}) ret&=get_host_code()
-- 		elsif get_schar ('-' )  then ret=append(ret,{"sub"   }) ret&=get_host_code()
-- 		elsif get_schar ('=' )  then ret=append(ret,{"eq"    }) ret&=get_host_code()
-- 		elsif get_schar ('<' )  then ret=append(ret,{"lt"    }) ret&=get_host_code()
-- 		elsif get_schar ('>' )  then ret=append(ret,{"gt"    }) ret&=get_host_code()
-- --		elsif get_schar ('\'')  then ret=append(ret,{"qa",get_qchars()})  get_eschar('\'') ret&=get_host_code()
-- 		elsif get_schars("<=")  then ret=append(ret,{"le"    }) ret&=get_host_code()
-- 		elsif get_schars(">=")  then ret=append(ret,{"ge"    }) ret&=get_host_code()
-- 		elsif get_schars("!=")  then ret=append(ret,{"ne"    }) ret&=get_host_code()
-- 		elsif get_schars("not") then ret=append(ret,{"not"   }) ret&=get_host_code()
-- 		elsif get_schars("and") then ret=append(ret,{"and"   }) ret&=get_host_code()
-- 		elsif get_schars("or")  then ret=append(ret,{"or"    }) ret&=get_host_code()
-- 		elsif get_schars("xor") then ret=append(ret,{"xor"   }) ret&=get_host_code()
-- -- 		elsif get_schar('?') then
-- -- 			ret=append(ret,{"ques"})
-- -- 			ret&=get_host_code()
-- 		elsif is_letter(peek_char()) then
-- 			ret=append(ret,{"id",get_eid()})
-- 		else
-- 			exit
-- 		end if
	end while
	return ret
end function

function get_host_code()
sequence ret
	ret={}
	while 1 do
		skip_space()
		if get_schar('(') then
			ret=append(ret,{"char",'('})
			ret&=get_host_code()
			skip_space()
			get_eschar(')')
			ret=append(ret,{"char",')'})
		elsif get_schar ('*' )  then ret=append(ret,{"mul"   }) ret&=get_host_code()
		elsif get_schar ('/' )  then ret=append(ret,{"div"   }) ret&=get_host_code()
		elsif get_schar ('+' )  then ret=append(ret,{"add"   }) ret&=get_host_code()
		elsif get_schar ('&' )  then ret=append(ret,{"concat"}) ret&=get_host_code()
		elsif get_schar ('-' )  then ret=append(ret,{"sub"   }) ret&=get_host_code()
		elsif get_schar ('=' )  then ret=append(ret,{"eq"    }) ret&=get_host_code()
		elsif get_schar ('<' )  then ret=append(ret,{"lt"    }) ret&=get_host_code()
		elsif get_schar ('>' )  then ret=append(ret,{"gt"    }) ret&=get_host_code()
		elsif get_schar ('\'')  then ret=append(ret,{"qa",get_qchars()})  get_eschar('\'') ret&=get_host_code()
		elsif get_schars("<=")  then ret=append(ret,{"le"    }) ret&=get_host_code()
		elsif get_schars(">=")  then ret=append(ret,{"ge"    }) ret&=get_host_code()
		elsif get_schars("!=")  then ret=append(ret,{"ne"    }) ret&=get_host_code()
		elsif get_schars("not") then ret=append(ret,{"not"   }) ret&=get_host_code()
		elsif get_schars("and") then ret=append(ret,{"and"   }) ret&=get_host_code()
		elsif get_schars("or")  then ret=append(ret,{"or"    }) ret&=get_host_code()
		elsif get_schars("xor") then ret=append(ret,{"xor"   }) ret&=get_host_code()
		elsif get_schar('?') then
			ret=append(ret,{"ques"})
			ret&=get_host_code()
		elsif is_letter(peek_char()) then
			ret=append(ret,{"id",get_eid()})
		else
			exit
		end if
	end while
	return ret
end function

function mget_exp()
atom count
sequence host_out,producer,ins
	count=0
	while 1 do
		skip_whitespace()
		if find(peek_char(),"\'[") then
			if not get_exp() then return count end if
			count+=1
		elsif get_schars("<_>") then
			parsers[$][2]=append(parsers[$][2],{"ranges",{{0,255}}})
			count+=1
		elsif get_schar('<') then
			producer={"producer",get_eid()}
			--puts(1,32&producer[2][1]&32)
			count+=1
			get_eschar('>')
			if get_schar(':') then
				--puts(1," : ")
				producer=append(producer,get_eid())
				--puts(1,32&producer[$]&32)
			end if
			parsers[$][2]=append(parsers[$][2],producer)
		elsif get_schar('{') then
			producer={"sproducer",get_eid()}
			--puts(1,32&producer[2][1]&32)
			count+=1
			get_eschar('}')
			if get_schar(':') then
				--puts(1," : ")
				producer=append(producer,get_eid())
				--puts(1,32&producer[$]&32)
			end if
			parsers[$][2]=append(parsers[$][2],producer)
		elsif get_schar('(') then
			skip_whitespace()
			--puts(1,32&get_eid()&32)
			--skip_whitespace()
			while get_schar('|') do
				skip_whitespace()
				--puts(1,32&get_eid()&32)
				skip_whitespace()
			end while
			count+=1
			get_eschar(')')
		elsif get_schar('*') then
			ins={"*",{parsers[$][2][$]}}
			if get_schar(':') then ins=append(ins,get_eid()) end if
			parsers[$][2][$]=ins
		elsif get_schar('+') then
			ins={"+",{parsers[$][2][$]}}
			if get_schar(':') then ins=append(ins,get_eid()) end if
			parsers[$][2][$]=ins
		elsif get_schar('!') then
			ins={"!",{parsers[$][2][$]}}
			if get_schar(':') then ins=append(ins,get_eid()) end if
			parsers[$][2][$]=ins
		elsif get_schar('|') then
			--puts(1," choice ")
			if not length(parsers[$][2]) then
				error("need expression before and after choice '|'")
			end if
			if 1 then
				parsers[$][2]={{"choice",parsers[$][2]}}
			else
				tparsers=append(tparsers,parsers[$][2])
				parsers[$][2]={{"tchoice",sprintf("#temp%d",length(tparsers))}}
			end if
			if not mget_exp() then error("expression expected") end if
			count+=1
		elsif get_schars("=>") then
			host_out={"host",get_host_code()}
			parsers[$][2]=append(parsers[$][2],host_out)
			skip_whitespace()
			if peek_char()!='|' then return count end if
			--host_out=till_char("\n")
		elsif get_schars("->") then
			host_out={"host2",get_host2_code()}
			parsers[$][2]=append(parsers[$][2],host_out)
			skip_whitespace()
			if not get_schars("-<") then error("expected -<") end if
			skip_whitespace()
			if peek_char()!='|' then return count end if
		else
			return count
		end if
	end while
end function

procedure get_parser()
sequence eid
	while 1 do
		skip_whitespace()
		if get_schar('}') then exit
		elsif is_letter(peek_char()) then
			eid=get_eid()
			parsers=append(parsers,{eid,{}})
			--puts(1,32&eid&32)
			skip_whitespace()
			get_eschar('=')
			--puts(1," = ")
			if not mget_exp() then error("expression expected") end if
			--puts(1,10)
		else
			error("Parser error")
		end if
	end while
end procedure

include seu.e
global sequence eparsers
eparsers=unserial_eu(read_whole_file("default_parser.ser"))

global procedure eat(sequence fn)
atom plogfn
sequence rval
atom lb,lp
	if length(eparsers) then
		parsers=eparsers
		parser_texts={}
		rval=exec("eat",fn,0)
		parsers=rval[2]
		parser_texts={}
		return
	end if
	puts(1,"Old eat\n")

	parsers={}
	parser_texts={}
	tparsers={}
	plogfn=open("plog.txt","w")
	cfn=fn
	src=read_whole_file(cfn)
	parsing_file_line_numbers=get_line_numbers(src)
		
	pos=1
	while 1 do
		skip_whitespace()
		if get_schar(-1) then exit
		elsif get_schar('{') then get_parser()
		else
			error("Parser error")
		end if
	end while
	for i = 1 to length(tparsers) do
		parsers=append(parsers,{sprintf("#temp%d",i),tparsers[i]})
	end for
	parser_texts={}
	tparsers={}
	pretty_print(plogfn,parsers,{2})
	close(plogfn)
-- ( e )
--   list
-- e1 e2
--   sequencing
-- e1 | e2
--   prioritized choice
-- e*
--   zero or more repetitions
-- e+
--   one or more repetitions (not essential)
-- ~e
--   negation
-- <p>
--   production application
-- ’x’
--   matches the character x
-- =>
--   Semantic actions in host language
-- ::=
--   make new rule
-- [a-z]
--   character range

--cRange x y ::= <char>:c ?(>= c x)
--?(<= c y) => c;
--<cRange ’a’ ’z’>
end procedure


global procedure named_p(sequence fn)
sequence ret
	eat("emem_part.e")
	ret=exec("named_p",fn,0) parsers=ret[2][3]-- parsers=optimize_parsers(parsers)
	named_parsers=append(named_parsers,ret[2])
end procedure

global procedure cached_named_ps(sequence cached_file_name, sequence fns)
object np
		np=unserial_file(cached_file_name)
		if not length(np) then
			for i = 1 to length(fns) do
			 	named_p(fns[i])
		 	end for
			serial_file(cached_file_name,{parsers,named_parsers})
		else
			parsers=np[1]
			parser_texts={}
			named_parsers=np[2]
		end if
end procedure