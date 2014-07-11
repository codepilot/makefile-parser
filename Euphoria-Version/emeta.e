without warning
include machine.e
include file.e
include get.e
include graphics.e
--em int

constant trace_msg_=routine_id("trace_msg")
global procedure trace_msg_rid(object a)
	if trace_msg_=-1 then return end if
	call_proc(trace_msg_,{a})
end procedure

global constant trace_msg_pos_=routine_id("trace_msg_pos")
global procedure trace_msg_pos_rid(object a, object b, object c)
	if trace_msg_pos_=-1 then return end if
	call_proc(trace_msg_pos_,{a,b,c})
end procedure

global constant trace_pos_=routine_id("trace_pos")
global procedure trace_pos_rid(object a, object b)
	if trace_pos_=-1 then return end if
	call_proc(trace_pos_,{a,b})
end procedure

global procedure pretty_print_whole_file(sequence fileName, object a, object b)
	atom fn
	fn=open(fileName,"wb")
	pretty_print(fn,a,b)
	close(fn)
end procedure

global constant failed=-1--{0,{}}
global function failed_rule(object rseq)--tests to see if rule failed
	if sequence(rseq) and length(rseq)>=2 and equal(rseq[1],1) then return 0 end if
	return 1
end function


global sequence parsers,named_parsers,parser_name
parsers={}
named_parsers={}
parser_name={}

without trace
global function read_whole_file(sequence fn)
	atom fh,err,size
	sequence ret
	fh=open(fn,"rb")
	if fh=-1 then return {} end if
	err=seek(fh,-1)
	size=where(fh)
	err=seek(fh,0)
	ret=repeat(0,size)
	for i = 1 to size do
		ret[i]=getc(fh)
	end for
	close(fh)
	return ret
end function

global procedure write_whole_file(sequence fn, object a)
	atom fh
	fh=open(fn,"wb")
	if fh=-1 then return end if
	puts(fh,a)
	close(fh)
end procedure
with trace

-- type pos_type(sequence a)
-- 	if find(1,a<1) then return 0 end if
-- 	printf(1,"pos=",{}) ? a
-- 	return 1
-- end type

global sequence cfn
cfn="UNKNOWN"
global sequence parsing_file_name,euincs,parsing_file_line_numbers
parsing_file_name="UNKNOWN"
parsing_file_line_numbers={}
global object src
src={}
global sequence pos
pos={}
global sequence bpos
bpos={}
global atom flat_pos
flat_pos=0
--global pos_type pos 
euincs={}

global function min(atom a, atom b)
	if a<b then return a end if
	return b
end function

global function max(atom a, atom b)
	if a>b then return a end if
	return b
end function

global function fsub(object a, sequence b)--a[b[1]][b[2]]...
	for i = 1 to length(b) do
		if b[i]>length(a) then return 0 end if
		if atom(a) then return 0 end if
		a=a[b[i]]
	end for
	return {1,a}
end function

global function enter_list()
	object ret
	ret=fsub(src,pos)
	if failed_rule(ret) then return 0 end if
	if atom(ret[2]) then return 0 end if
	pos&=1
	return 1
end function

global function exit_list()
	object ret
	ret=fsub(src,pos)
	if not failed_rule(ret) then return 0 end if
	if length(pos)<=1 then return 0 end if
	pos=pos[1..$-1]
	pos[$]+=1
	return 1
end function

global function get_any()
	object ret
	ret=fsub(src,pos) pos[$]+=1
	if failed_rule(ret) then return 0 end if
	return ret
end function

atom last_tick
last_tick=time()
global function get_char()--rule_ranges uses it
-- 	object ret
-- 	ret=get_any()
-- 	if failed_rule(ret) then return -1 end if
-- 	return ret[2]
	atom ret
	sequence spos
	object aret
	ret=-1
--	if pos<=length(src) then ret=src[pos] end if
	if equal(pos,{1}) and flat_pos then
		pos&=1
	end if
	spos=pos
	aret=get_any()
	if not failed_rule(aret) then ret=aret[2] end if
-- 	if last_tick<time() then
-- 		last_tick+=.01
-- 		spos=get_position()
-- 		if length(src) then
-- 		print(1,pos)--printf(1,"%6.2f%%\n",{pos[$]/length(src[1])*100})
-- 		position(min(spos[1],25),spos[2])
-- 		end if
-- 	end if
	return ret
end function

global procedure unget_char()--not used externally
	pos[$]-=1
end procedure

global function peek_char()--used alot
-- 	atom char
	if equal(pos,{1}) and flat_pos then
		pos&=1
	end if
	if pos[$]>length(src[$]) then return -1 end if
	return src[$][pos[$]]
-- 	char=get_char()
-- 	unget_char()
-- 	return char
end function

global function get_schar(atom schar)--get_schars uses it
-- ? 160
	if get_char()=schar then return 1 end if
	unget_char()
	return 0
end function

global function get_schars(object schars)--used by rule char
object a,b
atom len,npos,npose
-- ? 169
	if atom(schars) then return get_schar(schars) end if
-- ? 170
	a=src
	b=pos
	for i = 1 to length(b)-1 do
		if b[i]>length(a) then return 0 end if
		if atom(a) then return 0 end if
		a=a[b[i]]
	end for
-- 	?179
	if atom(a) then return 0 end if
	npos=pos[$]
	len=length(schars)
-- 	?183
	npose=npos+len-1
	if npose>length(a) then return 0 end if
-- 	?186
	if not equal(schars,a[npos..npose]) then return 0 end if
-- 	?188
	pos[$]+=len
-- 	?189
	return 1
-- 	tval=fsub(src,pos[1..$-1])
-- 	if failed_rule(tval) then return 0 end if
-- 	if not sequence(tval[2]) then return 0 end if
-- 	if pos[$]<1 or pos[$]+length(schars)-1>length(tval[2]) then return 0 end if
-- 	if not equal(tval[2][pos[$]..pos[$]+length(schars)-1],schars) then return 0 end if
-- 	pos[$]+=length(schars)
-- 	return 1

-- 	for i = 1 to length(schars) do
-- 		if get_char()!=schars[i] then
-- 			for ii=1 to i do unget_char()
-- 			end for
-- 			return 0
-- 		end if
-- 	end for
	return 1
end function

function pos_to_linepos(sequence src, object pos)
atom line, col,lpos
	return {1,1}
	line=1
	col=0
	lpos=pos[$]
	for i = 1 to lpos do
		if i>length(src) then exit end if
		if equal(src[i],'\n') then
			line+=1
			col=1
		else
			col+=1
		end if
	end for
	return {line,col}
end function

global atom dump_dictionary_rid
procedure high_dump_dictionary()
	call_proc(dump_dictionary_rid,{})
end procedure

global function get_line_numbers(sequence ns)
atom lb, lp
sequence ret
	lb=1
	lp=0
	ret=repeat(0, length(ns))
	for i = 1 to length(ns) do
		if ns[i]=10 then lb+=1 lp=0
		else lp+=1
		end if
		ret[i]={i,lb,lp}
	end for
	return ret
end function

atom lt
lt=time()
global function getfpos(sequence p)
atom len_numbers, len_src, pd
-- atom lb,lp
-- 	lb=1
-- 	lp=0
-- -- 	if lt+.1<time() then
-- -- 		position(1,1) puts(1,parsing_file_name&32) ? {p[$],length(src[1])}
-- -- 		lt=time()
-- --  	end if
-- 	for i = 1 to p[$] do
-- -- 		exit
-- 		if i>length(src[1]) then exit end if
-- 		if src[1][i]=10 then lb+=1 lp=0
-- 		else lp+=1
-- 		end if
-- 	end for
-- 	return {p[$],lb,lp}
	len_numbers=length(parsing_file_line_numbers)
	len_src=length(src[1])
	pd=p[$]
	if pd>len_src then return {pd,0,0} end if
	return parsing_file_line_numbers[pd]
end function

function flatten(object a)
sequence ret
	if atom(a) then return a end if
	ret={}
	for i = 1 to length(a) do
		ret&=flatten(a[i])
	end for
	return ret
end function

global procedure error(sequence text)
atom char
sequence lp
-- 	puts(1,10)
	char=peek_char()
-- 	lp=pos_to_linepos(src,pos)
	puts(1,parsing_file_name&10)
	trace_msg_rid(sprintf("error '%s' %s '%s' %d #%x\n",{flatten(text),sprint({getfpos(bpos),getfpos(pos)})}&char&char&char))
-- 	puts(1,"Press enter to quit\n")
-- 	high_dump_dictionary()
-- 	while wait_key()!=13 do end while
	--system(sprintf("ceditor.bat %d \"%s\"",{lp[1],cfn}),0)
	while task_self()>0 do
-- 		if task_self()<1 then ? 1/0 end if
		task_suspend(task_self()) task_yield()
	end while
-- 	abort(1/0)
end procedure




include "emeta_exec.e"
include "emeta_parse.e"
include "opmeta.e"
