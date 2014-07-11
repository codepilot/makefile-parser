atom logfn
global atom eval_rule_rid--used by Eval_rule(RULE r)--needs comment

with trace
atom named_exec_rid
global function rnamed_exec(object nrule, sequence fn, atom single_apply)
	return call_func(named_exec_rid,{nrule,fn,single_apply})
end function

--include emeta_pr.e
include emeta_pr_single.e
--packrat implementation
include emeta_host2.e
--host code


atom eorule_id
global function teorule(sequence code)
	return call_func(eorule_id,{code})
end function

include emeta_rule.e

function find_any(sequence a, sequence b)
	for i = 1 to length(a) do
		if find(a[i],b) then return 1 end if
	end for
	return 0
end function
global atom rseq_last
rseq_last=0

global function set_rseq(object rseq, object val)--sets the return sequence value
	if rseq_last then
		return {1,val}
	end if
	if failed_rule(rseq) then
		rseq={1,val}
		return rseq
	end if
	rseq[2]&=val--need to change to strictly set only, not append or concat
	return rseq
end function


-- function eo_choice(object arg)
-- --can return exit[tf]
-- --can return {exit[tf],new rseq}
-- 	object tval
-- 	tval=teorule(arg)
-- end function

global constant eo_tag=1,eo_arg=2,eo_varname=3
global constant rs_status=1, rs_val=2
constant rss_good=1

global sequence Rrules
Rrules={}
function enum(sequence r)
	Rrules=append(Rrules,r)
	return length(Rrules)
end function
constant
Rnproducer=enum("nproducer"),
Rproducer=enum("producer"),
Rsproducer=enum("sproducer"),
Rproducer2=enum("producer2"),
Rsuperproducer=enum("superproducer"),
Rforeignproducer=enum("foreignproducer"),
Rnproducer2=enum("nproducer2"),
Rchar=enum("char"),
Reqval=enum("eqval"),
Rranges=enum("ranges"),
Rhost2=enum("host2"),
Rquery=enum("query"),
Rdofirst=enum("dofirst"),
Rdoopen=enum("doopen"),
Rdoclose=enum("doclose"),
Rchoice=enum("choice"),
Rstar=enum("*"),
Rplus=enum("+"),
Rbang=enum("!"),
Rdolist=enum("dolist"),
Rrep=enum("rep"),
Rchoices=enum("choices"),
Rchoicemap=enum("choicemap"),
Rdofirst2=enum("dofirst2")

atom depth
depth=0

global atom tried_choices, succeeded_choices, failed_choices
tried_choices=0 succeeded_choices=0 failed_choices=0

function eorule(sequence code, sequence arg_names, sequence args)--direct rules running
sequence tpos
sequence vname,vval
object rseq,tval,tlen--return sequence, temp return seq
sequence ttval--multiple tvals

sequence eop     --{eo_tag,eo_arg[,eo_varname]}
object   tag     --eo_tag
object   arg     --eo_arg
sequence varname --eo_varname
object pc
-- 	printf(1,"'%d'",depth)
-- 	if equal(parsing_file_name,"listtest.txt") then trace(1) end if
	depth+=1
	tpos=pos
	vname=arg_names--{}
	vval=args--exp_host2s(args,{},{})--{}
	rseq=failed
	if not length(code) then
		pos=tpos
		depth-=1
		return failed
	end if
	for ci=1 to length(code) do
		eop=code[ci]
-- 		if sequence(eop[1][1]) then--fix this so that it doesn't happen
-- 			tval=eorule(eop,arg_names,args)
-- 			if failed_rule(tval)  then
-- 				pos=tpos
-- 				rseq=failed
-- 				exit
-- 			end if
-- 			if length(tval)=4 then vname&=tval[3] vval&=tval[4] end if
-- 			rseq=set_rseq(rseq,tval[rs_val])
-- 			tval=0
-- 		else
			tag=eop[eo_tag]
			if sequence(tag) then tag=find(tag,Rrules) end if
--			if equal(parsing_file_name,"listtest.txt") then printf(1,"%s ",{Rrules[tag]}) end if
			arg=eop[eo_arg]
			varname={}
			if length(eop)>=eo_varname then varname=eop[eo_varname] end if
			if find(tag,{Rhost2,Rquery,Rdofirst,Rdofirst2}) then
				   if tag=Rhost2                      then rseq=failed tval=run_host2(arg,vval,vname)
				elsif tag=Rquery                      then rseq=failed tval=run_host2(arg,vval,vname) if equal(tval[rs_val],0) then tval=0 end if
				elsif tag=Rdofirst                    then rseq=failed tval=eorule(arg,arg_names,args)
				elsif tag=Rdofirst2                   then rseq=failed tval=eorule(arg[1],arg[2],exp_host2s(arg[3],vval,vname))
				else ? 1/0
				end if
				if failed_rule(tval) then
					pos=tpos
					rseq=failed
					exit
				end if
				if length(varname) then vname=append(vname,varname) vval=append(vval,tval[rs_val]) end if
				rseq={1,tval[rs_val]}
				tval=0
			elsif find(tag,{Rnproducer,Rchar,Reqval,Rnproducer2,Rproducer2,Rproducer,Rsproducer,Rsuperproducer,Rforeignproducer,Rranges}) then
				if    tag=Rchar                       then tval=rule_char(arg)
				elsif tag=Reqval                      then tval=rule_eqval(arg)
				elsif tag=Rproducer or tag=Rsproducer then tval=rule_producer(arg)
				elsif tag=Rnproducer                  then tval=rule_nproducer(arg)
				elsif tag=Rproducer2                  then tval=rule_producer2(arg[1],exp_host2s(arg[2],vval,vname))
				elsif tag=Rnproducer2                 then tval=rule_nproducer2(arg[1],exp_host2s(arg[2],vval,vname))
				elsif tag=Rsuperproducer              then tval=rule_superproducer(arg[1],exp_host2s(arg[2],vval,vname))
				elsif tag=Rforeignproducer            then tval=rule_foreignproducer(arg[1],exp_host2s(arg[2],vval,vname))
	      elsif tag=Rranges                     then tval=rule_ranges(arg)
-- 				elsif tag=Rhost2                      then rseq=failed tval=run_host2(arg,vval,vname)
-- 				elsif tag=Rquery                      then rseq=failed tval=run_host2(arg,vval,vname) if equal(tval[rs_val],0) then tval=0 end if
-- 				elsif tag=Rdofirst                    then rseq=failed tval=eorule(arg,arg_names,args)
				--elsif find(tag,{"dolist"})               then rseq=failed tval=rule_dolist(arg)
				else ? 1/0
				end if

				if failed_rule(tval) then
					pos=tpos
					rseq=failed
					exit
				end if
				if length(varname) then vname=append(vname,varname) vval=append(vval,tval[rs_val]) end if
				if rseq_last then
					rseq={1,tval[rs_val]}
				else
					rseq=set_rseq(rseq,tval[rs_val])
				end if
				tval=0
			else
				if tag=Rchoice then
					tval=eorule(arg,arg_names,args)
					if not failed_rule(tval) then
						rseq=set_rseq(rseq,tval[rs_val])
						tval=0
						exit
					end if
				elsif tag=Rchoices then
					for cc = 1 to length(arg) do
						tried_choices+=1
						tval=eorule(arg[cc],arg_names,args)
						if not failed_rule(tval) then
							rseq=set_rseq(rseq,tval[rs_val])
							tval=0
							succeeded_choices+=1
							exit
						else
							failed_choices+=1
						end if
					end for
				elsif tag=Rchoicemap then
					pc=peek_char()
					if pc+2>length(arg[1]) then
						pos=tpos
						rseq=failed
						exit
					end if
					for cc = 1 to length(arg[1][pc+2]) do
						tried_choices+=1
						tval=eorule(arg[2][arg[1][pc+2][cc]],arg_names,args)
						if not failed_rule(tval) then
							rseq=set_rseq(rseq,tval[rs_val])
							tval=0
							succeeded_choices+=1
							exit
						else
							failed_choices+=1
						end if
					end for
				elsif tag=Rstar then--zero or more
					if not rseq_last then
						rseq=set_rseq(rseq,{})
					end if
					ttval={}
					while 1 do
						tval=eorule(arg,arg_names,args)
						if failed_rule(tval) then exit end if
						ttval=append(ttval,tval[rs_val])
						if not rseq_last then
							rseq=set_rseq(rseq,tval[rs_val])
						end if
						tval=0
					end while
					if length(varname) then vname=append(vname,varname) vval=append(vval,ttval) end if
					if rseq_last then
						rseq={1,ttval}
					end if
					tval=0
					ttval={}
					
				elsif tag=Rplus then--one or more,replace with a one then zero or more
					if not rseq_last then
						rseq=set_rseq(rseq,{})
					end if
					ttval={}
					tval=eorule(arg,arg_names,args)
					if failed_rule(tval) then
						pos=tpos
						rseq=failed
						exit
					end if
					ttval=append(ttval,tval[rs_val])
					if not rseq_last then
						rseq=set_rseq(rseq,tval[rs_val])
					end if
					tval=0
					while 1 do
						tval=eorule(arg,arg_names,args)
						if failed_rule(tval) then exit end if
						ttval=append(ttval,tval[rs_val])
						if not rseq_last then
							rseq=set_rseq(rseq,tval[rs_val])
						end if
					end while
					if length(varname) then vname=append(vname,varname) vval=append(vval,ttval) end if
					if rseq_last then
						rseq={1,ttval}
					end if
					tval=0
					ttval={}
				elsif tag=Rbang then--lookahead not true
					tval=eorule(arg,arg_names,args)
					if not failed_rule(tval) then
						pos=tpos
						rseq=failed
						exit
					else
						rseq=set_rseq(rseq,{})
					end if
					tval=0
				elsif tag=Rdoopen then
					if not enter_list() then pos=tpos rseq=failed exit end if
				elsif tag=Rdoclose then
					if not exit_list() then pos=tpos rseq=failed exit end if
				elsif tag=Rdolist then
					if not enter_list() then pos=tpos rseq=failed exit end if
					tval=eorule(arg,arg_names,args)
					if failed_rule(tval) then pos=tpos rseq=failed exit end if
					if length(tval)=4 then vname&=tval[3] vval&=tval[4] end if
					if not exit_list() then pos=tpos rseq=failed end if
					if length(varname) then vname=append(vname,varname) vval=append(vval,tval[rs_val]) end if
					rseq=set_rseq(rseq,tval[rs_val])
					tval=0
				elsif tag=Rrep then
					ttval={}
					tlen=exp_host2(arg[1],vval,vname)
					tval=0
					for ri = 1 to tlen do
						tval=eorule(arg[2],arg_names,args)
						if failed_rule(tval) then
							pos=tpos
							rseq=failed
							exit
						end if
						ttval=append(ttval,tval[rs_val])
						rseq=set_rseq(rseq,tval[rs_val])
					end for
					if failed_rule(rseq) then exit end if
					if length(varname) then vname=append(vname,varname) vval=append(vval,ttval) end if
					tval=0
				else
					pretty_print(1,eop,{2})
					puts(1,10)
					error("unknown command")
					? 1/0
				end if
			end if
-- 		end if
	end for
	depth-=1
-- 	printf(1,"`%d`",depth)
	if atom(rseq) then return rseq end if
	return {rseq[1],rseq[2],vname,vval}
end function
eorule_id=routine_id("eorule")

function eval_rule_low(RULE r,sequence args)--external erule invocation
object trval
	if length(parsers[r])>2 then
		trval=eorule(parsers[r][2],parsers[r][3],args)--76.03
	else
		trval=eorule(parsers[r][2],{},{})--76.03
	end if
	return trval
end function
eval_rule_rid=routine_id("eval_rule_low")


global atom logfn3
logfn3=open("log3.txt","w")

atom last_tick
last_tick=time()
global function exec_val(sequence rule, object val, atom single_apply)
sequence ret
object rval
object last_pos,spos
	
-- 	printf(1,"exec_val(%s,?) ",{rule})
	logfn=open("log1.txt","w")
	flat_pos=0
	if sequence(val) then
		flat_pos=1
		for i = 1 to length(val) do
			if not atom(val[i]) then flat_pos=0 exit end if
		end for
	end if
	ret={}
--	cfn={}--fn
	--if atom(val) then src={val} else src=val end if
	pos={1}
	src={val}
	parsing_file_line_numbers=get_line_numbers(src[1])
	--pos={1}
	--memos={}
	--memo_positions={}
	--heads={}
	--head_positions={}
	--all_lrs={}
	--all_heads={}
	--all_memoentrys={}
	--LRStack=new_lr(-1,-1,0,0)
	last_pos=pos
	while not equal(peek_char(),-1) do
		task_yield()
		init_pr(0,pos,val,length(parsers))
		rval=apply_rule(find_rule(rule),pos,{})
		ret&=rval
		if failed_rule(rval) then error("unknown character") end if
		if equal(last_pos,pos) then
			--puts(1,"success without input, main rule has a * zero or more\n")
			exit
		end if
		if equal(last_pos,pos) then puts(1,"stuck ") ? pos end if --error("parse stuck") end if
		last_pos=pos
--		puts(1,rval)
		if single_apply then ret=rval exit end if
-- 		if last_tick<time() then
-- 			last_tick+=.01
-- 			spos=get_position()
-- 			if length(src) then
-- 			print(1,pos&length(src[1]))--printf(1,"%6.2f%%\n",{pos[$]/length(src[1])*100})
-- 			position(min(spos[1],25),spos[2])
-- 			end if
-- 		end if
	end while
	--puts(logfn3,ret)
	if length(ret) and logfn3!=-1 then
		--puts(1, " printing file names ")
		--pretty_print(logfn3,ret[2],{2})
	end if
	if logfn3!=-1 then
		close(logfn)
	end if
-- 	puts(1,10)
	return ret
end function

global function exec(sequence rule, sequence fn, atom single_apply)
	cfn=fn
	parsing_file_name=fn
	return exec_val(rule, read_whole_file(fn),single_apply)
-- sequence ret
-- object rval
-- atom last_pos
-- 	printf(1,"exec(%s,%s) ",{rule,fn})
-- 	logfn=open("log1.txt","w")
-- 	ret={}
-- 	cfn=fn
-- 	src=read_whole_file(cfn)
-- 	pos=1
-- 	memos={}
-- 	heads={}
-- 	all_lrs={}
-- 	all_heads={}
-- 	all_memoentrys={}
-- 	LRStack=new_lr(-1,-1,0,0)
-- 	last_pos=pos
-- 	while peek_char()!=-1 do
-- 		rval=apply_rule(find_rule(rule),pos)
-- 		if failed_rule(rval) then error("unknown character") end if
-- 		if last_pos=pos then error("parse stuck") end if
-- 		last_pos=pos
-- --		puts(1,rval)
-- 		ret&=rval
-- 	end while
-- 	--puts(logfn3,ret)
-- 	if length(ret) then
-- 		pretty_print(logfn3,ret[2],{2})
-- 	end if
-- 	close(logfn)
-- 	puts(1,10)
-- 	return ret
end function

global function named_exec_val(object nrule, object val, atom single_apply)
sequence ret
object rval
object last_pos,spos
	
	push_pr()
	parsers={}
	for i = 1 to length(named_parsers) do
		for ii = 1 to length(named_parsers[i][3]) do
			parsers=append(parsers,named_parsers[i][3][ii])
			parsers[$][1]={named_parsers[i][1],parsers[$][1]}
		end for
	end for
	parser_texts={}
	if sequence(nrule) then
		current_parser=nrule[1]
	end if

	logfn=open("log1.txt","w")
	flat_pos=0
	if sequence(val) then
		flat_pos=1
		for i = 1 to length(val) do
			if not atom(val[i]) then flat_pos=0 exit end if
		end for
	end if
	ret={}
	pos={1}
	src={val}
	parsing_file_line_numbers=get_line_numbers(src[1])
	last_pos=pos
	while not equal(peek_char(),-1) do
		task_yield()
		init_pr(direct_eval,pos,val,length(parsers))
		rval=apply_rule(find_rule(nrule),pos,{})
		ret&=rval
		if failed_rule(rval) then error("unknown character") end if
		if equal(last_pos,pos) then
		--puts(1,"success without input, main rule has a * zero or more\n")
			exit
		end if
		if equal(last_pos,pos) then puts(1,"stuck ") ? pos end if --error("parse stuck") end if
		last_pos=pos
		if failed_rule(rval) then 
		if single_apply then ret=append({-1,-1},pos) exit end if
		else
		if single_apply then ret=append(rval,pos) exit end if
		end if
-- 		if single_apply then ret=rval exit end if
	end while
	if length(ret) and logfn3!=-1 then
		pretty_print(logfn3,ret[2],{2})
	end if
	if logfn!=-1 then
		close(logfn)
	end if
	pop_pr()
	return ret
end function

global sequence fnstack
fnstack={}

global function named_exec(object nrule, sequence fn, atom single_apply)
sequence ret
	fnstack=append(fnstack,{cfn,parsing_file_name})
	cfn=fn
	parsing_file_name=fn
	ret=named_exec_val(nrule, read_whole_file(fn),single_apply)
	cfn=fnstack[$][1]
	parsing_file_name=fnstack[$][1]
	fnstack=fnstack[1..$-1]
	return ret
end function

named_exec_rid=routine_id("named_exec")
