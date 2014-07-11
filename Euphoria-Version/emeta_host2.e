global atom h2_func_rid
global sequence eprocs,efuncs

include "builtin.e"
include wildcard.e
include machine.e
include get.e
include file.e

atom gtrace_status
gtrace_status=0
global sequence hdict--key, val
hdict={}

sequence argst_wtask
eprocs={}
efuncs={}
argst_wtask={}

procedure fixup_argst_wtask()
	while task_self()+1>length(argst_wtask) do
		argst_wtask=append(argst_wtask,{{}})
	end while
end procedure

atom trace_fn
trace_fn=open("trace.txt","w")

without trace

function dlook(atom n, object key, atom default)
	if n>length(hdict) then return default end if
	for i = 1 to length(hdict[n]) do
		if equal(hdict[n][i][1],key) then return hdict[n][i][2] end if
	end for
	return default
end function

procedure dset(atom n, object key, object val)
	if n>length(hdict) then hdict&=repeat({},n-length(hdict)) end if
	for i = 1 to length(hdict[n]) do
		if equal(hdict[n][i][1],key) then hdict[n][i][2]=val return end if
	end for
	hdict[n]=append(hdict[n],{key,val})
end procedure

function hdlook(object key, atom default)
	return dlook(1,key,default)
end function

procedure hdset(object key, object val)
	dset(1,key,val)
end procedure

with trace


sequence id_host2--host code runners
id_host2={}

global function exp_host2(sequence code, sequence vval, sequence vname)--host code expression
object ret
atom found
	found=0
	if equal(code[1],"trace")      then
		if not gtrace_status then
			if gtrace_status then
-- 				trace_msg_pos_rid("trace"&sprint(time()),parsing_file_name, code[2])
				trace_msg_pos_rid("trace"&sprint(time()),code[2][3], code[2])
				task_suspend(task_self())
			else
				trace_pos_rid(code[2][3], code[2])
			end if
		end if
		--print(trace_fn,code[2]) puts(trace_fn,10) flush(trace_fn) ? code[2]
		code=code[3]
	end if
	for i = 1 to length(id_host2) do
		if equal(id_host2[i][1],code[1]) then
			found=i
			exit
		end if
	end for
	if not found then
		pretty_print(1,code,{2})
		? 1/0
		error("unknown code")
	else
		ret=call_func(id_host2[found][2],{code,vval,vname})
		return ret
	end if
end function

global function exp_host2s(sequence codes, sequence vval, sequence vname)--host code expression
	for i = 1 to length(codes) do
		codes[i]=exp_host2(codes[i], vval, vname)
	end for
	return codes
end function

procedure host2_handler(sequence tag, atom id)
	id_host2=append(id_host2,{tag,id})
end procedure

function seal(sequence a)
	return append(a,{"uploop"})
end function

atom run_host2_rid
global procedure phost2(sequence code)
object ret
	ret=call_func(run_host2_rid,{append(code,{"return",{"lit",0}}),{},{}})
end procedure

global function fhost2(sequence code)
	return call_func(run_host2_rid,{code,{},{}})
end function

function subs_set(sequence a, sequence s, object v, object op)
	if length(s)=1 then
		if length(s[1])=1 then
			   if equal(op, '=') then   a[s[1][1]] =v
			elsif equal(op,"-=") then   a[s[1][1]]-=v
			elsif equal(op,"+=") then   a[s[1][1]]+=v
			elsif equal(op,"*=") then   a[s[1][1]]*=v
			elsif equal(op,"/=") then   a[s[1][1]]/=v
			elsif equal(op,"&=") then   a[s[1][1]]&=v
			else ? 1/0
			end if
			return a
		end if
		if length(s[1])=2 then
			   if equal(op, '=') then   a[s[1][1]..s[1][2]] =v
			elsif equal(op,"-=") then   a[s[1][1]..s[1][2]]-=v
			elsif equal(op,"+=") then   a[s[1][1]..s[1][2]]+=v
			elsif equal(op,"*=") then   a[s[1][1]..s[1][2]]*=v
			elsif equal(op,"/=") then   a[s[1][1]..s[1][2]]/=v
			elsif equal(op,"&=") then   a[s[1][1]..s[1][2]]&=v
			else ? 1/0
			end if
			return a
		end if
	else
		if length(s[1])=1 then a[s[1][1]]=subs_set(a[s[1][1]],s[2..$],v,op) return a end if
		if length(s[1])=2 then a[s[1][1]..s[1][2]]=subs_set(a[s[1][1]..s[1][2]],s[2..$],v,op) return a end if
	end if
end function

function subs_list(sequence a, sequence sb)
	object sub
	sequence ret
	ret={}
	for i = 1 to length(sb) do
		hdset({"var","$"},length(a))
		sub=exp_host2s(sb[i],{},{})
		ret=append(ret,sub)
-- 		if    length(sub)=1 then a=a[sub[1]]
-- 		elsif length(sub)=2 then a=a[sub[1]..sub[2]]
-- 		else ? 1/0
-- 		end if
	end for
	return ret
end function

function rvar(sequence name)
	fixup_argst_wtask()
	if length(argst_wtask[task_self()+1]) then
	for i = 1 to length(argst_wtask[task_self()+1][$]) do
		if equal(argst_wtask[task_self()+1][$][i][1],name) then
-- 			printf(1,"aget %s=%s\n",{name,sprint(argst_wtask[task_self()+1][$][i][2])})
			return argst_wtask[task_self()+1][$][i][2]
		end if
	end for
	end if
	return hdlook({"var",name},0)
end function

--fixup_argst_wtask() argst_wtask[task_self()+1]
procedure vset(sequence name, object v)
	fixup_argst_wtask()
	if length(argst_wtask[task_self()+1]) then
	for i = 1 to length(argst_wtask[task_self()+1][$]) do
		if equal(argst_wtask[task_self()+1][$][i][1],name) then
-- 			printf(1,"aset %s=%s\n",{name,sprint(v)})
			argst_wtask[task_self()+1][$][i][2]=v
			return
		end if
	end for
	end if
	hdset({"var",name},v)
end procedure

with trace
global function run_host2(sequence code, sequence vval, sequence vname)--host code statement
sequence tag,ins
sequence args,ret,fname,cstack,forvars,lst
atom run_else, ip,nip
object pf,sval,nval
	cstack={}
	ip=1
	forvars={}
	while 1 do
	while 1 do
		if ip>length(code) then ? 1/0 end if--no return statement found
		nip=ip+1
		ins=code[ip]
		tag=ins[1]
		if equal(tag,"trace")      then
			if gtrace_status then
				trace_msg_pos_rid("trace "&sprint(time()),ins[2][3], ins[2])
				task_suspend(task_self())
			else
				trace_pos_rid(ins[2][3], ins[2])
			end if
			--print(trace_fn,ins[2]) puts(trace_fn,10) flush(trace_fn) ? ins[2]
			ins=ins[3] tag=ins[1]
		end if
		if equal(tag,"global")     then ins=ins[2] tag=ins[1] end if
		
		if    equal(tag,"return")  then  return {1,exp_host2(ins[2],vval,vname)}
		elsif equal(tag,"par")     then  return {1,exp_host2(ins[2],vval,vname)}
		elsif equal(tag,"with")    then
		elsif equal(tag,"without") then
		elsif equal(tag,"vardef")  then
			fixup_argst_wtask()
			if length(argst_wtask[task_self()+1])>1 then
				for i = 1 to length(ins[3]) do
					argst_wtask[task_self()+1][$]=append(argst_wtask[task_self()+1][$],{ins[3][i],0})
				end for
			end if
		elsif equal(tag,"global")  then
		elsif equal(tag,"include") then--should handle this in parser
			cstack=append(cstack,{nip,code,forvars}) nip=1 code=seal(ins[4]) forvars={}
		elsif equal(tag,"if")      then
			if exp_host2(ins[2],vval,vname) then
				cstack=append(cstack,{nip,code,forvars}) nip=1 code=seal(ins[3]) forvars={}
			else
				run_else=1
				for n = 1 to length(ins[4]) do
					if exp_host2(ins[4][n][2],vval,vname) then
						cstack=append(cstack,{nip,code,forvars}) nip=1 code=seal(ins[4][n][3]) forvars={}
						run_else=0
						exit
					end if
				end for
				if run_else then
					cstack=append(cstack,{nip,code,forvars}) nip=1 code=seal(ins[5]) forvars={}
				end if
			end if
		elsif equal(tag,"while")  then
-- 			trace(1)
			if exp_host2(ins[2],vval,vname) then
				forvars={1}
				cstack=append(cstack,{ip,code,forvars}) nip=1 code=seal(ins[3]) forvars={}
			else
				forvars={}
			end if
		elsif equal(tag,"for")     then
			if length(forvars) then
				forvars[1]+=forvars[3]
			else
				forvars={exp_host2(ins[3],vval,vname),exp_host2(ins[4],vval,vname),exp_host2(ins[5],vval,vname)}
				fixup_argst_wtask()
				argst_wtask[task_self()+1][$]=append(argst_wtask[task_self()+1][$],{ins[2],forvars[1]})
			end if
			if (forvars[3]>0 and forvars[1]<=forvars[2]) or (forvars[3]<0 and forvars[1]>=forvars[2]) then
				vset(ins[2],forvars[1])
				cstack=append(cstack,{ip,code,forvars}) nip=1 code=seal(ins[6]) forvars={}
			else
				forvars={}
			end if
-- 			for n = exp_host2(ins[3],vval,vname) to exp_host2(ins[4],vval,vname) by exp_host2(ins[5],vval,vname) do
-- 				hdset({"var",ins[2]},n)
-- 				--args=run_host2(append(ins[6],{"return",{"lit",0}}),vval,vname)
-- 				cstack=append(cstack,{ip,code}) nip=1 code=seal(ins[6])
-- 			end for
		elsif equal(tag,"varset")    then
			nval=exp_host2(ins[5],vval,vname)
			sval=rvar(ins[2])
			if length(ins[3])!=0 then
-- 				trace(1)
				lst=subs_list(sval,ins[3])
				sval=subs_set(sval,lst,nval,ins[4])
-- 				puts(1,"set_subs ") ? sval
			else
				   if equal(ins[4], '=') then   sval =nval
				elsif equal(ins[4],"-=") then   sval-=nval
				elsif equal(ins[4],"+=") then   sval+=nval
				elsif equal(ins[4],"*=") then   sval*=nval
				elsif equal(ins[4],"/=") then   sval/=nval
				elsif equal(ins[4],"&=") then   sval&=nval
				else ? 1/0
				end if
			end if
			vset(ins[2],sval)
		elsif equal(tag,"procedure") then eprocs=append(eprocs,ins[3..4]) hdset({"procedure",ins[2]},length(eprocs))
		elsif equal(tag,"function")  then efuncs=append(efuncs,ins[3..4]) hdset({"function",ins[2]},length(efuncs))
		elsif equal(tag,"type")      then efuncs=append(efuncs,ins[3..4]) hdset({"function",ins[2]},length(efuncs))
		elsif equal(tag,"constant")  then
			for n = 1 to length(ins[2]) do
				hdset({"var",ins[2][n][1]},exp_host2(ins[2][n][2],vval,vname))
			end for
		elsif equal(tag,"proc")    then
		
		
			args=exp_host2s(ins[3],vval,vname)
			pf=hdlook({"procedure",ins[2]},0)
			if pf then
				fixup_argst_wtask()
				argst_wtask[task_self()+1]=append(argst_wtask[task_self()+1],{})
				for n = 1 to length(eprocs[pf][1]) do
					--hdset({"var",efuncs[pf][1][n][2]},args[n])
					argst_wtask[task_self()+1][$]=append(argst_wtask[task_self()+1][$],{eprocs[pf][1][n][2],args[n]})
				end for
				phost2(eprocs[pf][2])
				argst_wtask[task_self()+1]=argst_wtask[task_self()+1][1..$-1]
-- 				for n = 1 to length(eprocs[pf][1]) do
-- 					hdset({"var",eprocs[pf][1][n][2]},args[n])
-- 				end for
-- 				phost2(eprocs[pf][2])
			elsif equal(ins[2],"call_proc") then
				pf=args[1]
				args=args[2]
			fixup_argst_wtask()
				argst_wtask[task_self()+1]=append(argst_wtask[task_self()+1],{})
				for n = 1 to length(eprocs[pf][1]) do
					argst_wtask[task_self()+1][$]=append(argst_wtask[task_self()+1][$],{eprocs[pf][1][n][2],args[n]})
				end for
				phost2(eprocs[pf][2])
				argst_wtask[task_self()+1]=argst_wtask[task_self()+1][1..$-1]
-- 				for n = 1 to length(efuncs[pf][1]) do
-- 					hdset({"var",efuncs[pf][1][ip][2]},args[n])
-- 				end for
-- 				phost2(efuncs[pf][2])
			elsif equal(ins[2],"trace") then
				gtrace_status=args[1]
-- 			elsif equal(ins[2],"task_schedule") then --  **** task_schedule(args[1],args[2])
-- 			elsif equal(ins[2],"task_yield") then --  **** task_yield()
-- 			elsif equal(ins[2],"task_suspend") then --task_suspend(args[1])
-- 			elsif equal(ins[2],"task_clock_stop") then --task_clock_stop()
-- 			elsif equal(ins[2],"task_clock_start") then --task_clock_start()
			elsif find(ins[2],builtin_procs) then
				builtin_proc(ins[2],args)
			else ? 1/0
			end if
		elsif equal(tag,"returnp") then
			return {1,"returnp"}
		elsif equal(tag,"returnf") then
			return exp_host2(ins[2],vval,vname)
		elsif equal(tag,"uploop") then
			exit
		elsif equal(tag,"exit") then
-- 			trace(1)
			for n = 1 to length(cstack) do
				nip=cstack[$][1]
				code=cstack[$][2]
				forvars=cstack[$][3]
				cstack=cstack[1..$-1]
				if length(forvars) then nip+=1 forvars={} exit end if
			end for
		else
			pretty_print(1,ins,{2})
			? 1/0
			error("unknown code")
		end if
		ip=nip
	end while
		ip=cstack[$][1]
		code=cstack[$][2]
		forvars=cstack[$][3]
		cstack=cstack[1..$-1]
	end while
	pretty_print(1,code,{2})
	error("no return")
end function
without trace
run_host2_rid=routine_id("run_host2")

function h2_par(sequence code, sequence vval, sequence vname)
object ret
	ret=code[2]
  ret=exp_host2(ret,vval,vname)
	return ret
end function

function h2_seq(sequence code, sequence vval, sequence vname)
object ret
	ret=code[2]
	for i = 1 to length(ret) do
		ret[i]=exp_host2(ret[i],vval,vname)
	end for
	return ret
end function

sequence rule_rids,rule_names
rule_rids={}
rule_names={}
global procedure add_rule_rid(atom rid, sequence name)
	rule_rids&=rid
	rule_names=append(rule_names,name)
end procedure

function h2_id(sequence code, sequence vval, sequence vname)
object ret
atom found
	found=find(code[2],vname)
	if not found then
		found=find(code[2],rule_names)
		if found then return {"rule",rule_rids[found]} end if
	end if
	if not found then
		found=find_rule(code[2])
		if found then return {"rule",code[2]} end if
		error("id not found "&code[2])
	end if
	ret=vval[found]
	return ret
end function

function h2_seqlen(sequence code, sequence vval, sequence vname)
object ret
atom found
	return hdlook({"var","$"},0)
end function


function h2_var(sequence code, sequence vval, sequence vname)
object ret
atom found
	return rvar(code[2])
-- 	found=find(code[2],vname)
-- 	if not found then error("id not found "&code[2]) end if
-- 	ret=vval[found]
-- 	return ret
end function

function h2_subs(sequence code, sequence vval, sequence vname)
	object a,sub
	a=exp_host2(code[2][1],vval,vname)
	for i = 1 to length(code[2][2]) do
		hdset({"var","$"},length(a))
		sub=exp_host2s(code[2][2][i],vval,vname)
		if    length(sub)=1 then a=a[sub[1]]
		elsif length(sub)=2 then a=a[sub[1]..sub[2]]
		else ? 1/0
		end if
	end for
	return a
end function
host2_handler("subs",routine_id("h2_subs"))

function h2_vlit(sequence code, sequence vval, sequence vname)
object ret
	ret=value(code[2])
	ret=ret[2]
	return ret
end function

function h2_lit(sequence code, sequence vval, sequence vname)
object ret
	ret=code[2]
	return ret
end function

function h2_concat(sequence code, sequence vval, sequence vname)
object ret
	ret=exp_host2(code[2][1],vval,vname)&exp_host2(code[2][2],vval,vname)
	return ret
end function

function h2_add(sequence code, sequence vval, sequence vname)
object ret
	ret=exp_host2(code[2][1],vval,vname)+exp_host2(code[2][2],vval,vname)
	return ret
end function

function h2_sub(sequence code, sequence vval, sequence vname)
object ret
	ret=exp_host2(code[2][1],vval,vname)-exp_host2(code[2][2],vval,vname)
	return ret
end function

function h2_mul(sequence code, sequence vval, sequence vname)
object ret
	ret=exp_host2(code[2][1],vval,vname)*exp_host2(code[2][2],vval,vname)
	return ret
end function

function h2_div(sequence code, sequence vval, sequence vname)
object ret
	ret=exp_host2(code[2][1],vval,vname)/exp_host2(code[2][2],vval,vname)
	return ret
end function

function h2_lt (sequence code, sequence vval, sequence vname) object ret ret=    exp_host2(code[2][1],vval,vname) <   exp_host2(code[2][2],vval,vname) return ret end function
host2_handler("lt",routine_id("h2_lt"))

function h2_gt (sequence code, sequence vval, sequence vname) object ret ret=    exp_host2(code[2][1],vval,vname) >   exp_host2(code[2][2],vval,vname) return ret end function
host2_handler("gt",routine_id("h2_gt"))

function h2_le (sequence code, sequence vval, sequence vname) object ret ret=    exp_host2(code[2][1],vval,vname) <=  exp_host2(code[2][2],vval,vname) return ret end function
host2_handler("le",routine_id("h2_le"))

function h2_ge (sequence code, sequence vval, sequence vname) object ret ret=    exp_host2(code[2][1],vval,vname) >=  exp_host2(code[2][2],vval,vname) return ret end function
host2_handler("ge",routine_id("h2_ge"))

function h2_eq (sequence code, sequence vval, sequence vname) object ret ret=    exp_host2(code[2][1],vval,vname) =   exp_host2(code[2][2],vval,vname) return ret end function
host2_handler("eq",routine_id("h2_eq"))

function h2_ne (sequence code, sequence vval, sequence vname) object ret ret=    exp_host2(code[2][1],vval,vname) !=  exp_host2(code[2][2],vval,vname) return ret end function
host2_handler("ne",routine_id("h2_ne"))

function h2_andi(sequence code, sequence vval, sequence vname) if exp_host2(code[2][1],vval,vname) and exp_host2(code[2][2],vval,vname) then return 1 end if return 0 end function
function h2_ori (sequence code, sequence vval, sequence vname) if exp_host2(code[2][1],vval,vname) or  exp_host2(code[2][2],vval,vname) then return 1 end if return 0 end function
function h2_xori (sequence code, sequence vval, sequence vname) if exp_host2(code[2][1],vval,vname) xor  exp_host2(code[2][2],vval,vname) then return 1 end if return 0 end function
function h2_and(sequence code, sequence vval, sequence vname) object ret ret=    exp_host2(code[2][1],vval,vname) and exp_host2(code[2][2],vval,vname) return ret end function
function h2_or (sequence code, sequence vval, sequence vname) object ret ret=    exp_host2(code[2][1],vval,vname)  or exp_host2(code[2][2],vval,vname) return ret end function
function h2_xor(sequence code, sequence vval, sequence vname) object ret ret=    exp_host2(code[2][1],vval,vname) xor exp_host2(code[2][2],vval,vname) return ret end function
function h2_not(sequence code, sequence vval, sequence vname) object ret ret=not exp_host2(code[2],vval,vname)                                         return ret end function
host2_handler("not",routine_id("h2_not"))

function h2_pos(sequence code, sequence vval, sequence vname) object ret ret=+   exp_host2(code[2],vval,vname)                                         return ret end function
host2_handler("pos",routine_id("h2_pos"))

function h2_neg(sequence code, sequence vval, sequence vname) object ret ret=-   exp_host2(code[2],vval,vname)                                         return ret end function
host2_handler("neg",routine_id("h2_neg"))


function split(sequence a, atom b)
	sequence ret,t
	ret={}
	t={}
	for i = 1 to length(a) do
		if a[i]=b then ret=append(ret,t) t={}
		else
			t=append(t,a[i])
			if i = length(a) then ret=append(ret,t) end if
		end if
	end for
	return ret
end function

function eupath(sequence p)
sequence a,t,np
atom rf
	if sequence(dir(p)) then return p end if
	rf=find('\\',reverse(parsing_file_name))
	if not rf then
		if sequence(dir(p)) then return p end if
	else
		np=parsing_file_name[1..1+$-rf]&p
		if sequence(dir(np)) then return np end if
	end if
	--a=command_line() a=a[2]
	a=split(getenv("EUINC"),';')
	for i = 1 to length(a) do
		t=a[i]&"\\"&p
		if sequence(dir(t)) then return t end if
	end for
	a=split(getenv("EUDIR"),';')
	for i = 1 to length(a) do
		t=a[i]&"\\INCLUDE\\"&p
		if sequence(dir(t)) then return t end if
	end for
	puts(1,"Can't find include file "&p&" from file "&parsing_file_name&"\n")
	return 1/0
end function
without trace


-- sequence mf1,mf2
-- mf1={} mf2={}

-- function icf(atom id, sequence args)
-- sequence args
-- object ret,pf
-- 		pf=id
-- 		args=args
-- 		argst=append(argst,{})
-- 		for n = 1 to length(efuncs[pf][1]) do
-- 			argst[$]=append(argst[$],{efuncs[pf][1][n][2],args[n]})
-- 		end for
-- 		ret=fhost2(efuncs[pf][2])
-- 		argst=argst[1..$-1]
-- 		return ret
-- end function

function read_base(atom base, sequence parts)
	sequence ct
	atom ret,f,scale,pw,scale_change
	scale=1
	pw=0
	scale_change=0
	ct="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	ct=ct[1..base]
	ret=0
	parts=upper(parts)
	if parts[1]='+' then    parts=parts[2..$]
	elsif parts[1]='-' then parts=parts[2..$] scale*=-1
	end if
	for i = 1 to length(parts) do
		if parts[i]='.' then
			scale_change=-1
		elsif parts[i]='_' then
		else
			f=find(parts[i],ct)
			if not f then exit end if
			ret*=base
			ret+=f-1
			pw+=scale_change
		end if
	end for
	return ret*scale*power(base,pw)
end function


procedure tp(sequence args)
atom pf
-- 	? 1/0
	pf=args[1]
	args=args[2]
	fixup_argst_wtask()
	argst_wtask[task_self()+1]=append(argst_wtask[task_self()+1],{})
	for n = 1 to length(eprocs[pf][1]) do
		argst_wtask[task_self()+1][$]=append(argst_wtask[task_self()+1][$],{eprocs[pf][1][n][2],args[n]})
	end for
	phost2(eprocs[pf][2])
	argst_wtask[task_self()+1]=argst_wtask[task_self()+1][1..$-1]
end procedure

function h2_func(sequence code, sequence vval, sequence vname)
sequence args
object ret,pf,fr,found
	if equal(code[1],"func") then
		args=exp_host2s(code[3],vval,vname)
	elsif equal(code[1],"funclit") then
		args=code[3]
	end if
	pf=hdlook({"function",code[2]},0)
	if pf then
		fixup_argst_wtask()
		argst_wtask[task_self()+1]=append(argst_wtask[task_self()+1],{})
		for n = 1 to length(efuncs[pf][1]) do
			--hdset({"var",efuncs[pf][1][n][2]},args[n])
			argst_wtask[task_self()+1][$]=append(argst_wtask[task_self()+1][$],{efuncs[pf][1][n][2],args[n]})
		end for
		ret=fhost2(efuncs[pf][2])
		argst_wtask[task_self()+1]=argst_wtask[task_self()+1][1..$-1]
		return ret
	end if
	if equal(code[2],"routine_id") then
		pf=hdlook({"function",args[1]},0) if pf then return pf end if
		pf=hdlook({"procedure",args[1]},0) if pf then return pf end if
	elsif equal(code[2],"call_func") then-- ? 1/0 
		pf=args[1]
		args=args[2]
-- 		for n = 1 to length(efuncs[pf][1]) do
-- 			hdset({"var",efuncs[pf][1][n][2]},args[n])
-- 		end for
-- 		return fhost2(efuncs[pf][2])
		fixup_argst_wtask()
		argst_wtask[task_self()+1]=append(argst_wtask[task_self()+1],{})
		for n = 1 to length(efuncs[pf][1]) do
			--hdset({"var",efuncs[pf][1][n][2]},args[n])
			argst_wtask[task_self()+1][$]=append(argst_wtask[task_self()+1][$],{efuncs[pf][1][n][2],args[n]})
		end for
		ret=fhost2(efuncs[pf][2])
		argst_wtask[task_self()+1]=argst_wtask[task_self()+1][1..$-1]
		return ret
	elsif equal(code[2],"task_create") then
-- 		return 0
		return task_create(routine_id("tp"),{args})
-- 	elsif equal(code[2],"task_self") then return 1 --task_self()
-- 	elsif equal(code[2],"task_list") then return {1} --task_list()
-- 	elsif equal(code[2],"task_status") then return 1 --task_status(args[1])
	end if
-- 	if equal(code[2],"machine_func") and equal(args[1],M_CALL_BACK) then
-- 		pf=args[1]
-- 		args=args[2]
-- 		? length(efuncs[pf][1])
-- 		ret=call_back(routine_id("h2_func"))
-- 		? ret
-- 		return ret
-- 	end if
	if find(code[2],builtin_funcs) then
		return builtin_func(code[2],args)
	end if
	if equal(code[2],"error") then error(args[1]) return "" end if
	if equal(code[2],"host2") then hdict={} return run_host2(args[1],{},{}) end if
	if equal(code[2],"apply_rule") then
		if not equal("rule",args[1][1]) then ? 1/0 end if
		if length(args)=1 then args=append(args,{}) end if
		if atom(args[1][2]) then
			fr=args[1][2]
		else
			fr=find_rule(args[1][2])
		end if
		ret=apply_rule(fr,pos,args[2])
		if failed_rule(ret) then return 0 end if
		return ret[2]
	end if
	if equal(code[2],"lower") then return lower(args[1]) end if
	if equal(code[2],"statement_text") then return src[1][bpos[2]..pos[2]-1] end if
	if equal(code[2],"statement_start") then return bpos[2] end if
	if equal(code[2],"statement_end") then return pos[2]-1 end if
	if equal(code[2],"read_stream") then return src[1][args[1]..args[2]] end if
	if equal(code[2],"read_base") then return read_base(args[1],args[2]) end if
	if equal(code[2],"get_schars") then return get_schars(args[1]) end if
	if equal(code[2],"getfpos") then return {getfpos(bpos),getfpos(pos),parsing_file_name} end if
	if equal(code[2],"named_exec") then return rnamed_exec(args[1],args[2],args[3]) end if
	if equal(code[2],"eupath")     then return eupath(args[1]) end if
	if equal(code[2],"include")    then
		args[1]=eupath(args[1])
		if find(args,euincs) then return {} end if
		euincs=append(euincs,args)
-- 		ret=rnamed_exec({"emdebug"  ,"grammar"},eupath(args[1]),0)

		found=find("grammar",rule_names)
		if found then
			ret=rnamed_exec(rule_rids[found],args[1],0)
		else
-- 			ret=rnamed_exec({"emtest"  ,"grammar"},args[1],0)
			ret=rnamed_exec({"emdebug"  ,"grammar"},args[1],0)
		end if

		return ret[2]
	end if
	if equal(code[2],"float64_to_atom") then
		return float64_to_atom(args[1])
	end if
	error("unknown function "&code[2])
-- 	if equal(code[2],"equal") then return equal(args[1],args[2]) end if
-- 	if equal(code[2],"length") then return length(args[1]) end if
-- 	if equal(code[2],"puts") then return puts(args[1],args[2]) end if
-- 	if equal(code[2],"integer") then return integer(args[1]) end if
-- 	if equal(code[2],"sequence") then return sequence(args[1]) end if
-- 	if equal(code[2],"position") then return position(args[1]) end if
-- 	if equal(code[2],"object") then return object(args[1]) end if
-- 	if equal(code[2],"append") then return append(args[1]) end if
-- 	if equal(code[2],"prepend") then return prepend(args[1]) end if
-- 	if equal(code[2],"print") then return print(args[1]) end if
-- 	if equal(code[2],"printf") then return printf(args[1]) end if
-- 	if equal(code[2],"clear_screen") then return clear_screen(args[1]) end if
-- 	if equal(code[2],"floor") then return floor(args[1]) end if
-- 	if equal(code[2],"getc") then return getc(args[1]) end if
-- 	if equal(code[2],"gets") then return gets(args[1]) end if
-- 	if equal(code[2],"get_key") then return get_key(args[1]) end if
-- 	if equal(code[2],"rand") then return rand(args[1]) end if
-- 	if equal(code[2],"repeat") then return length(args[1]) end if
-- 	if equal(code[2],"length") then return length(args[1]) end if
-- 	if equal(code[2],"length") then return length(args[1]) end if
-- 	if equal(code[2],"length") then return length(args[1]) end if
-- 	if equal(code[2],"length") then return length(args[1]) end if
-- 	if equal(code[2],"length") then return length(args[1]) end if
-- 	if equal(code[2],"length") then return length(args[1]) end if
-- 	if equal(code[2],"length") then return length(args[1]) end if
-- 	if equal(code[2],"length") then return length(args[1]) end if
-- 	if equal(code[2],"length") then return length(args[1]) end if
-- 	if equal(code[2],"length") then return length(args[1]) end if
-- 	
--     "length", "puts", "integer", "sequence", "position", "object",
--     "append", "prepend", "print", "printf", 
--     "clear_screen", "floor", "getc", "gets", "get_key",
--     "rand", "repeat", "atom", "compare", "find", "match",
--     "time", "command_line", "open", "close", "trace", "getenv",
--     "sqrt", "sin", "cos", "tan", "log", "system", "date", "remainder",
--     "power", "machine_func", "machine_proc", "abort", "peek", "poke", 
--     "call", "sprintf", "arctan", "and_bits", "or_bits", "xor_bits",
--     "not_bits", "pixel", "get_pixel", "mem_copy", "mem_set",
--     "c_proc", "c_func", "routine_id", "call_proc", "call_func", 
--     "poke4", "peek4s", "peek4u", "profile", "equal", "system_exec",
--     "platform", "task_create", "task_schedule", "task_yield",
--     "task_self", "task_suspend", "task_list",
--     "task_status", "task_clock_stop", "task_clock_start","find_from",
--     "match_from"	
end function
host2_handler("func",routine_id("h2_func"))
host2_handler("funclit",routine_id("h2_func"))



host2_handler("seq",routine_id("h2_seq"))
host2_handler("id",routine_id("h2_id"))
host2_handler("var",routine_id("h2_var"))
host2_handler("seqlen",routine_id("h2_seqlen"))
host2_handler("lit",routine_id("h2_lit"))
host2_handler("vlit",routine_id("h2_vlit"))
host2_handler("concat",routine_id("h2_concat"))
host2_handler("add",routine_id("h2_add"))
host2_handler("sub",routine_id("h2_sub"))
host2_handler("mul",routine_id("h2_mul"))
host2_handler("div",routine_id("h2_div"))
host2_handler("and",routine_id("h2_and"))
host2_handler( "or",routine_id("h2_or"))
host2_handler("xor",routine_id("h2_xor"))
host2_handler("andi",routine_id("h2_andi"))
host2_handler( "ori",routine_id("h2_ori"))
host2_handler("xori",routine_id("h2_xori"))



host2_handler("par",routine_id("h2_par"))
without trace
h2_func_rid=routine_id("h2_func")