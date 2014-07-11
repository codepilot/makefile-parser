global function rule_dolist(object arg)
object ret
	if not enter_list() then return 0 end if
	ret=teorule(arg)
	if not exit_list() then ? 1/0 end if
	return ret
end function

global function rule_char(object arg)--ometa rule char
-- atom pc
-- 	pc=peek_char()
	if not get_schars(arg) then return failed end if
	if atom(arg) then return {1,arg} end if
	if length(arg)=1 then return {1,arg[1]} end if
	return {1,arg}
end function

global function rule_eqval(object arg)--ometa rule chars
object g
	g=get_any()
	if failed_rule(g) then return failed end if
	g=g[2]
	if not equal(g,arg) then return failed end if
-- atom pc
-- 	pc=peek_char()
-- 	if not get_schars(arg) then return failed end if
-- 	if atom(arg) then return {1,arg} end if
-- 	if length(arg)=1 then return {1,arg[1]} end if
 	return {1,arg}
end function

global function rule_producer(object arg)--ometa rule producer
-- atom pc
-- 	pc=peek_char()
	return apply_rule(find_rule(arg),pos,{})
end function

global function rule_nproducer(atom arg)--ometa rule producer
	return apply_rule(arg,pos,{})
end function

global function rule_superproducer(object arg, sequence args)--ometa rule producer
	return apply_rule(find_rule({parser_super(current_parser),arg}),pos,args)
end function

global function rule_foreignproducer(object arg, sequence args)--ometa rule producer
sequence cp
object ret
	cp=current_parser
	current_parser=arg[1]
	ret=apply_rule(find_rule(arg),pos,args)
	current_parser=cp
	return ret
end function

global function rule_producer2(object arg, sequence args)--ometa rule producer
	return apply_rule(find_rule(arg),pos,args)
end function

global function rule_nproducer2(atom arg, sequence args)--ometa rule producer
	return apply_rule(arg,pos,args)
end function

function in_ranges(atom c, sequence ranges)--tests is c is in ranges
	for i = 1 to length(ranges) do
		if c>=ranges[i][1] and c<=ranges[i][2] then
			return i
		end if
	end for
	return 0
end function

global function rule_ranges(object arg)--ometa rule char in ranges
object pc
	pc=peek_char()
	if not atom(pc) then return failed end if
	if not in_ranges(pc,arg) then return failed end if
	return {1,get_char()}
end function

