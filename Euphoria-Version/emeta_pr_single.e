--packrat implementation
-- without type_check


constant nil=0
--constant fail=-1
sequence otypes
otypes={}

type tchecked(sequence a)
	if length(otypes)>length(a) then ? 1/0 end if
	return 1
	for i = 1 to length(a) do
		call_proc(otypes[i],{i})
	end for
	return 1
end type
tchecked objs--used for variable references
objs={}

function remove(object a, sequence b)--remove all instances of a from sequence b(delta set)
	sequence c
	c={}
	for i = 1 to length(b) do
		if not equal(a,b[i]) then c=append(c,b[i]) end if
	end for
	return c
end function

global function parser_super(sequence a)
	for i = 1 to length(named_parsers) do
		if equal(a,named_parsers[i][1]) then
			return named_parsers[i][2]
		end if
	end for
	return {}
end function


global object current_parser
current_parser={}

function rule_name(atom R)
	return parsers[R][1]
end function

global sequence rule_stack
rule_stack={}
global atom current_rule

global sequence parser_texts
parser_texts={}

global function find_rule(object text)--turns a text rule into an index of parers
sequence super
atom f
	if atom(text) then return text end if
	if length(parsers)!= length(parser_texts) then
		parser_texts={}
		for i = 1 to length(parsers) do
-- 			parser_texts=append(parser_texts, parsers[i][1])
			if length(parsers[i][1]) and sequence(parsers[i][1][1]) then
				parser_texts=append(parser_texts, parsers[i][1][1]&':'&parsers[i][1][2])
			else
				parser_texts=append(parser_texts, parsers[i][1])
			end if
		end for
-- 		pretty_print(1,parser_texts,{3})
	end if
	if length(text) and sequence(text[1]) then
		f=find(text[1]&':'&text[2], parser_texts)
	else
		f=find(text, parser_texts)
	end if
	if f then
		return f
	end if
-- 	for i = 1 to length(parsers) do
-- 		if equal(text,parsers[i][1]) then
-- 			return i
-- 		end if
-- 	end for
-- 	trace(1)
	if length(text) and atom(text[1]) then
		text={current_parser,text}
		return find_rule(text)
	end if
	if length(text) and sequence(text[1]) then
		super=parser_super(text[1])
		if length(super) then
			text[1]=super
			return find_rule(text)
		end if
	end if
	error("rule not found "&text)
end function
without trace

global type RULE     (atom   a) return a!=0 and a<=length(parsers)                           end type --routine id of rule

global atom direct_eval
global sequence rule_calls,rule_calls_cached
rule_calls=repeat(0,1000)
rule_calls_cached=repeat(0,1000)
direct_eval=0
function Eval_rule(RULE r, sequence args)--needs comment
	rule_calls[r]+=1
	if direct_eval then
		return call_func(r,args)
	else
--		current_parser={}
--		if length(parsers[r][1]) and sequence(parsers[r][1][1]) then current_parser=parsers[r][1][1] end if
		return call_func(eval_rule_rid,{r,args})
	end if
end function


-- type HEAD     (object a)
-- 	return atom(a)
-- end type --index of all_heads
type HEAD     (object a)
object ai
	if not atom(a) then return 0 end if
	if equal(a,nil) then return 1 end if
	ai=objs[a]
	if equal(ai,nil) then return 1 end if
	if sequence(ai) then
		if atom(ai[1]) and sequence(ai[2]) and sequence(ai[3]) then
			return 1
		end if
	end if
	? 1/0
	return 0
end type --index of all_heads

--type MEMOENTRY(object a) return atom(a)                         end type --index of all_memoentrys

type MEMOENTRY(object a)
object ai
	if not atom(a) then return 0 end if
	if equal(a,nil) then return 1 end if
	ai=objs[a]
	if sequence(ai) then return 1 end if
	? 1/0
	return 0
end type --index of all_memoentrys

-- type LR       (object a) return atom(a) and not equal(a,failed) end type --index of all_lrs
function is_lr(object a) return atom(a) and not equal(a,failed) end function

type LR       (object a)
object ai
	if not atom(a) then return 0 end if
	if equal(a,nil) then return 1 end if
	ai=objs[a]
	if sequence(ai) then return 1 end if
	? 1/0
	return 0
end type --index of all_lrs

procedure check_head(HEAD a)      end procedure
procedure check_memo(MEMOENTRY a) end procedure
procedure check_lr(LR a)          end procedure
constant otHead=routine_id("check_head")
constant otMemo=routine_id("check_memo")
constant otLr=  routine_id("check_lr")


sequence rule_pos_index
--should it grow past the end?

--evalSet, which holds the subset of the involved rules that may still be evaluated during the current growth cycle
--involvedSet, for the rules involved in the left recursion
--HEAD(rule:RULE,involvedSet,evalSet:Set of Rule)
-- contains the head rule of the left recursion (rule), and the following
-- two sets of rules
-- HEADS is NIL at any position where left recursion growth is not underway.
constant HEAD_RULE=1
constant HEAD_INVOLVED_SET=2
constant HEAD_EVAL_SET=3
--sequence all_heads--repeat([1]=rule,[2]=sequence involved set,[3]=sequence evalset

--what is answer
--memo
--memo entry(ans:AST or LR,pos:POS) answer is an result of eval or an LR
--if ans=failed or seq(ans) then result of eval, else atom(ans) and ans>0 then LR

constant STATE_POS=1
constant STATE_ARGS=2
constant MEMO_STATE=1--MEMO_POS=1
constant MEMO_ANS=2

--sequence all_memoentrys--repeat({position,answer})

--old LR(detected:Boolean)
-- rule invocation stack
--LR(seed:AST,rule:RULE,head:HEAD,next:LR)
--seed is the actual result from running this rule
-- LR’s seed field holds the initial parse found for the associated
-- rule, which is stored in the rule field.
-- In place of LR’s old detected
-- field, we now have the head field which, for a left-recursive invocation,
-- holds information pertinent to the left recursion (head is set
-- to NIL for non-left-recursive invocations).
-- The rule invocation stack is kept in the global variable LRStack, of
-- type LR, and is represented as a linked list, using LR’s next field.
constant LR_SEED=1
constant LR_RULE=2
constant LR_HEAD=3
constant LR_NEXT=4
--sequence all_lrs--repeat([1]=seed,[2]=rule,[3]=head,[4]=next)

sequence upos
upos={}
function fpos(sequence p)
	atom f
	if flat_pos then return p[$] end if
	f=find(p,upos)
	if f then return f end if
	upos=append(upos,p)
	return length(upos)
end function

global sequence dictionary--key, val
dictionary={}
constant def_dictionary=repeat({},#FFFF+1)

function dlook(atom n, object key, atom default)
	if n>length(dictionary) then return default end if
	for i = 1 to length(dictionary[n]) do
		if equal(dictionary[n][i][1],key) then return dictionary[n][i][2] end if
	end for
	return default
end function

procedure dset(atom n, object key, object val)
	if n>length(dictionary) then dictionary&=repeat({},n-length(dictionary)) end if
	for i = 1 to length(dictionary[n]) do
		if equal(dictionary[n][i][1],key) then dictionary[n][i][2]=val return end if
	end for
	dictionary[n]=append(dictionary[n],{key,val})
end procedure

function hdlook(object key, atom default)
-- 	return dlook(key[1][$],key,default)
	return dlook(and_bits(key[1][$],#FFFF)+1,key,default)
end function

procedure hdset(object key, object val)
-- 	dset(key[1][$],key,val)
	dset(and_bits(key[1][$],#FFFF)+1,key,val)
end procedure

global procedure dump_dictionary()
	atom fn
	object a
	fn=open("temp_dictionary.txt","w")
	for i = 1 to length(dictionary) do
		for ii = 1 to length(dictionary[i]) do
			a=dictionary[i][ii]
			if length(a[1])=3 and a[1][3]<=length(parsers) then
				printf(fn,"pos=%s args=%s rule=%s memo=",{
					sprint(a[1][1]),
					sprint(a[1][2]),
					parsers[a[1][3]][1]})
					pretty_print(fn,objs[a[2]][2],{3}) puts(fn,10)
			elsif length(a[1])=2 and a[2] then
				printf(fn,"pos=%s args=%s head=%d\n",{
					sprint(a[1][1]),
					sprint(a[1][2]),
					sprint(objs[a[2]][2])})
			elsif length(a[1])=2 then
				printf(fn,"pos=%s args=%s head=nil\n",{
					sprint(a[1][1]),
					sprint(a[1][2])})
			end if
		end for
	end for
end procedure
dump_dictionary_rid=routine_id("dump_dictionary")


function  get_head(sequence state_pa)--state_pa
-- 	? state_pa
	return hdlook(state_pa,nil)
-- 	if(fpos(state_pa[STATE_POS]))>length(rule_pos_index) then return nil end if
-- 	return rule_pos_index[fpos(state_pa[STATE_POS])][1]
end function
procedure set_head(sequence state_pa, HEAD head)--state_pa
	hdset(state_pa,head)
-- 	if(fpos(state_pa[STATE_POS]))>length(rule_pos_index) then return     end if
-- 	rule_pos_index[fpos(state_pa[STATE_POS])][1]=head
end procedure
function  get_memo(sequence state_pa, RULE rule)--state_pa
-- sequence spr
-- 	if object(parsers[rule]) then end if
-- 	spr=append(state_pa,rule)
-- 	if length(spr[1])!=2 or spr[1][1]!=1 or length(spr[2])!=0 then ? spr ? 1/0 end if
	
	return dlook(and_bits(state_pa[1][$]+rule*256,#FFFF)+1,append(state_pa,rule),nil)
	--hdlook(append(state_pa,rule),nil)
-- 	if(fpos(state_pa[STATE_POS]))>length(rule_pos_index) then return nil end if
-- 	return rule_pos_index[fpos(state_pa[STATE_POS])][2][rule]
end function
procedure set_memo(sequence state_pa, RULE rule, MEMOENTRY memo)--state_pa
-- 	if object(parsers[rule]) then end if
	--hdset(append(state_pa,rule),memo)
	dset(and_bits(state_pa[1][$]+rule*256,#FFFF)+1,append(state_pa,rule),memo)
-- 	if(fpos(state_pa[STATE_POS]))>length(rule_pos_index) then return     end if
-- 	rule_pos_index[fpos(state_pa[STATE_POS])][2][rule]=memo
end procedure

function    new_head(RULE r, sequence involvedSet, sequence evalSet) otypes&=otHead objs=append(objs,{r,involvedSet,evalSet}) return length(objs) end function
function    new_memoentry(sequence P, object ans)                    otypes&=otMemo objs=append(objs,{P,ans})                 return length(objs) end function
function    new_lr(object seed, RULE r, HEAD h, LR next)             otypes&=otLr   objs=append(objs,{seed,r,h,next})         return length(objs) end function

LR LRStack--next lr
LRStack=0
-- An invocation of rule R is left-recursive if R is already on the
-- rule invocation stack, and the parser’s position has not changed
-- since that first invocation.

sequence bpos_stack
bpos_stack={}

atom last_tick
last_tick=time()
procedure track(RULE R)
object bf,af
	if trace_msg_pos_=-1 then return end if
-- 	if last_tick<time() then
		last_tick+=0.10
--		trace_msg(sprint(pos&length(src[1])))
-- -- 		trace_pos(parsing_file_name,{getfpos(bpos),getfpos(pos)})
		bf=getfpos(bpos)
		af=getfpos(pos)
-- ? bf
-- ? af
trace_msg_pos_rid(parsers[R][1],parsing_file_name,{bf,af})
-- 	end if
end procedure

function implode(object parts, object glue)
sequence ret
	if atom(parts) then return parts end if
	ret={}
	for i = 1 to length(parts) do
		if i<length(parts) then
			ret&=parts[i]&glue
		else
			ret&=parts[i]
		end if
	end for
	return ret
end function

atom lt
lt=open("lt.txt","w")

global atom lt_trace
lt_trace=0
global sequence rule_stats
rule_stats={}
procedure trace_rule_stack(object ret)
sequence pr
	if not lt_trace then return end if
	if failed_rule(ret) then
		return
		puts(lt,"F")
	else
		puts(lt,"S")
	end if
	printf(lt,"%10d ",pos[$])
	pr=rule_stack
	for i = 1 to length(pr) do
		if i=length(pr) then
			if pr[i]>length(rule_stats) then
				rule_stats&=repeat({0,0},pr[i]-length(rule_stats))
			end if
			if failed_rule(ret) then
				rule_stats[pr[i]][1]+=1
			else
				rule_stats[pr[i]][2]+=1
			end if
		end if
		pr[i]=parsers[pr[i]][1]
		if sequence(pr[i]) and length(pr[i])=2 and sequence(pr[i][1]) then
-- 			pr[i]=pr[i][1]&':'&pr[i][2]
			pr[i]=pr[i][2]
		end if
	end for
-- 	print(lt,ret)
	puts(lt, 32&implode(pr,' '))
-- 	pretty_print(lt,rule_stack,{3})
-- 	pretty_print(lt,ret,{3})
	puts(lt,10)
end procedure

with trace
global function apply_rule(RULE R, sequence state_pos, sequence state_args)--applies this rule at this position
--state_pos used to be P
--state_pa
	MEMOENTRY m
	LR lr,s
	object ans
	HEAD h
	sequence state_pa
	
-- 	trace(1)
	rule_stack=append(rule_stack,R)
-- 	if length(parsers) then
-- 		rule_stack=append(rule_stack,parsers[R][1])
-- 	else
-- 		rule_stack&=R
-- 	end if
	
-- 	printf(lt,"applyRule %s %d %s\n",{rule_name(R),state_pos,sprint(state_args)})
	
	bpos_stack=append(bpos_stack,bpos)
	bpos=pos
-- 	task_yield()
	--track(R)
	state_pa={state_pos,state_args}
-- 	? state_pos
	
	--start recall
	m=get_memo(state_pa,R)
	--m=get_memo(P,R)
	h=get_head(state_pa)
-- 	h=get_head(P)
	if h!=nil then--then it is growing
-- 		printf(lt,"growing %s\n",{rule_name(R)})
		if m=nil and (R=objs[h][HEAD_RULE] or find(R,objs[h][HEAD_INVOLVED_SET])) then --if m=nil and (R=h.rule or find(R,h.involvedSet)) then
-- 			printf(lt,"rule is involved %s\n",{rule_name(R)})
			--do not evaluate any rule that is not involved in this left recursion.
			--m=new_memoentry(0,failed)
			otypes&=otMemo
			objs=append(objs,{0,failed})
			m=length(objs)
		elsif find(R,objs[h][HEAD_EVAL_SET]) then--elsif find(R,h.evalSet) then
-- 			printf(lt,"rule is evalSet %s\n",{rule_name(R)})
			objs[h][HEAD_EVAL_SET]=remove(R,objs[h][HEAD_EVAL_SET])--h.evalSet=remove(R,h.evalSet)
			objs[m][MEMO_ANS]=Eval_rule(R,state_args)--m.ans=Eval_rule(R)
			trace_rule_stack(objs[m][MEMO_ANS])
			--eval rule can change the position
			objs[m][MEMO_STATE][STATE_POS]=pos--m.pos=pos
		end if
	end if
	--end recall
	
	if m=nil then                             --this rule has not been tried at this position
-- 		printf(lt,"untried rule %s\n",{rule_name(R)})

		--lr=new_lr(failed,R,0,LRStack)           --create a new lr and push it onto the rule invocation stack
		otypes&=otLr
		objs=append(objs,{failed,R,0,LRStack})
		lr=length(objs)
		LRStack=lr
		
		--m=new_memoentry(P,lr)                   --memorize lr, then evaluate R
		otypes&=otMemo
		objs=append(objs,{state_pa,lr})
-- 		objs=append(objs,{P,lr})
		
		m=length(objs)
		set_memo(state_pa,R,m)
-- 		set_memo(P,R,m)
		ans=Eval_rule(R,state_args)
		trace_rule_stack(ans)
-- 		printf(lt,"ur %s result %s new pos %s\n",{rule_name(R),sprint(ans),sprint(pos)})
	--LRStack=LRStack.next                    --pop lr off the rule invocation stack
		LRStack=objs[LRStack][LR_NEXT]
		
	--m.pos=pos
		objs[m][MEMO_STATE][STATE_POS]=pos
	--if lr.head!=nil then
		if objs[lr][LR_HEAD]!=nil then
-- 			printf(lt,"not growing %s\n",{rule_name(R)})
			                                        --not growing
		--lr.seed=ans
			objs[lr][LR_SEED]=ans
			                                        --when it detects left recursion.
		--h=m.ans.head
			h=objs[objs[m][MEMO_ANS]][LR_HEAD]
		--if h.rule!=R then			
			if objs[h][HEAD_RULE]!=R then
-- 				printf(lt,"h.rule!=rule %s\n",{rule_name(R)})
			--m.ans.seed
				bpos=bpos_stack[$] bpos_stack=bpos_stack[1..$-1]
				track(R)
				rule_stack=rule_stack[1..$-1]
				return objs[objs[m][MEMO_ANS]][LR_SEED]
			else
			--m.ans=m.ans.seed
				objs[m][MEMO_ANS]=objs[objs[m][MEMO_ANS]][LR_SEED]
			--if failed_rule(m.ans) then
				--if failed_rule(objs[m][MEMO_ANS]) then
				if equal(objs[m][MEMO_ANS],failed) then
-- 					printf(lt,"m.ans is FailedRule %s\n",{rule_name(R)})
					bpos=bpos_stack[$] bpos_stack=bpos_stack[1..$-1]
					track(R)
					rule_stack=rule_stack[1..$-1]
					return failed
				else
-- 					printf(lt,"start grow_lr %s\n",{rule_name(R)})
																											--If the current rule is the head of the left recursion, LR-ANSWER invokes GROWLR
																											--Otherwise, the current rule is involved in the left recursion and must defer to the head rule to grow any 
																											--left recursive parse, and pass its current parse to participate in the construction of a seed parse.
					--start grow_lr
					set_head(state_pa,h)                              --which indicates that left recursion growth is in progress.
-- 					set_head(P,h)                              --which indicates that left recursion growth is in progress.
					while 1 do
						pos=state_pa[1]
-- 						pos=P
						                                          --For each cycle of growth, the involved rules are given a fresh opportunity for evaluation.
					--h.evalSet=h.involvedSet
						objs[h][HEAD_EVAL_SET]=objs[h][HEAD_INVOLVED_SET]
						ans=Eval_rule(R,state_args)
						trace_rule_stack(ans)
					--if failed_rule(ans) or compare(pos,m.pos)<=0 then
						--if failed_rule(ans) or compare(pos,objs[m][MEMO_POS])<=0 then
						if equal(ans,failed) or compare(pos,objs[m][MEMO_STATE][STATE_POS])<=0 then
							exit
						end if
					--m.ans=ans
						objs[m][MEMO_ANS]=ans
					--m.pos=pos						
						objs[m][MEMO_STATE][STATE_POS]=pos
					end while
					set_head(state_pa,nil)                               --Finally, when left recursion growth is completed, the head at the left recursion position must be removed.
-- 					set_head(P,nil)                               --Finally, when left recursion growth is completed, the head at the left recursion position must be removed.
				--pos=m.pos
					pos=objs[m][MEMO_STATE][STATE_POS]
				--return m.ans					
					bpos=bpos_stack[$] bpos_stack=bpos_stack[1..$-1]
					track(R)
					rule_stack=rule_stack[1..$-1]
					return objs[m][MEMO_ANS]
					--end of grow_lr
				end if
			end if
			--end of lr_answer
			                                                    --If the current rule is the head of the left recursion, LR-ANSWER invokes GROWLR just as we did before.
		else
			                                                    --is growing
-- 			printf(lt,"is growing %s\n",{rule_name(R)})
		--m.ans=ans
			objs[m][MEMO_ANS]=ans
			bpos=bpos_stack[$] bpos_stack=bpos_stack[1..$-1]
			track(R)
				rule_stack=rule_stack[1..$-1]
			return ans
		end if
	else
		rule_calls_cached[R]+=1
-- 		printf(lt,"before %s\n",{rule_name(R)})
		                                                      --this rule has been tried at this position before
	--pos=m.pos
		pos=objs[m][MEMO_STATE][STATE_POS]
	--ans=m.ans		
		ans=objs[m][MEMO_ANS]
		if not is_lr(ans) then
-- 		printf(lt,"not lr %s\n",{rule_name(R)})
			bpos=bpos_stack[$] bpos_stack=bpos_stack[1..$-1]
			track(R)
			rule_stack=rule_stack[1..$-1]
			return ans
		else
-- 			printf(lt,"start of setup_lr %s\n",{rule_name(R)})
			--start of setup_lr
		--if ans.head=nil then
			if objs[ans][LR_HEAD]=nil then
			--ans.head=new head(R,{},{})
				h=new_head(R,{},{})
				objs[ans][LR_HEAD]=h
			end if
			s=LRStack
		--while s.head!=ans.head do
			while objs[s][LR_HEAD]!=objs[ans][LR_HEAD] do
			--s.head=ans.head
				objs[s][LR_HEAD]=objs[ans][LR_HEAD]
			--ans.head.involvedSet&=s.rule
				objs[objs[ans][LR_HEAD]][HEAD_INVOLVED_SET]&=objs[s][LR_RULE]
			--s=s.next
				s=objs[s][LR_NEXT]
			end while
			--end setup_lr
		--return ans.seed
			bpos=bpos_stack[$] bpos_stack=bpos_stack[1..$-1]
			track(R)
			rule_stack=rule_stack[1..$-1]
			return objs[ans][LR_SEED]
		end if
	end if
end function


sequence pstack
pstack={}

global procedure push_pr()
	pstack=append(pstack,
		{
			otypes,
			objs,
			LRStack,
			direct_eval,
			pos,
			bpos,
			src,
			dictionary,
			current_parser
		}
	)
end procedure

global procedure pop_pr()
		otypes=pstack[$][1]
		objs=pstack[$][2]
		LRStack=pstack[$][3]
		direct_eval=pstack[$][4]
		pos=pstack[$][5]
		bpos=pstack[$][6]
		src=pstack[$][7]
		parsing_file_line_numbers=get_line_numbers(src[1])
		dictionary=pstack[$][8]
		current_parser=pstack[$][9]
		pstack=pstack[1..$-1]
end procedure

global procedure init_pr(atom d_eval, sequence npos, object src_val, atom num_rules)
--	all_heads={}
--	all_memoentrys={}
--	all_lrs={}
	otypes={}
	objs={}
	LRStack=new_lr(-1,-1,0,0)
	direct_eval=d_eval
	pos=npos--{1}
	bpos=pos
	src={src_val}
	
	parsing_file_line_numbers=get_line_numbers(src[1])
	
-- 	rule_pos_index=repeat({nil,repeat(nil,num_rules)},length(src_val))
	dictionary=def_dictionary
end procedure
