#MEMOENTRY : (ans : AST or LR,pos : POSITION)
class MEMOENTRY
	constructor: (@ans, @pos)->
		console.log 'class MEMOENTRY', ans: ans, pos: pos
		@type = 'MEMOENTRY'

#LR : (seed : AST,rule : RULE,head : HEAD,next : LR)
class LR
	constructor: (@seed, @rule, @head, @next)->
		console.log 'class LR', seed: seed, rule: rule, head: head, next: next
		@type = 'LR'

#HEAD : (rule : RULE,involvedSet,evalSet : SET of RULE)
class HEAD
	constructor: (@rule, @involvedSet, @evalSet)->
		console.log 'class HEAD', rule: rule, involvedSet: involvedSet, evalSet: evalSet
		@type = 'HEAD'

#MEMO : (RULE,POS) -> MEMOENTRY
memo_dict = Object.create(null)
MEMO = (RULE, POS)->
	try
		ret = memo_dict[RULE][POS]
		console.log 'MEMO(', RULE: RULE, POS: POS, ') = ', ret
		return ret
	catch e
		console.log 'MEMO(', RULE: RULE, POS: POS, ') = ', e
		return undefined

MEMO_STORE = (RULE, POS, MEMOENTRY)->
	console.log 'MEMO_STORE(', RULE: RULE, POS: POS, MEMOENTRY: MEMOENTRY, ')'

#HEADS : POSITION -> HEAD #HEADS is NIL at any position where left recursion growth is not underway.
heads_array = []
HEADS = (POSITION)->
	try
		ret = heads_array[POSITION]
		console.log 'HEADS(', POSITION: POSITION, ') = ', ret
		return ret
	catch e
		console.log 'HEADS(', POSITION: POSITION, ') = ', e
		return undefined

HEADS_STORE = (POSITION, HEAD)->
	console.log 'HEADS_STORE(', POSITION: POSITION, HEAD: HEAD, ')'

RECALL = (R, P)->
	console.log 'RECALL(', R: R, P: P, ')'
	m = MEMO(R, P)
	h = HEADS(P)
	# If not growing a seed parse, just return what is stored
	# in the memo table.
	unless h?
		return m
	# Do not evaluate any rule that is not involved in this
	# left recursion.
	if (!(m?)) and NOT_ELEMENT_OF(R, UNION([h.head], h.involvedSet))
		return new MEMOENTRY(false, P)
	# Allow involved rules to be evaluated, but only once,
	# during a seed-growing iteration.
	if R ELEMENT_OF h.evalSet
		h.evalSet = h.evalSet RELATIVE_COMPLEMENT [R]
		ans = EVAL(R.body)
		m.ans = ans
		m.pos = Pos
	return m

LR_ANSWER = (R, P, M)->
	console.log 'LR_ANSWER(', R: R, P: P, M: M, ')'
	h = M.ans.head
	if h.rule isnt R
		return M.ans.seed
	else
		M.ans = M.ans.seed
		if M.ans = false
			return false
		else
			return GROW_LR(R, P, M, h)

SETUP_LR = (R, L)->
	console.log 'SETUP_LR(', R: R, L: L, ')'
	unless L.head?
		L.head = new HEAD(R, [], [])
	s = LRStack
	while s.head isnt L.head
		s.head = L.head
		L.head.involvedSet = UNION(L.head.involvedSet, [s.rule])
		s = s.next

APPLY_RULE = (R, P)->
	console.log 'APPLY_RULE(', R: R, P: P, ')'
	m = RECALL(R, P)
	unless m?
		# Create a new LR and push it onto the rule
		   # invocation stack.
		lr = new LR(false, R, undefined, LRStack)
		LRStack = lr
		# Memoize lr, then evaluate R.
		m = new MEMOENTRY(lr,P)
		MEMO_STORE(R, P, m)
		ans = EVAL(R.body)
		# Pop lr off the rule invocation stack.
		LRStack = LRStack.next
		m.pos = Pos
		if lr.head?
			lr.seed = ans
			return LR_ANSWER(R, P, m)
		else
			m.ans = ans
			return ans
	else
		Pos = m.pos
		if m.ans is LR
			SETUP_LR(R, m.ans)
			return m.ans.seed
		else
			return m.ans

GROW_LR = (R, P, M, H)->
	console.log 'GROW_LR(', R: R, P: P, M: M, H: H, ')'
	HEADS_STORE(P, H)#line A
	while TRUE
		Pos = P
		H.evalSet = COPY(H.involvedSet)#line B
		ans = EVAL(R.body)
		if ans = false or Pos = M.pos
			break
		M.ans = ans
		M.pos = Pos
		HEADS_STORE(P, undefined) #line C
	Pos = M.pos
	return M.ans

EVAL = (body)->
	console.log 'EVAL(', body: body, ')'
	return true

console.log 'APPLY_RULE(\'parse\', 0)', APPLY_RULE('parse', 0)

