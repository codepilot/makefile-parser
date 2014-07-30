fs = require 'fs'
util = require('util')

#seq = require('./seu.coffee').unserial_eu './../preFiles_results/makefile_004.seu'


deep_copy = (thing)->
	return thing unless thing?
	try
		str = JSON.stringify(thing)
		ret = JSON.parse(str)
		return ret
	catch e
		console.log 'str', str
		throw e

#console.log JSON.stringify seq

indention = 0
pretty_print = (val, maxDepth = 2)->
	if typeof val is 'object'
		if val.length is 0
			console.log ('  ' for n in [0...indention]).join('') + '{}'
		else
			isAscii = true
			for n in val
				unless typeof n is 'number' and n >= 0 and n <= 255 and (n is (n >>> 0))
					isAscii = false
					break
			if isAscii
				console.log ('  ' for n in [0...indention]).join('') + JSON.stringify (String.fromCharCode(n) for n in val).join('')
			else if indention > maxDepth
				console.log ('  ' for n in [0...indention]).join('') + '<SEQ>'
			else
				console.log ('  ' for n in [0...indention]).join('') + '{'
				indention++
				for n in val
					pretty_print n
				indention--
				console.log ('  ' for n in [0...indention]).join('') + '}'
	else
		console.log ('  ' for n in [0...indention]).join('') + val

#pretty_print seq

convert_cache = (val, maxDepth = 2)->
	if typeof val is 'object'
		if val.length is 0
			[]
		else
			isAscii = true
			for n in val
				unless (typeof n is 'number' and n >= 0 and n <= 255 and (n is (n >>> 0))) or (typeof n is 'string' and n.length is 1)
					isAscii = false
					break
			if isAscii
				((if typeof n is 'number' then String.fromCharCode(n) else n) for n in val).join('')
			else
				for n in val
					convert_cache n
	else
		val


#console.log util.inspect gnu_make_converted, depth: null, colors: true
#console.log ''

convert_rule = (steps)->
#	console.log 'convert_rule', util.inspect steps, depth: null, colors: true
	ret = []
	for step in steps
		switch typeof step
			when 'string'
				console.log 'string', util.inspect step, depth: null, colors: true
#				ret.push step
			when 'number'
				console.log 'number', util.inspect step, depth: null, colors: true
#				ret.push step
			when 'object'
				if typeof step[0] is 'string'
#					console.log 'step', util.inspect step, depth: null, colors: true
					switch step[0]
						when 'choices', 'host2', '*', '+', '!', 'dofirst', 'query', 'add', 'sub', 'mul', 'div'
							ri = {step: step[0], arg1: convert_rule(step[1])}
							ri.name = step[2] if step.length >= 3
							ret.push ri
						when 'return', 'seq', 'concat'
							ri = {step: step[0], arg1: convert_rule([step[1]])[0]}
							ret.push ri
						when 'char', 'producer', 'ranges', 'lit', 'subs'
							ri = {step: step[0], arg1: step[1]}
							ri.name = step[2] if step.length >= 3
							ret.push ri
						when 'func'
							ri = {step: step[0], func: step[1], args: convert_rule(step[2])}
							ret.push ri
						when 'producer2'
							ri = {step: step[0], arg1: step[1][0], arg2: convert_rule(step[1][1])}
							ri.name = step[2] if step.length >= 3
							ret.push ri
							
						else
							#console.log 'convert_rule', 'unknown step', util.inspect step, depth: null, colors: true
							console.log 'convert_rule', 'unknown step'
							console.log JSON.stringify(step, null, '  ')
							ri = {step: step[0], arg1: step[1], arg2: step[2]}
							ret.push ri
							throw err
				else
					ret.push convert_rule step
	ret


make_rules = (converted)->
	ret = {}
	for l0 in converted
#		console.log 'l0'
#		console.log JSON.stringify l0, null, '  '
		for l1 in l0
#			console.log l1[0]
#			console.log JSON.stringify l1, null, '  '
			ret[l1[0]] = body: convert_rule l1[1]
			if l1.length >= 3
				ret[l1[0]].args = l1[2]
	return ret

rules = {}

pos = 0
src = ''

debugHost2 = false

host2e = (vars, n)->
	switch n.step
		when 'subs'
			if debugHost2
				console.log 'host2e subs'
				console.log util.inspect n, depth: null, colors: true
				console.log util.inspect vars, depth: null, colors: true

			retVal = vars
			for n in n.arg1[0]
				retVal = retVal[n]
			return retVal
		when 'seq'
			retVal = for n in n.arg1
				host2e(vars, n)
			return retVal

		when 'add' then return host2e(vars, n.arg1[0]) + host2e(vars, n.arg1[1])
		when 'sub'
			argA = host2e(vars, n.arg1[0])
			argB = host2e(vars, n.arg1[1])

			if typeof argA is 'string' and argA.length is 1 and typeof argB is 'number'
				return argA.charCodeAt(0) - argB

			if typeof argA is 'number'  and typeof argB is 'number'
				return argA - argB

			throw err

		when 'mul' then return host2e(vars, n.arg1[0]) * host2e(vars, n.arg1[1])
		when 'div' then return host2e(vars, n.arg1[0]) / host2e(vars, n.arg1[1])

		when 'lit'
			return n.arg1
		when 'concat'
			#console.log util.inspect n, depth: null, colors: true
			#console.log vars
			retVal = []
			parts = []
			isAllStrings = true
			for n in n.arg1
				rv = host2e(vars, n)
				isAllStrings = false unless typeof rv is 'string'
				parts.push rv
				retVal = retVal.concat rv
			if isAllStrings
				retVal = parts.join('')
			#else
			#	console.log 'concat retval', retVal, 'parts', parts
			#	console.log ''
			return retVal
		when 'func'
			switch n.func
				when 'equal'
					retVal = for n in n.args
						host2e(vars, n)
					if debugHost2
						console.log 'equal', retVal, JSON.stringify(retVal[0]) is JSON.stringify(retVal[1])
					return JSON.stringify(retVal[0]) is JSON.stringify(retVal[1])
				else
					console.log 'host2e unknown'
					console.log util.inspect vars, depth: null, colors: true
					console.log util.inspect n, depth: null, colors: true
					console.log JSON.stringify n
					throw err
		else
			console.log 'host2e unknown'
			console.log util.inspect vars, depth: null, colors: true
			console.log util.inspect n, depth: null, colors: true
			console.log JSON.stringify n
			throw err

host2 = (vars, r)->
	for n in r
		switch n.step
			when 'return'
				if debugHost2
					console.log 'host2 return', util.inspect n.arg1, depth: null, colors: true

				return host2e(vars, n.arg1)
			else
				console.log 'host2 unknown'
				console.log util.inspect n, depth: null, colors: true
				console.log JSON.stringify n
				throw err

	return


#EVAL = (body)->
#	console.log 'EVAL(', body: body, ')'
#	return true

debugEVAL = false

indention = 0
EVAL = (r, arg_names=[], args=[])->
	if false and debugEVAL
		console.log 'EVAL(', body: r, ')'

	tempPos = pos
	indention++
	hasFailed = false
	listing = []
	rseq = undefined
	vars = id: {}
	for arg_name, i in arg_names
		vars.id[arg_name] = args[i]

#	if arg_names.length
#		console.log vars

	for n in r
		if debugEVAL
			console.log ('  ' for i in [0...indention]).join('') + n.step

		switch n.step

			when 'ranges'
				isRangeGood = false
				for range in n.arg1
					if range.length is 2
						if typeof range[0] is 'string' and typeof range[1] is 'string' and typeof src[pos] is 'string'
							if range[0] <= src[pos] and src[pos] <= range[1]
								isRangeGood = true
								break
						else if typeof range[0] is 'string' and typeof range[1] is 'string' and typeof src[pos] is 'number'
							if range[0] <= String.fromCharCode(src[pos]) and String.fromCharCode(src[pos]) <= range[1]
								isRangeGood = true
								break
						else if typeof range[0] is 'string' and typeof range[1] is 'string' and typeof src[pos] is 'undefined'
							break
						else
							console.log 'range', JSON.stringify(range)
							console.log 'src[pos]', JSON.stringify(src[pos])
							throw err
				unless isRangeGood
					indention--
					pos = tempPos
					return
				rseq = src[pos]
				listing.push rseq
				pos++

				if n.name?
					vars.id[n.name] = rseq
					if debugEVAL
						console.log id: vars.id

			when 'char'
				charFailed = false
				isRealString = true
				if debugEVAL
					console.log ('  ' for i in [0...indention]).join('') + '  ', JSON.stringify n.arg1
					console.log ('  ' for i in [0...indention]).join('') + '  ', pos

				for c, i in n.arg1
					if typeof c is 'string' and typeof src[pos + i] is 'string'
						if c isnt src[pos + i]
							charFailed = true
							break
					else if typeof c is 'string' and typeof src[pos + i] is 'number'
						isRealString = false
						if c isnt String.fromCharCode(src[pos + i])
							charFailed = true
							break
					else if typeof c is 'string' and typeof src[pos + i] is 'undefined'
						isRealString = false
						charFailed = true
						break
					else
						console.log 'n.arg1', JSON.stringify(n.arg1)
						console.log 'c', JSON.stringify(c)
						console.log 'src[pos + i]', JSON.stringify(src[pos + i])
						throw err

				if debugEVAL
					console.log ('  ' for i in [0...indention]).join('') + '  ', not charFailed

				if charFailed
					hasFailed = true
					indention--
					pos = tempPos
					return
					undefined
				else
					if isRealString
						rseq = n.arg1
					else if n.arg1.length is 1
						rseq = src[pos]
					else
						rseq = (b for b in src[pos...pos + n.arg1.length])
					pos += n.arg1.length
					listing.push rseq

				if n.name?
					vars.id[n.name] = rseq
					if debugEVAL
						console.log id: vars.id

			when 'host2'
				if JSON.stringify(n) is '{"step":"host2","arg1":[{"step":"return","arg1":{"step":"seq","arg1":[]}}]}'
					indention--
					return []
				retVal = host2(vars, n.arg1)
				if debugEVAL
					console.log 'host2', JSON.stringify retVal, null, '  '

				rseq = retVal
				listing.push rseq

				if n.name?
					vars.id[n.name] = rseq
					if debugEVAL
						console.log id: vars.id

			when 'query'
				retVal = host2(vars, n.arg1)
				if debugEVAL
					console.log 'host2', retVal

				switch typeof retVal
					when 'boolean'
						unless retVal
							if debugEVAL
								console.log n.step, 'failure', JSON.stringify(retVal)
							indention--
							pos = tempPos
							return
					when 'string' then
					when 'object'
						unless retVal is null
							if debugEVAL
								console.log n.step, 'failure', JSON.stringify(retVal)
							indention--
							pos = tempPos
							return
					when 'undefined'
						if debugEVAL
							console.log n.step, 'failure', JSON.stringify(retVal)
						indention--
						pos = tempPos
						return

				if debugEVAL
					console.log n.step, 'success', JSON.stringify(retVal)

				rseq = retVal
				listing.push rseq

				if n.name?
					vars.id[n.name] = rseq
					if debugEVAL
						console.log id: vars.id

			when 'dofirst'
				retVal = EVAL n.arg1
				unless retVal?
					indention--
					pos = tempPos
					return

				rseq = retVal
				listing.push rseq
				if n.name?
					vars.id[n.name] = retVal
					if debugEVAL
						console.log id: vars.id

			when '*', '+'
				retVal = []
				isCharArray = true
				loop
					rv = EVAL n.arg1
					break unless rv?
					isCharArray = false unless typeof rv is 'string' and rv.length is 1
					retVal.push rv

				unless n.step is '*' or retVal.length
					if debugEVAL
						console.log n.step, 'failure', JSON.stringify(retVal)
					indention--
					pos = tempPos
					return

				if isCharArray and retVal.length > 0
					retVal = retVal.join ''

				rseq = retVal

				if debugEVAL
					console.log n.step, 'success', JSON.stringify(rseq)

				listing.push rseq
				if n.name?
					vars.id[n.name] = retVal
					if debugEVAL
						console.log id: vars.id

			when 'producer'
				retVal = APPLY_RULE n.arg1, pos
				unless retVal?
					if debugEVAL
						console.log n.step, n.arg1, 'failure', JSON.stringify(retVal)
					hasFailed = true
					indention--
					pos = tempPos
					return

				if debugEVAL
					console.log n.step, n.arg1, 'success', JSON.stringify(retVal)

				rseq = retVal
				listing.push rseq
				if n.name?
					vars.id[n.name] = retVal
					if debugEVAL
						console.log id: vars.id

			when 'producer2'
				callArgs = for arg2n in n.arg2
					host2e(vars, arg2n)

				retVal = APPLY_RULE n.arg1, pos, callArgs
				unless retVal?
					if debugEVAL
						console.log n.step, n.arg1, JSON.stringify(callArgs), 'failure', JSON.stringify(retVal)
					hasFailed = true
					indention--
					pos = tempPos
					return

				if debugEVAL
					console.log n.step, n.arg1, JSON.stringify(callArgs), 'success', JSON.stringify(retVal)

				rseq = retVal
				listing.push rseq
				if n.name?
					vars.id[n.name] = retVal
					if debugEVAL
						console.log id: vars.id

			when '!'
				beforePos = pos
				rseq = not (EVAL(n.arg1)?)
				pos = beforePos
				unless rseq
					indention--
					pos = tempPos
					return

			when 'choices'
				isGoodChoice = false
				goodChoice = undefined
				for choice in n.arg1
					goodChoice = EVAL choice
					if goodChoice?

						if debugEVAL
							console.log ('  ' for i in [0...indention]).join('') + '  ', 'goodChoice', goodChoice

						isGoodChoice = true
						rseq = goodChoice
						listing.push rseq
						break
				unless isGoodChoice
					indention--
					pos = tempPos
					return

			else
				console.log 'unknown step'
				console.log util.inspect n, depth: null, colors: true
				console.log ''
				throw err

	if hasFailed
		indention--
		pos = tempPos
		return
	indention--

	if debugEVAL
		console.log ('  ' for i in [0...indention]).join(''), 'listing', listing
		console.log ('  ' for i in [0...indention]).join(''), 'rseq', rseq

	listing

	if debugEVAL
		console.log pos: pos

	rseq

DebugLogProc = false
lpIndent = 0
logProc = (proc, args)->
	return unless DebugLogProc
#	console.log ('  ' for i in [0...lpIndent]).join('') + proc + '(', args, ')'
#	console.log ('  ' for i in [0...lpIndent]).join('') + proc + '(', util.inspect(args, depth: null, colors: true), ')'
	console.log ('  ' for i in [0...lpIndent]).join('') + proc, JSON.stringify(args)

logProcRet = (proc, ret)->
	return unless DebugLogProc
	console.log ('  ' for i in [0...lpIndent]).join('') + proc + '->', JSON.stringify(ret)


isNIL = (v)-> typeof v is 'undefined'
isntNIL = (v)-> typeof v isnt 'undefined'

#MEMOENTRY : (ans : AST or LR,pos : POSITION)
class MEMOENTRY
	constructor: (@ans, @pos)->
		@pos = JSON.stringify(@pos)
#		console.log 'class MEMOENTRY', ans: ans, pos: pos
		logProc 'class MEMOENTRY',
			ans: @ans
			pos: @pos

		@type = 'MEMOENTRY'

#LR : (seed : AST,rule : RULE,head : HEAD,next : LR)
class LR
	constructor: (@seed, @rule, @head, @next)->
#		console.log 'class LR', seed: seed, rule: rule, head: head, next: next
		logProc 'class LR',
			seed: seed
			rule: rule
			head: head
			next: next

		@type = 'LR'

#HEAD : (rule : RULE,involvedSet,evalSet : SET of RULE)
class HEAD
	constructor: (@rule, @involvedSet, @evalSet)->
#		console.log 'class HEAD', rule: rule, involvedSet: involvedSet, evalSet: evalSet
		logProc 'class HEAD',
			rule: rule
			involvedSet: involvedSet
			evalSet: evalSet

		@type = 'HEAD'

memo_dict = undefined
#MEMO : (RULE,POS) -> MEMOENTRY
MEMO = (RULE, POS)->
	POS=JSON.stringify(POS)
	logProc 'MEMO',
		RULE: RULE
		POS: POS
	try
		ret = memo_dict[RULE][POS]
#		console.log 'MEMO(', RULE: RULE, POS: POS, ') = ', ret
		logProcRet 'MEMO', ret

		return ret
	catch e
#		console.log 'MEMO(', RULE: RULE, POS: POS, ') = ', e
		logProcRet 'MEMO', undefined

		return undefined

MEMO_STORE = (RULE, POS, MEMOENTRY)->
#	console.log 'MEMO_STORE(', RULE: RULE, POS: POS, MEMOENTRY: MEMOENTRY, ')'
	POS=JSON.stringify(POS)
	logProc 'MEMO_STORE',
		RULE: RULE
		POS: POS
		MEMOENTRY: MEMOENTRY

	unless memo_dict[RULE]?
		memo_dict[RULE] = []
	memo_dict[RULE][POS] = MEMOENTRY

#HEADS : POSITION -> HEAD #HEADS is NIL at any position where left recursion growth is not underway.

heads_array = undefined
HEADS = (POSITION)->
	POSITION=JSON.stringify(POSITION)
	logProc 'HEADS',
		POSITION: POSITION
	try
		ret = heads_array[POSITION]
#		console.log 'HEADS(', POSITION: POSITION, ') = ', ret
		logProcRet 'HEADS', ret

		return ret
	catch e
#		console.log 'HEADS(', POSITION: POSITION, ') = ', e
		logProcRet 'HEADS', undefined

		return undefined

HEADS_STORE = (POSITION, HEAD)->
#	console.log 'HEADS_STORE(', POSITION: POSITION, HEAD: HEAD, ')'
	POSITION=JSON.stringify(POSITION)
	logProc 'HEADS_STORE',
		POSITION: POSITION
		HEAD: HEAD

	heads_array[POSITION] = HEAD

ELEMENT_OF = (element, set)->
	logProc 'ELEMENT_OF',
		element: element
		set: set

	unless set.length
		logProcRet 'HEADS', false
		return false
	
	index = set.indexOf element
	unless index isnt -1
		logProcRet 'HEADS', false
		return false

	logProcRet 'HEADS', true
	return true

UNION = (setA, setB)->
	logProc 'UNION',
		setA: setA
		setB: setB

	set = Object.create(null)

	for n in setA
		set[n] = true

	for n in setB
		set[n] = true

	ret = Object.keys(set)
	logProcRet 'UNION', ret

	return ret

RELATIVE_COMPLEMENT = (setA, setB)->
	logProc 'RELATIVE_COMPLEMENT',
		setA: setA
		setB: setB

	set = Object.create(null)

	for n in setA
		set[n] = true

	for n in setB
		delete set[n]

	ret = Object.keys(set)
	logProcRet 'RELATIVE_COMPLEMENT', ret

	return ret

LRStack = undefined

topLRStack = ->
#	console.log 'LRStack', LRStack
	LRStack[LRStack.length - 1]

#single
APPLY_RULE = (R, P, args)->
	logProc 'APPLY_RULE',
		R: R
		P: P
		args: args
	
	if arguments.length >=3
		state_pa =
			pos: P
			args: args
	else
		state_pa = P

	lpIndent++

#	m = MEMO(R, P)
	m = MEMO(R, state_pa)
#	h = HEADS(P)
	h = HEADS(state_pa)

	#recall start
	if isNIL(h)
	else if isNIL(m) and NOT_ELEMENT_OF(R, UNION([h.head], h.involvedSet))
#		m = new MEMOENTRY(false, P)
		m = new MEMOENTRY(undefined, state_pa)
	else if ELEMENT_OF(R, h.evalSet)
		h.evalSet = RELATIVE_COMPLEMENT(h.evalSet, [R])
		m.ans = EVAL(rules[R].body, rules[R].args, args)
		if debugEVAL
			console.log 'EVAL-A', JSON.stringify(m.ans)
		m.pos = pos
	#recall end

	if isNIL(m)
		lr = new LR(undefined, R, undefined)
		LRStack.push lr

#		m = new MEMOENTRY(lr,P)
		m = new MEMOENTRY(lr,state_pa)
#		MEMO_STORE(R, P, m)
		MEMO_STORE(R, state_pa, m)
		ans = EVAL(rules[R].body, rules[R].args, args)
		if debugEVAL
			console.log 'EVAL-B', JSON.stringify(ans)

		LRStack.pop()

		m.pos = pos
		if isntNIL(lr.head)
			lr.seed = ans
			#LR_ANSWER start
			h = m.ans.head
			if h.rule isnt R
				ret = m.ans.seed
			else
				m.ans = m.ans.seed
				if isNIL(m.ans)
					ret = undefined
				else
#					HEADS_STORE(P, h)#line A
					HEADS_STORE(state_pa, h)#line A
					#GROW_LR start
					loop
#						pos = P
						pos = P
						h.evalSet = deep_copy(h.involvedSet)#line B
						ans = EVAL(rules[R].body, rules[R].args, args)
						if debugEVAL
							console.log 'EVAL-C', JSON.stringify(ans)

						if isNIL(ans) or (pos <= m.pos)
							break
						m.ans = ans
						m.pos = pos
#					HEADS_STORE(P, undefined) #line C
					HEADS_STORE(state_pa, undefined) #line C
					pos = m.pos
					ret = m.ans
					#GROW_LR end
			#LR_ANSWER end

			lpIndent--
			logProcRet 'APPLY_RULE', ret
			return ret
		else
			m.ans = ans
			ret = ans
			lpIndent--
			logProcRet 'APPLY_RULE', ret
			return ret
	else
		pos = m.pos
		#if m.ans is LR
		if m.ans?.type is 'LR'
			#SETUP_LR(R, m.ans) start
			if isNIL(m.ans.head)
				m.ans.head = new HEAD(R, [], [])

			s = topLRStack()

			while s.head isnt m.ans.head
				s.head = m.ans.head
				m.ans.head.involvedSet = UNION(m.ans.head.involvedSet, [s.rule])
				LRStack.pop()
				s = topLRStack()

			#SETUP_LR(R, m.ans) end

			ret = m.ans.seed
			lpIndent--
			logProcRet 'APPLY_RULE', ret
			return ret
		else
			ret = m.ans
			lpIndent--
			logProcRet 'APPLY_RULE', ret
			return ret

topParse = (_rules, _rule, _src, expected)->
	rules = _rules
	memo_dict = Object.create(null)
	LRStack = []
	heads_array = []
	pos = 0
	if typeof _src is 'string'
		src = new Buffer _src
	else
		src = _src
	ret = APPLY_RULE _rule, pos
	
	retStr = JSON.stringify(ret)
	retConvStr = JSON.stringify(convert_cache(ret))
	if arguments.length > 3
		if retStr is JSON.stringify(expected)
			if retStr.length < 100
				console.log 'topParse   valid', _rule, retStr
			else
				console.log 'topParse   valid', _rule

		else if retConvStr is JSON.stringify(expected)
			if retConvStr.length < 100
				console.log 'topParse   valid', _rule, retConvStr
			else
				console.log 'topParse   valid', _rule
		else
#			console.log 'expected', expected
#			throw topParse_error
			console.log 'topParse invalid', _rule, retStr, 'expected', JSON.stringify(expected)
	else
#		console.log 'topParse', _rule, util.inspect ret, depth: null, colors: true
		if retStr.length < 100
			console.log 'topParse        ', _rule, retStr
		else
			console.log 'topParse        ', _rule

	return ret

topParseConv = (_rules, _rule, _src, expected)->
	if arguments.length > 3
		convert_cache(topParse(_rules, _rule, _src, expected))
	else
		convert_cache(topParse(_rules, _rule, _src))

DebugLogProc = false

#gnu_make_converted = convert_cache(require('./seu.coffee').unserial_eu('./../Euphoria-Version/gnu_make.cache'))
#fs.writeFileSync 'gnu_make_converted.json', JSON.stringify gnu_make_converted, null, '  '
#gnu_make_rules = make_rules(gnu_make_converted)
#fs.writeFileSync 'gnu_make_rules.json', JSON.stringify gnu_make_rules, null, '  '

emem_part_converted = convert_cache(require('./seu.coffee').unserial_eu('./../Euphoria-Version/emem_part.cache'))
fs.writeFileSync 'emem_part_converted.json', JSON.stringify emem_part_converted, null, '  '
emem_part_rules = make_rules(emem_part_converted)
fs.writeFileSync 'emem_part_rules.json', JSON.stringify emem_part_rules, null, '  '

default_parser_converted = convert_cache([require('./seu.coffee').unserial_eu('./../Euphoria-Version/default_parser.ser')])
fs.writeFileSync 'default_parser_converted.json', JSON.stringify default_parser_converted, null, '  '
default_parser_rules = make_rules(default_parser_converted)
fs.writeFileSync 'default_parser_rules.json', JSON.stringify default_parser_rules, null, '  '

if 0
	#good
	if 0 then topParse gnu_make_rules, 'num', '123', '123'
	
	#good
	if 0 then topParse gnu_make_rules, 'exprA', '123-456-789', [['123','-','456'],'-','789']
	
	#good (only 123 will match)
	if 0 then topParse gnu_make_rules, 'exprB', '123-456-789', '123'
	
	#good
	if 0 then topParse gnu_make_rules, 'exprC', '123-456-789', ['123','-',['456','789']]
	
	#good
	if 0 then topParse gnu_make_rules, 'exprD', '123-456-789', [['123','-','456'],'-','789']
	
	#good
	if 0 then topParse gnu_make_rules, 'exprE', '123+456', ['123','+','456']
	
	#good
	if 0 then topParse gnu_make_rules, 'exprE', '123-456', ['123','-','456']
	
	if 0
		DebugLogProc = true
		debugEVAL = true
		debugHost2 = true
		#topParse gnu_make_rules, 'file_symbol', 'edit.c'
		#topParse gnu_make_rules, 'file_symbols', 'edit.c'
		topParse gnu_make_rules, 'filename', 'edit.c'
		

	#good
	if 0 then topParse gnu_make_rules, 'term',  '123+456*274', ['123','+',['456','*','274']]
	
	if 0
		if 0
			DebugLogProc = true
			debugEVAL = true
			debugHost2 = true
		ret = topParseConv gnu_make_rules, 'gnu_make_main', fs.readFileSync('./../preFiles/makefile-004.txt', 'utf8')
		fs.writeFileSync('result-004.json', JSON.stringify(ret, null, '  '))
		console.log util.inspect ret, depth: null, colors: true

if 0
	#DebugLogProc = true
	debugEVAL = true
	ret = topParse emem_part_rules, 'ochar', '<super test>'
	fs.writeFileSync('emem_part_ochar.json', JSON.stringify(ret, null, '  '))
	console.log util.inspect ret, depth: null, colors: true

if 0
	DebugLogProc = false
	debugEVAL = false
	
	if 0 #good
		ret = topParse emem_part_rules, 'parser', """ochar      = '<'            <ws>               <eid>:id <ws> '>' $({       "producer",     id})"""
		console.log util.inspect ret, depth: null, colors: true

	if 0 #good
		DebugLogProc = true
		debugEVAL = true
		debugHost2 = true
		#good
		#ret = topParse emem_part_rules, 'equ', '"super"'

		#good
		#ret = topParse emem_part_rules, 'equl', ',"super"'

		#good
		#ret = topParse emem_part_rules, 'mequl', ',"super"'

		#good
		#ret = topParse emem_part_rules, 'mequl', ',"super","super"'

		#good
		#ret = topParse emem_part_rules, 'mequl', ''

		#good
		#ret = topParse emem_part_rules, 'equ_list', '"super","super"'

		#good
		#ret = topParse emem_part_rules, 'qs', '"super"'

		#good
		#ret = topParse emem_part_rules, 'ldig', '"super"'

		#good
		#ret = topParse emem_part_rules, 'ldigs', '"super"'

		#good
		#ret = topParse emem_part_rules, 'call_ret', '"super"'

		#good
		#ret = topParse emem_part_rules, 'call_ret', '0'

		#good
		#ret = topParse emem_part_rules, 'num', '0'

		#good
		#ret = topParse emem_part_rules, 'int', '0'

		#good
		#ret = topParse emem_part_rules, 'ldigs', '0'
		
		#good
		#ret = topParse emem_part_rules, 'ldig', '0'

		#good
		#ret = topParse emem_part_rules, 'equ_list', '"super"'

		#good
		#ret = topParse emem_part_rules, 'mchar2', '"super"'
		console.log util.inspect ret, depth: null, colors: true

	if 0 #good
		DebugLogProc = true
		debugEVAL = false
#		ret = topParse emem_part_rules, 'parser', """ochar      = '<' <tk("super")>                 <eid>:id <ws> '>' $({  "superproducer", {   id ,{}}})"""
		ret = topParse emem_part_rules, 'parser', """ochar      = '<' <tk("super")>"""
		console.log util.inspect ret, depth: null, colors: true

	if 0 #good
		ret = topParse emem_part_rules, 'parser', """ochar      = '<' <tk("foreign")> <eid>:p  <ws> <eid>:id                                        <ws> '>' $({"foreignproducer", {{p,id},{}}})"""
		console.log util.inspect ret, depth: null, colors: true

	if 0 #good
		ret = topParse emem_part_rules, 'parser', """ochar      = '<'            <ws>               <eid>:id <ws> '(' <ws> <equ_list>:args <ws> ')' <ws> '>' $({       "producer2",{   id ,args}})"""
		console.log util.inspect ret, depth: null, colors: true

	if 0 #good
		ret = topParse emem_part_rules, 'parser', """ochar      = '<' <tk("super")>                 <eid>:id <ws> '(' <ws> <equ_list>:args <ws> ')' <ws> '>' $({  "superproducer", {   id ,args}})"""
		console.log util.inspect ret, depth: null, colors: true

	if 0 #bad
		ret = topParse emem_part_rules, 'parser', """ochar      = '<' <tk("foreign")> <eid>:p  <ws> <eid>:id <ws> '(' <ws> <equ_list>:args <ws> ')' <ws> '>' $({"foreignproducer", {{p,id},args}})"""
		console.log util.inspect ret, depth: null, colors: true

	if 0 #host2 missing, bad number range
		ret = topParse emem_part_rules, 'parser', """ochar      = '<_>'                                                         $({"ranges",{{0,255}}})"""
		console.log util.inspect ret, depth: null, colors: true

if 1
	DebugLogProc = false
	debugEVAL = false
	debugHost2 = false
#	ret = topParse emem_part_rules, 'eat', fs.readFileSync('./../Euphoria-Version/emem_part.e', 'utf8')
	ret = topParse emem_part_rules, 'eat', fs.readFileSync('./../Euphoria-Version/emem_part.e'), emem_part_converted[0]
#	fs.writeFileSync('emem_part.json', JSON.stringify([ret,[]], null, '  '))
#	fs.writeFileSync('emem_part_conv.json', JSON.stringify(convert_cache([ret,[]]), null, '  '))

#	gnu_make_conv = ["gnu_make", [], gnu_make_converted[0]]
#	fs.writeFileSync('gnu_make_convA.json', JSON.stringify(gnu_make_conv, null, '  '))

	ret = topParse emem_part_rules, 'named_p', fs.readFileSync('./../Ometa-Javascript/gnu_make.ometajs')#, gnu_make_conv
	fs.writeFileSync('gnu_make_conv.json', JSON.stringify(convert_cache([ret[2], [ret]]), null, '  '))

	makefileSources = [
		'004'
		'008'
		'009'
		'010'
		'011'
		'012'
		'013'
		'014'
		'016'
		'017'
		'019'
	]

	gnu_make_rules = make_rules(convert_cache([ret[2], [ret]]))
	fs.writeFileSync 'gnu_make_rules.json', JSON.stringify gnu_make_rules, null, '  '

	for mn in makefileSources
		ret = topParseConv gnu_make_rules, 'gnu_make_main', fs.readFileSync('./../preFiles/makefile-' + mn + '.txt', 'utf8')
		fs.writeFileSync('result-' + mn + '.txt', fs.readFileSync('./../preFiles/makefile-' + mn + '.txt', 'utf8'))
		fs.writeFileSync('result-' + mn + '.json', JSON.stringify(ret, null, '  '))
		#console.log util.inspect ret, depth: null, colors: true


#	console.log util.inspect ret, depth: null, colors: true
