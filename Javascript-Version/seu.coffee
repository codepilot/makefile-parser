exports.iuseu = iuseu = (b, offset={location: 0}) ->
	nType = String.fromCharCode b[offset.location++]
	atom = b.readDoubleLE offset.location
	offset.location += 8
	switch nType
		when 'a' then atom
		when 's' then iuseu b, offset for i in [0...atom]

exports.unserial_eu = unserial_eu = (fn) -> iuseu require('fs').readFileSync(fn)
