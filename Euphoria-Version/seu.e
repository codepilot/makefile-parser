include file.e
include machine.e

function iuseu(sequence a)
sequence ret,rval
atom len
	if a[1][a[2]]='a' then a[2]+=1 return {float64_to_atom(a[1][a[2]..a[2]+7]),a[2]+8} end if
	if a[1][a[2]]='s' then
		a[2]+=1
		len=float64_to_atom(a[1][a[2]..a[2]+7])
		a[2]+=8
		ret=repeat(0,len)
		for i = 1 to len do
			rval=iuseu(a)
			ret[i]=rval[1]
			a[2]=rval[2]
		end for
		return {ret,a[2]}
	end if
end function

global function unserial_eu(sequence a)
sequence ret
	if not length(a) then return {} end if
	ret=iuseu({a,1})
	return ret[1]
end function

global function serial_eu(object a)
sequence ret
	if atom(a) then
		ret='a'&atom_to_float64(a)
		if not equal(a,unserial_eu(ret)) then ? 1/0 end if
		return ret
	end if
	ret='s'&atom_to_float64(length(a))
	for i = 1 to length(a) do
		ret&=serial_eu(a[i])
	end for
	if not equal(a,unserial_eu(ret)) then ? 1/0 end if
	return ret
end function

global procedure serial_fileOld(sequence fn, object a)
	atom fh
	fh=open(fn,"wb")
	puts(fh,serial_eu(a))
	close(fh)
end procedure

function read_whole_file(sequence fn)
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

global function unserial_file(sequence fn)
	return unserial_eu(read_whole_file(fn))
end function


	atom fh
	
global procedure serial_eu_file(object a)
	if atom(a) then
		puts(fh,'a'&atom_to_float64(a))
		return
	end if
	puts(fh,'s'&atom_to_float64(length(a)))
	for i = 1 to length(a) do
		serial_eu_file(a[i])
	end for
end procedure

global procedure serial_file(sequence fn, object a)
	fh=open(fn,"wb")
	if fh = -1 then return end if
	serial_eu_file(a)
	close(fh)
end procedure

