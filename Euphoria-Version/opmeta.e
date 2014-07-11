
global function optimize_parser(sequence r, sequence p)
sequence tag,ins
	for i = 1 to length(p) do
		ins=p[i]
		tag=ins[eo_tag]
		if find(tag,{"producer","sproducer"}) then
			p[i][eo_tag]="nproducer"
			p[i][eo_arg]=find(p[i][eo_arg],r)
			if not p[i][eo_arg] then ? 1/0 end if
		elsif find(tag,{"producer2"}) then
			p[i][eo_tag]="nproducer2"
			p[i][eo_arg][1]=find(p[i][eo_arg][1],r)
			if not p[i][eo_arg][1] then ? 1/0 end if
		elsif find(tag,{"char","eqval","ranges","host2","query","dofirst","doopen","doclose"}) then
		elsif find(tag,{"choice","*","+","!","dolist"}) then
			p[i][eo_arg]=optimize_parser(r,p[i][eo_arg])
		else
			? 1/0
		end if
		p[i][eo_tag]=find(p[i][eo_tag],Rrules)
	end for
	return p
end function

global function optimize_parsers(sequence p)
sequence r
r=repeat(0,length(p))
	for i = 1 to length(p) do
		r[i]=p[i][1]
	end for
	for i = 1 to length(p) do
		p[i][2]=optimize_parser(r,p[i][2])
	end for
	return p
end function