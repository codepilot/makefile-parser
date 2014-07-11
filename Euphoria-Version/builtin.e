include dll.e
without warning
constant
	M_OPEN_DLL  = 50,
	M_DEFINE_C  = 51,
	M_CALL_BACK = 52,
	M_FREE_CONSOLE = 54,
	M_DEFINE_VAR = 56

	
-- M_COMPLETE      =0,  -- determine Complete Edition
-- M_SOUND         =1,
-- M_LINE          =2,
-- M_PALETTE       =3, 
-- M_PIXEL         =4, -- obsolete, but keep for now
-- M_GRAPHICS_MODE =5,
-- M_CURSOR        =6,
-- M_WRAP          =7,
-- M_SCROLL        =8,
-- M_SET_T_COLOR   =9,
-- M_SET_B_COLOR  =10,
-- M_POLYGON      =11,
-- M_TEXTROWS     =12,
-- M_VIDEO_CONFIG =13,
-- M_GET_MOUSE    =14,
-- M_MOUSE_EVENTS =15,
-- M_ALLOC        =16,
-- M_FREE         =17,
-- M_ELLIPSE      =18,
-- M_SEEK         =19,
-- M_WHERE        =20,
-- M_GET_PIXEL    =21, -- obsolete, but keep for now
-- M_DIR          =22,
-- M_CURRENT_DIR  =23,
-- M_MOUSE_POINTER =24,
-- M_GET_POSITION =25,
-- M_WAIT_KEY     =26,
-- M_ALL_PALETTE  =27,
-- M_GET_DISPLAY_PAGE =28,
-- M_SET_DISPLAY_PAGE =29,
-- M_GET_ACTIVE_PAGE =30,
-- M_SET_ACTIVE_PAGE =31,
-- M_ALLOC_LOW    =32,
-- M_FREE_LOW     =33,
-- M_INTERRUPT    =34,
-- M_SET_RAND     =35,
-- M_USE_VESA     =36,
-- M_CRASH_MESSAGE =37,
-- M_TICK_RATE    =38,
-- M_GET_VECTOR   =39,
-- M_SET_VECTOR   =40,
-- M_LOCK_MEMORY  =41,
-- M_ALLOW_BREAK  =42,
-- M_CHECK_BREAK  =43,
-- M_MEM_COPY     =44, -- obsolete, but keep for now
-- M_MEM_SET      =45, -- obsolete, but keep for now
-- M_A_TO_F64     =46,
-- M_F64_TO_A     =47,
-- M_A_TO_F32     =48,
-- M_F32_TO_A     =49,
-- M_OPEN_DLL     =50,
-- M_DEFINE_C     =51,
-- M_CALLBACK     =52,
-- M_PLATFORM     =53, -- obsolete, but keep for now
-- M_FREE_CONSOLE =54,
-- M_INSTANCE     =55,
-- M_DEFINE_VAR   =56,
-- M_CRASH_FILE   =57,
-- M_GET_SCREEN_CHAR =58,
-- M_PUT_SCREEN_CHAR =59,
-- M_FLUSH        =60,
-- M_LOCK_FILE    =61,
-- M_UNLOCK_FILE  =62,
-- M_CHDIR        =63,
-- M_SLEEP        =64,
-- M_BACKEND      =65,
-- M_CRASH_ROUTINE =66


atom depth
depth=0
function test2(atom ptr, atom nargs, atom rid)
atom ret
-- 	trace(3)
-- 	puts(1,repeat(32,depth*2)&"test2 ")
-- 	? {ptr,nargs,rid,task_self()}
-- 	puts(1,repeat(32,depth*2)&"args ")
-- 	? peek4u({ptr,nargs})
-- 	depth+=1
	ret=call_func(h2_func_rid,{{"funclit","call_func",{rid,peek4u({ptr,nargs})}},{},{}})
-- 	depth-=1
-- 	puts(1,repeat(32,depth*2)&"ret ") ? ret puts(1,10)
	return ret
end function
constant test2_cb=call_back(routine_id("test2"))

--      3 00000000 55                     push ebp
--      4 00000001 89E5                   mov ebp, esp
--     17 0000001A 89EC                   mov esp,ebp
--     18 0000001C 5D                     pop ebp

function stdcall_func(atom rid, atom numargs)
	return allocate_string({
	#55,                     --push ebp
	#89,#E5,                   --mov ebp, esp
	#89,#E0,                        --mov eax, esp
	#83,#C0,#08,                    --add eax, 8
	#68}&int_to_bytes(rid)&{        --push dword 0x01234555
	#68}&int_to_bytes(numargs)&{          --push dword 0x01234555
	#50,                            --push eax
	#B8}&int_to_bytes(test2_cb)&{   --mov eax, 0x01234567
	#FF,#D0,                        --call dword eax
	#89,#EC,                   --mov esp,ebp
	#5D,                     --pop ebp
-- 	#C3                             --ret
	#C2}&numargs*4&{#00               --ret 5
})
end function

function cdecl_func(atom rid, atom numargs)
	return allocate_string({
	#89,#E1,                        --mov ecx, esp
	#68}&int_to_bytes(rid)&{        --push dword 0x01234555
	#68}&int_to_bytes(5)&{          --push dword 0x01234555
	#83,#C1,#04,                    --add ecx, 4
	#51,                            --push ecx
	#B8}&int_to_bytes(test2_cb)&{   --mov eax, 0x01234567
	#FF,#D0,                        --call dword eax
	#C3                             --ret
})
end function


global constant builtin_procs = {"?","puts","position","print","printf","clear_screen","close","trace","system","machine_proc","abort","poke","call","pixel","mem_copy","mem_set","c_proc","call_proc","poke4","profile","task_schedule","task_yield","task_suspend","task_clock_stop","task_clock_start"}
global constant builtin_funcs = {"length","integer","sequence","object","append","prepend","floor","getc","gets","get_key","rand","repeat","atom","compare","find","match","time","command_line","open","getenv","sqrt","sin","cos","tan","log","date","remainder","power","machine_func","peek","sprintf","arctan","and_bits","or_bits","xor_bits","not_bits","get_pixel","c_func","routine_id","call_func","peek4s","peek4u","equal","system_exec","platform","task_create","task_self","task_list","task_status","find_from","match_from"}

sequence c_funcs,c_func_ids
c_funcs={}
c_func_ids={}

global procedure builtin_proc(sequence name, sequence args)
--internal program controll pure
	if equal(name,"abort") then abort(args[1]) return end if
	if equal(name,"call_proc") then call_proc(args[1],args[2]) return end if
--internal debug(can do without)
	if equal(name,"trace") then trace(args[1]) return end if
	if equal(name,"profile") then profile(args[1]) return end if
--internal task(later work)
	if equal(name,"task_schedule") then task_schedule(args[1],args[2]) return end if
	if equal(name,"task_yield") then task_yield() return end if
	if equal(name,"task_suspend") then task_suspend(args[1]) return end if
	if equal(name,"task_clock_stop") then task_clock_stop() return end if
	if equal(name,"task_clock_start") then task_clock_start() return end if
--external file io(except for stdin/out/err external)
	if equal(name,"?") then ? args[1] return end if
	if equal(name,"puts") then puts(args[1],args[2]) return end if
	if equal(name,"position") then return position(args[1],args[2]) return end if
	if equal(name,"print") then print(args[1],args[2]) return end if
	if equal(name,"printf") then printf(args[1],args[2],args[3]) return end if
	if equal(name,"clear_screen") then return clear_screen() return end if
	if equal(name,"close") then close(args[1]) return end if
--external memory
	if equal(name,"poke") then poke(args[1],args[2]) return end if
	if equal(name,"call") then call(args[1]) return end if
	if equal(name,"mem_copy") then mem_copy(args[1],args[2],args[3]) return end if
	if equal(name,"mem_set") then mem_set(args[1],args[2],args[3]) return end if
	if equal(name,"c_proc") then c_proc(args[1],args[2]) return end if
	if equal(name,"poke4") then poke4(args[1],args[2]) return end if
--internal/external misc(mostly external)
	if equal(name,"machine_proc") then machine_proc(args[1],args[2]) return end if
--external graphics
	if equal(name,"pixel") then pixel(args[1],args[2]) return end if
--external shell
	if equal(name,"system") then system(args[1],args[2]) return end if
end procedure

global function builtin_func(sequence name, sequence args)
object ret
--internal pure
--type check
	if equal(name,"integer") then return integer(args[1]) end if
	if equal(name,"sequence") then return sequence(args[1]) end if
	if equal(name,"object") then return object(args[1]) end if
	if equal(name,"atom") then return atom(args[1]) end if
--seq
	if equal(name,"length") then if not sequence(args[1]) then return -1 end if return length(args[1]) end if
	if equal(name,"append") then return append(args[1],args[2]) end if
	if equal(name,"prepend") then return prepend(args[1],args[2]) end if
	if equal(name,"repeat") then return repeat(args[1],args[2]) end if
	if equal(name,"find") then return find(args[1],args[2]) end if
	if equal(name,"match") then return match(args[1],args[2]) end if
	if equal(name,"sprintf") then return sprintf(args[1],args[2]) end if
	if equal(name,"find_from") then return find_from(args[1],args[2],args[3]) end if
	if equal(name,"match_from") then return match_from(args[1],args[2],args[3]) end if
--math
	if equal(name,"floor") then return floor(args[1]) end if
	if equal(name,"compare") then return compare(args[1],args[2]) end if
	if equal(name,"sqrt") then return sqrt(args[1]) end if
	if equal(name,"sin") then return sin(args[1]) end if
	if equal(name,"cos") then return cos(args[1]) end if
	if equal(name,"tan") then return tan(args[1]) end if
	if equal(name,"log") then return log(args[1]) end if
	if equal(name,"remainder") then return remainder(args[1],args[2]) end if
	if equal(name,"power") then return power(args[1],args[2]) end if
	if equal(name,"arctan") then return arctan(args[1]) end if
	if equal(name,"and_bits") then return and_bits(args[1],args[2]) end if
	if equal(name,"or_bits") then return or_bits(args[1],args[2]) end if
	if equal(name,"xor_bits") then return xor_bits(args[1],args[2]) end if
	if equal(name,"not_bits") then return not_bits(args[1]) end if
	if equal(name,"equal") then return equal(args[1],args[2]) end if
--internal dirty
	if equal(name,"rand") then return rand(args[1]) end if
--semi internal
	if equal(name,"routine_id") then return routine_id(args[1]) end if
	if equal(name,"call_func") then return call_func(args[1],args[2]) end if
	if equal(name,"platform") then return platform() end if
	if equal(name,"task_create") then return task_create(args[1],args[2]) end if
	if equal(name,"task_self") then return task_self() end if
	if equal(name,"task_list") then return task_list() end if
	if equal(name,"task_status") then return task_status(args[1]) end if
--semi idem
	if equal(name,"time") then return time() end if
	if equal(name,"date") then return date() end if
--external memory
	if equal(name,"peek") then return peek(args[1]) end if
	if equal(name,"c_func") then
--  		printf(1,repeat(32,depth*2)&"%s %d \n",{c_funcs[find(args[1],c_func_ids)][2],task_self()})
--  		? args[2]
--  		if equal({c_funcs[find(args[1],c_func_ids)][2],{0,0,0,0}},{"GetMessageA",args[2]}) then
--  			return 1/0
--  		end if
		return c_func(args[1],args[2])
	end if
	if equal(name,"peek4s") then return peek4s(args[1]) end if
	if equal(name,"peek4u") then return peek4u(args[1]) end if
--external graphics
	if equal(name,"get_pixel") then return get_pixel(args[1]) end if
--external file
	if equal(name,"open") then return open(args[1],args[2]) end if
	if equal(name,"getc") then return getc(args[1]) end if
	if equal(name,"gets") then return gets(args[1]) end if
--external keyboard
	if equal(name,"get_key") then return get_key() end if
--external
	if equal(name,"command_line") then return command_line() end if
	if equal(name,"getenv") then return getenv(args[1]) end if
--external shell
	if equal(name,"system_exec") then return system_exec(args[1],args[2]) end if
	
	if equal(name,"machine_func") then
		if M_CALL_BACK=args[1] then
			puts(1,"M_CALL_BACK ") ? length(efuncs[args[2]][1])
			ret=stdcall_func(args[2],length(efuncs[args[2]][1]))
			printf(1,"#%08x\n",ret)
			return ret
		else
			ret=machine_func(args[1],args[2])
			if equal(args[1],M_DEFINE_C) then
				c_funcs=append(c_funcs,args[2])
				c_func_ids=append(c_func_ids,ret)
			end if
			return ret
		end if
	end if
end function


-- get, set, setting math -=, +=, *=, /=, &=, subscript, slice
-- math, cmp, log
-- returnp, returnf
-- if/elsif/else
-- while
-- for
-- proc/func
-- exit
