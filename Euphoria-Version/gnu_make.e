gnu_make {
--need parse for 9
--     objects = main.o kbd.o command.o display.o \
--               insert.o search.o files.o utils.o
--
--need parse for 13
--     clean:
--             rm edit $(objects)
--

--need parse for 14
--     .PHONY : clean
--


--need parse for 19
--     foo:
--             frobnicate > foo
--
--     %: force
--             @$(MAKE) -f Makefile $@
--     force: ;
--

--need to look at the rest





  white_space     = '\\' <eol>                                                                                      $({})
                  | ' '
  white_space_eol = <white_space>* <eol>                                                                            $({})
  alphanumeric    = [a-zA-Z0-9]
  file_symbol     = '.' | '-'
  file_symbols    = <alphanumeric> | <file_symbol>
  filename        = <white_space>*:a <file_symbols>+:f <white_space>*:b                                             $({f})
  filenames       = <filename>+
  eol             = '\n'                                                                                            $({})
                  | '\r\n'                                                                                          $({})
  make_tree       = <filename>:f ':' <filenames>:s <eol> <white_space>*:a <filenames>:b  <white_space_eol>+         $({"rule", f, s, b})
                  | <filename>:f ':'               <eol> <white_space>*:a <filenames>:b  <white_space_eol>+         $({"rule", f, {}, b})
  gnu_make_main   = <make_tree>*:m                                                                                  $(m)
}