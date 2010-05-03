LIBDIR					= `erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION					= 0.1
CC							= erlc
ERL							= erl
EBIN						= ebin
CFLAGS					= -I include -pa $(EBIN)
COMPILE					= $(CC) $(CFLAGS) -o $(EBIN)
EBIN_DIRS				= $(wildcard deps/*/ebin)
ERLC_FLAGS      = -W -pa ./ebin

all: ebin compile

compile:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

edoc:
	@echo Generating $(APP) documentation from srcs
	@$(ERL) -noinput -eval 'edoc:application($(APP), "./", [{doc, "doc/"}, {files, "src/"}])' -s erlang halt

ebin:
	@mkdir ebin


test: all $(TEST_OBJ)
	$(SILENCE) $(ERL)	$(ERLC_FLAGS) \
				-pa ./test/ebin/ \
				-noshell 	\
				-sname local_test 	\
				-s test_suite test 	\
				-s init stop

clean:
	rm -rf ebin/*.beam ebin/erl_crash.dump erl_crash.dump ebin/*.boot ebin/*.rel ebin/*.script