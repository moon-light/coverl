COVERL_DIR ?= $(CURDIR)
COVERL_EUNIT_DATA ?= $(COVERL_DIR)/ct.coverl.data
COVERL_CT_DATA ?= $(COVERL_DIR)/eunit.coverl.data
COVERL_REPORT ?= $(COVERL_DIR)/lcov.info

help::
	$(verbose) printf "%s\n" "" \
		"Coverl:" \
		"If COVERL=1 is set, coverage data is collected by the targets eunit and ct."

ifdef COVERL
define eunit.erl
	{ok, _} = application:ensure_all_started(coverl),
	case [{Module, Error}
		|| {Module, Error} <- coverl:compile_beam_directory("ebin"),
			Error =/= ok]
	of [] -> ok
	; Error ->
		io:format("coverl_compile:~n~p~n", [Error]),
		halt(1)
	end,
	case eunit:test($1, [$(EUNIT_OPTS)])
	of ok -> ok
	; error -> halt(2)
	end,
	coverl:export("$(COVERL_DIR)/eunit.coverl.data"),
	halt()
endef

CT_OPTS += -ct_hooks coverl_ct_hook \
'[{cover, {application, $(PROJECT)}}, {export,"$(abspath $(COVERL_DIR)/ct.coverl.data)"}]'

endif

COVERL_ERL_OPTS += -pa $(DEPS_DIR)/*/ebin $(APPS_DIR)/*/ebin $(CURDIR)/ebin

define coverl_report.erl
	{ok, _} = application:ensure_all_started(coverl),
	ok = coverl:import("$(COVERL_EUNIT_DATA)"),
	ok = coverl:import("$(COVERL_CT_DATA)"),
	ok = coverl:report(#{out => "$(COVERL_REPORT)"}),
	halt()
endef

.PHONY: coverl_data-distclean coverl_report-clean coverl_dir-distclean

coverl-report:
	$(gen_verbose) $(call erlang,$(coverl_report.erl),$(COVERL_ERL_OPTS))

clean:: coverl_report-clean

coverl_report-clean:
	$(gen_verbose) rm -rf $(COVERL_LINE_REPORT) $(COVERL_CD_REPORT)

distclean:: coverl_data-distclean

coverl_data-distclean:
	$(gen_verbose) rm -rf $(COVERL_EUNIT_DATA) $(COVERL_CT_DATA)

ifneq ($(COVERL_DIR),$(CURDIR))
distclean:: coverl_dir-distclean
endif

coverl_dir-distclean:
	$(gen_verbose) rm -rf $(COVERL_DIR)
