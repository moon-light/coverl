PROJECT = coverl
PROJECT_DESCRIPTION = Erlang cover tool
PROJECT_VERSION = 0.1.0

LOCAL_DEPS = compiler

CT_LOGS_DIR = _info/ct

COVERL_DIR = _info/

DEP_PLUGINS = $(PROJECT)

# Hack for self coverl
ifeq ($(IS_DEP),)
ifdef COVERL
ebin/$(PROJECT).app:: override ERLC_OPTS += -DCOVERL_DATA_MODULE=coverl_data_orig
endif
endif

include erlang.mk

TEST_ERLC_OPTS += +export_all
