################################################################################
NAME = eldoro
include elpkg/elpkg.mk

################################################################################
all: README.md

################################################################################
README.md: $(CORE_FILE) INSTALL.md
	echo '# Eldoro: '`$(ELPKG)/desc.sh $(CORE_FILE)`"\n" > $@
	$(ELPKG)/fdoc2md.sh eldoro-mode $(CORE_FILE) >> $@
