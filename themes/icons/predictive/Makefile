
# Copyright (C) 2004-2009 Toby Cubitt

# Author: Toby Cubitt <toby-predictive@dr-qubit.org>
# Version: 0.6.1
# URL: http://www.dr-qubit.org/emacs.php

# This file is part of the Emacs Predictive Completion package.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA.


EMACS = emacs
DESTDIR = $(HOME)/.emacs.d/predictive
DICTDIR = $(HOME)/.emacs.d/predictive
INFODIR = /usr/share/info


all: core dict-english dicts info


docs: info pdf dvi txt ps html


.PHONY: clean
clean:
	find . -name '*.elc' -delete
	[ ! -e dict-english.el ] || rm dict-english.el
	[ ! -e predictive-user-manual.info.gz ] || rm predictive-user-manual.info.gz
	[ ! -e predictive-user-manual.pdf ] || rm predictive-user-manual.pdf
	[ ! -e predictive-user-manual.dvi ] || rm predictive-user-manual.dvi
	[ ! -e predictive-user-manual.ps.gz ] || rm predictive-user-manual.ps.gz
	[ ! -e predictive-user-manual.txt.gz ] || rm predictive-user-manual.txt.gz
	[ ! -d predictive-user-manual-html ] || rm -r predictive-user-manual-html/


.PHONY : install
install: all
	mkdir -p $(DESTDIR)
	find . \( -name '*.el' -o -name '*.elc' \) -a \( -name dict-tree.el -o -name dict-tree.elc -o \! -name 'dict-*' \) -execdir cp {} $(DESTDIR) \;
	mkdir -p $(DICTDIR)
	cp dict-english.el dict-english.elc $(DICTDIR)
	for d in `find . -mindepth 1 -type d -exec mkdir -p $(DICTDIR)/{} \; -print`; do cp $$d/dict-*.el $$d/dict-*.elc $(DICTDIR)/$$d; done

	@echo
	@echo "To complete the installation, add the following lines to your .emacs file:"
	@echo
	@echo "  (add-to-list 'load-path \"$(DESTDIR)\")"
	@[ "$(DICTDIR)" = "$(DESTDIR)" ] || echo "  (add-to-list 'load-path \"$(DICTDIR)\")"
	@for d in `find . -mindepth 1 -type d`; do echo "  (add-to-list 'load-path \"$(DICTDIR)/$${d#./}\")"; done
	@echo "  (require 'predictive)"
	@echo




# list of core elisp files
core_files := $(shell ls *.el | grep -v 'dict-english.el' | sed 's:\.el:\.elc:g')

# lists of dictionaries
latex_dicts := $(shell ls latex/dict-*.word-list | sed 's:\.word-list:\.elc:g')
html_dicts := $(shell ls html/dict-*.word-list | sed 's:\.word-list:\.elc:g')
texinfo_dicts := $(shell ls texinfo/dict-*.word-list | sed 's:\.word-list:\.elc:g')
#f90_dicts := $(shell ls f90/dict-*.word-list | sed 's:\.word-list:\.elc:g')




# byte-compilation target
core: $(core_files)

# overrides implicit rules, since these require the dictionaries
predictive-latex.elc: predictive-latex.el $(latex-dicts)
	$(EMACS) --batch -L ./ -L ./latex/ -f batch-byte-compile $<
predictive-html.elc: predictive-html.el $(html-dicts)
	$(EMACS) --batch -L ./ -L ./html/ -f batch-byte-compile $<



# English dictionary target
dict-english: dict-english.elc

# overrides implicit rule for dictionaries, to create it from the .el file
dict-english.elc: dict-english.el #dict-tree.elc
	$(EMACS) --batch -L ./ --eval="(progn (setq byte-compile-disable-print-circle t) (byte-compile-file \"$<\"))"

# in case dict-english.el doesn't exist (should be included in package)
dict-english.el: dict-english.word-list
	$(EMACS) --batch -L ./ --eval="(progn (require 'predictive) (setq dict-english (predictive-create-dict '$(basename $(notdir $@)) \"$(basename $@)\" \"$<\" nil nil t)) (dictree-write dict-english \"dict-english\" t))"




# dictionary targets
dicts: dict-english latex_dicts html_dicts texinfo_dicts # f90_dicts

latex_dicts: $(latex_dicts)

html_dicts: $(html_dicts)

texinfo_dicts: $(texinfo_dicts)

#f90_dicts: $(f90_dicts)




# documentation targets
info: predictive-user-manual.info.gz

pdf: predictive-user-manual.pdf

dvi: predictive-user-manual.dvi

ps: predictive-user-manual.ps.gz

txt: predictive-user-manual.txt.gz

html: predictive-user-manual-html


# info file installation target
install-info: predictive-user-manual.info.gz
	install-info predictive-user-manual.info.gz $(INFODIR)/dir




# implicit rule for creating dictionaries
dict-%.elc: dict-%.word-list #dict-tree.elc
	$(EMACS) --batch -L ./ --eval="(progn (require 'predictive) (predictive-create-dict '$(basename $(notdir $@)) \"$(basename $@)\" \"$<\" nil nil t) (dictree-save-modified))"


# implicit rule for byte-compiling elisp files
%.elc: %.el
	$(EMACS) --batch -L ./ -f batch-byte-compile $<


# implicit rules for making docs in various formats
%.info: %.texinfo
	makeinfo $< -o $@

%.info.gz: %.info
	gzip -f $< -c > $@

%.txt: %.texinfo
	makeinfo --plaintext $< > $@

%.txt.gz: %.texinfo
	makeinfo --plaintext $< | gzip -c > $@

%.dvi: %.texinfo
	texi2dvi -c -o $@ $<

%.pdf: %.texinfo
	texi2dvi --pdf -c -o $@ $<

%.ps.gz: %.dvi
	dvips -f $< | gzip -c > $@

%-html: %.texinfo
	makeinfo --html $< -o $(dir $@)/html

%-html.tar.gz: %-html
	cd $(dir $@); pwd; tar -cvzf $(notdir $@) predictive-user-manual-html
