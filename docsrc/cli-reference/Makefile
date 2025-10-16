# Convert the output of '--help' for the various subcommands into HTML pages
.PHONY: build
build: help.html run-help.html status-help.html approve-help.html

# Convert ansiterm data to HTML, remove junk sequences by
# removing all non-ascii characters (may remove too much).
# A proper solution would be to fix cmdliner's pager output.
%.html: ../tutorial/my_project/%.term
	# ansi2html is installed on Ubuntu as part of the 'colorized-logs'
	# package. Run 'sudo apt install colorized-logs' if ansi2html
	# is missing.
	# Not using a pipe (|) to fail if the first command fails.
	ansi2html < $< > $@
	LANG=iso-8859-1 sed -i -e 's/[\d128-\d255]//g' $@
