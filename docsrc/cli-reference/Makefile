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
	ansi2html < $< > $@
	sed -i -e 's:<head>:<head>\n<meta charset="utf-8" />:g' $@
