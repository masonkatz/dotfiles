#!/bin/zsh

export VISUAL=emacsclient
export EDITOR=emacsclient
export ALTERNATE_EDITOR="emacs -nw"	# emacs client uses this

alias e="emacsclient -c"

function restart-emacs() {
	if emacsclient -e '(progn (message "Killing Emacs...") (kill-emacs))' >/dev/null 2>&1; then
		echo "Stopped Emacs daemon."
	else
		echo "Emacs daemon was not running."
	fi

	echo "Starting Emacs daemon..."
	"emacs" --daemon
}


