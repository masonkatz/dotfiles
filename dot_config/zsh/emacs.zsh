#!/bin/zsh

export VISUAL=e
export EDITOR=e

alias e='emacsclient --alternate-editor=" " -c'

function restart-emacs() {
	if emacsclient -e '(progn (message "Killing Emacs...") (kill-emacs))' >/dev/null 2>&1; then
		echo "Stopped Emacs daemon."
	else
		echo "Emacs daemon was not running."
	fi

	echo "Starting Emacs daemon..."
	"emacs" --daemon
}


