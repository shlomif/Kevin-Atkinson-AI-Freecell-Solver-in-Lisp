;; Copyright (C) 2001 by Kevin Atkinson <kevin_fc@atkinson.dhs.org>

(declaim (optimize (space 1) (speed 3) (compilation-speed 0)
		   (safety 0) (debug 0)))

(load "gc-parms.lsp" :print nil :verbose nil)

(load "util.lsp" :print nil :verbose nil)

(compile-file '("util.lsp" "card.lsp" "game.lsp" "search.lsp" "gc-parms.lsp")
	      :print nil
	      :verbose nil
	      :block-compile t
	      :output-file "all.x86f"
	      :load t)
