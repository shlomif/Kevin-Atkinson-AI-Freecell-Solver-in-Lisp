;; Copyright (C) 2001 by Kevin Atkinson <kevin_fc@atkinson.dhs.org>

(setf comp:*cltl1-compile-file-toplevel-compatibility-p* t)

(set-case-mode :case-insensitive-lower)

(declaim (optimize (space 0) (speed 3) (safety 0) (debug 0)))
(load "util.lsp")
(compile-file "util.lsp")
(load "util")

(load "card.lsp")
(compile-file "card.lsp")
(load "card")

(load "game.lsp")
(compile-file "game.lsp")
(load "game")

(load "search.lsp")
(compile-file "search.lsp")
(load "search")

