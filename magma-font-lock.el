;;; magma-font-lock.el --- Syntax highlight for magma code. ;

;; Copyright (C) 2007-2014 Luk Bettale
;; Licensed under the GNU General Public License.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:

;; Documentation available in README.org or on
;; https://github.com/ThibautVerron/magma-mode

;;; Code:

;; Useful regexps
(defconst magma-space-comments-regexp
  (concat
   "[[:space:]\n\r]*"
   "\\(\\(/\\*[^*]*\\*\\([^/][^*]*\\*\\)*/\\|//[^\n]*\\)[[:space:]\n\r]*\\)*")
  "Regular expression to find magma comments or space.")

(defconst magma-blank-comments-regexp
  (concat
   "[[:blank:]]*"
   "\\(\\(/\\*[^*\n]*\\*\\([^/][^*\n]*\\*\\)*/\\|//[^\n]*\\)[[:blank:]]*\\)*")
  "Regular expression to find magma comments or space")

(defconst magma-ident-regexp
  "\\_<\\([[:alpha:]]\\|_[[:alnum:]_]\\)[[:alnum:]_]*\\_>"
  "Regular expression of an identifier")

(defconst magma-defun-regexp
  (concat
   (regexp-opt '("function" "procedure" "intrinsic") 'words)
   "[[:space:]\n\r]*\\(" magma-ident-regexp
   "\\)?[[:space:]\n\r]*([^)]*)")
  "Regular expression of a defun")

(defconst magma-intrinsic-regexp
  (concat
   "\\<intrinsic\\>"
   "[[:space:]\n\r]+" magma-ident-regexp
   "[[:space:]\n\r]*([^)]*)"
   "[[:space:]\n\r]*->[[:space:]\n\r]*[[:alnum:]]+"
   "[[:space:]\n\r]*{[^}]*}")
  "Regular expression of an intrinsic declaration");

(defconst magma-statement-end-regexp
  (concat
   ;; "\\(;\\|:\\|}\\|" ;; Original
   ;; Change 10.11.2013 T.Verron : two following lines
   "\\(;\\|}\\|"
   (regexp-opt '("case" "when") 'words) "[^:]*:\\|"
   (regexp-opt '("repeat" "do" "try" "then" "else") 'words) "\\|"
   magma-intrinsic-regexp "\\|"
   "\\<\\(function\\|procedure\\)\\>[[:space:]\n\r]*"
   "\\(" magma-ident-regexp "\\)?[[:space:]\n\r]*([^)]*)\\|"
   "\\<catch\\>[[:blank:]]+" magma-ident-regexp "\\)"
   ;;"[[:space:]\n\r]*"
   )
  "Regular expression ending a statement")


;; Keywords in magma
(defconst magma-keywords
  '("_" "by" "default" "do" "is" "select" "then" "to" "where" "end" "until"
    "catch" "elif" "else" "when" "case" "for" "function" "if" "intrinsic"
    "procedure" "repeat" "try" "while")
  "Keywords used by magma")

(defconst magma-operators
  '("adj" "and" "cat" "cmpeq" "cmpne" "diff" "div" "eq" "ge" "gt" "in" "join"
    "le" "lt" "meet" "mod" "ne" "notadj" "notin" "notsubset" "or" "sdiff"
    "subset" "xor" "not")
  "Operators used by magma")

(defconst magma-proc-keywords
  '("assert" "assert2" "assert3" "break" "clear" "continue" "declare" "delete"
    "error" "error if" "eval" "exit"
    "forward" "fprintf" "freeze" "iload" "import" "load" "local" "print"
    "printf" "quit" "random" "read" "readi" "require" "requirege"
    "requirerange" "restore" "return" "save" "time" "vprint" "vprintf" "vtime")
  "Keywords representing a procedure")

(defconst magma-function-keywords
  '("assigned" "exists" "forall")
  "Keywords representing a function")

(defconst magma-constants
  '("true" "false")
  "Constants in magma")

(defconst magma-constructors
  '("car" "case" "cop" "elt" "ext" "func" "hom" "ideal" "lideal" "map"
    "ncl" "pmap" "proc" "quo" "rec" "recformat" "rideal" "sub")
  "Constructors used by magma")

(defconst magma-block-end
  '("try" "case" "function" "procedure" "intrinsic")
  "Keywords opening a block, closing with an end.")

(defconst magma-block-open
  '("try" "catch" "case" "when" "then" "else" "do" "repeat"
    "function" "procedure" "intrinsic")
  "Keywords opening a block.")

(defconst magma-block-close
  '("until"
    "end"
    "when" "elif" "else" "catch")
  "Keywords closing a block.")


(defvar magma-font-lock-keywords
  (list
   (list
    "->\\([[:alnum:][:space:]\n\r]*\\)\\({[^}]*}\\)"
    '(1 font-lock-type-face) '(2 font-lock-string-face))
   (list
    (concat (regexp-opt magma-constructors 'words) "[[:space:]\n\r]*<")
    1 'font-lock-builtin-face)
   (list
    (regexp-opt magma-keywords 'words)
    0 'font-lock-keyword-face)
   (list
    (regexp-opt magma-proc-keywords 'words)
    0 'font-lock-keyword-face)
   (list
    (regexp-opt magma-function-keywords 'words)
    0 'font-lock-keyword-face)
   (list
    (regexp-opt magma-operators 'words)
    0 'font-lock-keyword-face)
   (list
    (regexp-opt magma-constants 'words)
    0 'font-lock-constant-face)
   (list
    (concat
     "\\(~?" magma-ident-regexp "\\)[[:space:]\n\r]*::[[:space:]\n\r]*"
     "\\(\\w+\\|\\.\\|\\[\\w*\\]\\|{\\w*}\\|{\\[\\w*\\]}\\|{@\\w*@}\\|"
     "{\\*\\w*\\*}\\|<>\\)")
    '(1 font-lock-variable-name-face) '(3 font-lock-type-face))
   (list
    (concat "\\(" magma-ident-regexp "\\|\\_<\\$\\$\\_>\\)[[:space:]\n\r]*(")
    1 'font-lock-function-name-face)
   )
  "Default expressions to highlight in Magma mode.")

(defvar magma-interactive-font-lock-keywords
  (list

   ;; ListSignatures
   (list
    "^     '\\(\\w+\\)\\(:=\\)?'(" 1 'font-lock-keyword-face)
   (list
    "^     \\(\\w+\\)(" 1 'font-lock-function-name-face)
   (list
    ") -> \\(\\.\\|\\w+\\(, \\w+\\)*\\)"
    1 'font-lock-type-face)
   (list
    "<\\([][:alnum:][]*\\)>\\( \\(~?\\w+\\)\\|,\\|)\\)"
    '(1 font-lock-type-face)
    '(3 font-lock-variable-name-face))
   (list
    "^\\(\\w+\\)(\\([^):]*\\)"
    '(1 font-lock-function-name-face)
    '(2 font-lock-variable-name-face))
   (list
    "^\\(\\w+\\)<\\([^>|]*\\)|\\([^>]*\\)"
    '(1 font-lock-builtin-face)
    '(2 font-lock-variable-name-face)
    '(3 font-lock-variable-name-face))
   (list
    "[>)] : \\([^-]*\\)->\\([^\n]*\\)"
    '(1 font-lock-type-face)
    '(2 font-lock-type-face))
   )
  "Default expressions to highlight in Magma Interactive mode.")

(provide 'magma-font-lock)

;;; magma-font-lock.el ends here
