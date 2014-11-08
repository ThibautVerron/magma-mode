;;; magma-smie.el --- Indentation in the magma buffers. ;

;; Copyright (C) 2007-2014 Luk Bettale
;;               2013-2014 Thibaut Verron
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

(require 'smie)

(require 'magma-vars)

(defvar magma-smie-verbose-p nil "Information about syntax state")
;; Not used atm

;;;;;
;; Here we describe the smie grammar for the magma language. Some
;; tokens are ambiguous, we ask the lexer to lift the ambiguity and
;; pass unique "phony" token names to the parser. These token names
;; are all prefixed with '@', so that in case they appear verbatim in
;; a magma buffer, the lexer will not pick them as tokens: by default,
;; the lexer picks "\w+" matches as token, and '@' is not a word
;; consistuant. (Proof : place the point at "token" above, then press
;; `C-s' and enter "\w". Check that the '@' is not highlighted amongst
;; the matches)
;;;;;

(makunbound 'magma-smie-grammar)

(defvar magma-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '(;; Identifier
      (id)

      ;; Assignment
      (assign (id ":=" expr))
      
      ;; Instruction
      (inst (assign)
            (expr)
            ("for" expr "do" insts "end for")
            ("while" expr "do" insts "end while")
            ("if" ifbody "end if")
            ("case" expr "@case:" caseinsts "end case")
            ("try" insts "@catche" insts "end try")
            ("function" id "@fun(" funargs "@fun)" insts "end function")
            ("procedure" id "@fun(" funargs "@fun)" insts "end procedure")
            ("@special1" specialargs)
            ("@special2" specialargs "@special:" specialargs)
            )

      ;; Several instructions
      (insts (insts ";" insts)
             (inst))
      
      ;; Expression
      (expr (id)
            ;;("(" expr ")")
            ("&")
            (expr "where" id "is" expr)
            (expr "select" expr "@selectelse" expr)
            ("~" expr)
            ("#" expr)
            (expr "+" expr)
            ;; (expr "::" expr) ;; For intrinsics only
            (expr "-" expr)
            (expr "*" expr)
            (expr "/" expr)
            (expr "^" expr)
            (expr "!" expr)
            (expr "." expr)
            (expr "cat" expr)
            (expr "div" expr)
            (expr "mod" expr)
            (expr "and" expr)
            (expr "or" expr)
            ("not" expr)
            (expr "eq" expr)
            (expr "ne" expr)
            (expr "lt" expr)
            (expr "le" expr)
            (expr "gt" expr)
            (expr "ge" expr)
            (expr "in" expr)
            ("function" "@fun(" funargs "@fun)" insts "end function")
            ("procedure" "@fun(" funargs "@fun)" insts "end procedure")
            (funcall)
            ("[" listargs "]")
            ("<" listargs ">")
            )

      ;; What appears in a list or a similar construct (basic)
      (listargs (expr)
                (listargs "," listargs)
                (listargs "@paren:" listargs)
                (listargs "|" listargs))

      ;; What appears in a function or procedure arguments
      ;;(funbody (id "@fun(" funargs "@fun)" insts))
      (funcall (id "(" funargs ")"))
      (funargs (lfunargs)
               (lfunargs "@paren:" rfunargs))
      (lfunargs (expr)
                (lfunargs "," lfunargs))
      (rfunargs (assign)
                (rfunargs "," rfunargs))

      ;; What appears after a special construct
      (specialargs (expr)
                   (specialargs "," specialargs))

      ;; What appears in a "if" block
      (ifbody (ifelsebody) (ifthenbody "elif" ifbody))
      (ifelsebody (ifthenbody) (ifthenbody "else" insts))
      (ifthenbody (expr "then" insts))

      ;; What appears in a "case" block
      (caseinsts (caseinsts "when" expr "@when:" insts) 
                 (caseinsts "else" insts)
                 )
      )
    '((left "if")
      (nonassoc "end if")
      (assoc "then" "else"))
    '((left "case")
      (nonassoc "end case")
      (assoc "@case:")
      (assoc "when")
      (assoc "@when:"))
      ;; (assoc "else")
      ;;(left "ifthen")
      ;;(left "elifthen" "@when:")
    '((nonassoc "end function" "end procedure")
      ;; (left "(") (right ")")
      ;;(left ":")
      (assoc ",")
      (left "|") (left "@paren:")
      (assoc ":="))
    '((assoc "@special:"))
    '((assoc ";")
      (assoc "::")
      (assoc "select" "@selectelse")
      (assoc "where" "is")
      (left "not")
      (assoc "or")
      (assoc "and")
      (assoc "ge")
      (assoc "gt")
      (assoc "le")
      (assoc "lt")
      (assoc "ne")
      (assoc "eq")
      (assoc "in")
      (assoc "+")
      (assoc "-")
      (assoc "mod")
      (assoc "div")
      (assoc "cat")
      (assoc "/")
      (assoc "*")
      (assoc "^")
      (assoc ".")
      (assoc "!")
      (left "#")
      (left "&")
      (left "~"))
   ))
  "BNF grammar for the SMIEngine.")

(defvar magma-smie-tokens-regexp
  (concat
   "\\("
   (regexp-opt '("," "|" ";" "(" ")" "[" "]" "<" ">" ":="))
   "\\|" 
   (regexp-opt '("for" "while" "do" "if" "else" "elif" 
                 "case" "when" "try" "catch" "function" "procedure"
                 "then" "where" "is" "select") 'words)
   "\\)")
  "SMIE tokens for magma keywords, except for block ends.")

(defvar magma-smie-end-tokens-regexp
  (regexp-opt '("end while" "end if" "end case" "end try" "end for"
                "end function" "end procedure") 'words)
  "SMIE tokens for block ends.")

(defvar magma-smie-operators-regexp
  (concat
   "\\("
   (regexp-opt '("*" "+" "^" "-" "/" "~" "." "!" "#" "->" "&"))
   "\\|"
   (regexp-opt '("div" "mod" "in" "notin" "cat"
                 "eq" "ne" "lt" "gt" "ge" "le"
                 "and" "or" "not"
                 ) 'words)
   "\\)")
  "Regexp matching magma operators.")


(defvar magma-smie-special1-regexp
  (regexp-opt
   '("assert" "assert2" "assert3" "break" "clear" "continue" "declare" "delete"
     "error" "error if" "eval" "exit" "forward" "fprintf" "freeze" "iload"
     "import" "load" "local" "print" "printf" "quit" "random" "read" "readi"
     "require" "requirege" "requirerange" "restore" "return" "save")
   'words)
  "Regexp matching special functions requiring no parentheses and no colon")

(defvar magma-smie-special2-regexp
  (regexp-opt '("vprint" "vprintf") 'words)
  "Regexp matching special functions requiring no parentheses but a colon")

(defun magma-identify-colon ()
  "If point is at a colon, returns the appropriate token for that
  colon. The returned value is :
- \"@case:\" if the colon is at the end of a case... : construct
- \"@when:\" if the colon is at the end of a when... : construct
- \"@special:\" if the colon is a separator for one of the special
  function calls
- \"@paren:\" if the colon is a separator in a pair of
  parens (parameter for a function, specification for a list...)
- \":\" otherwise (this shouldn't appear)"
  (let ((forward-sexp-function nil)) ;; Do not use the smie table if loaded!
    (save-excursion
      (catch 'token
        (while t
          (condition-case nil 
              (progn
                (forward-comment (- (point)))
                (backward-sexp)
                (cond
                 ((looking-at "case") (throw 'token "@case:"))
                 ((looking-at "when") (throw 'token "@when:"))
                 ((looking-at magma-smie-special2-regexp)
                  (throw 'token "@special:"))
                 ((bobp) (throw 'token ":"))
                 ))
            (error (throw 'token "@paren:"))))
        ))))

(defun magma-identify-else()
  "Assume the point is before \"else\". Returns:
- \"@selectelse\" : if the \"else\" belongs to a select
- \"else\" : otherwise, that is if the \"else\" belongs to an if
  or a case"
  (let ((forward-sexp-function nil)) ;; Do not use the smie table if loaded!
    (save-excursion
      (catch 'token
        (while t
          (condition-case nil 
              (progn
                (forward-comment (- (point)))
                (backward-sexp)
                (cond
                 ((looking-at "select") (throw 'token "@selectelse"))
                 ((looking-at "\\(case\\|if\\)") (throw 'token "else"))
                 ;; We also need to take care of the case of a select
                 ;; in a if. There is no way we can see two
                 ;; selectelse without a select in between.
                 ((looking-at "else") (throw 'token "else"))
                 ((bobp) (throw 'token "else"))
                 ))
            (error (throw 'token "else"))))
        ))))
  
(defun magma-looking-at-fun-openparen ()
  "Returns t if we are currently looking at the open paren of a
  block of function arguments."
  (condition-case nil
      (and (looking-at "(")
           (save-excursion
             (or
              (looking-back "\\(function\\|procedure\\)[[:space:]]*"
                            (- (point) 10))
              (progn
                (backward-word)
                (looking-back "\\(function\\|procedure\\)[[:space:]]*"
                              (- (point) 10))))))
    (error nil) ))

(defun magma-looking-at-fun-closeparen ()
  "Returns t if we are currently looking at the closing paren of a
  block of function arguments."
  (and
   (not (eobp))
   (save-excursion
     (forward-char)
     (magma-looking-back-fun-closeparen))))

(defun magma-looking-back-fun-openparen ()
  "Returns t if we are currently looking at the open paren of a
  block of function arguments."
  (and
   (not (bobp))
   (save-excursion
    (forward-char -1)
    (magma-looking-at-fun-openparen))))

(defun magma-looking-back-fun-closeparen ()
  "Returns t if we are currently looking at the closing paren of a
  block of function arguments."
  (let ((forward-sexp-function nil))
    (and (looking-back ")")
         (save-excursion
           (backward-sexp)
           (magma-looking-at-fun-openparen)))))

(defun magma-smie-forward-token ()
  "Read the next token in the magma buffer"
  (forward-comment (point-max))
  (cond
   ((magma-looking-at-fun-openparen)
    (forward-char)
    "@fun(")
   ((magma-looking-at-fun-closeparen)
    (forward-char)
    "@fun)")
   ((looking-at magma-smie-operators-regexp)
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   ((looking-at magma-smie-end-tokens-regexp)
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   ((looking-at "\\<catch [[:alnum:]]+")
    (goto-char (match-end 0))
    "@catche")
   ((looking-at "else")
    (let ((elsetoken (save-match-data (magma-identify-else))))
      (goto-char (match-end 0))
      elsetoken))
   ((looking-at magma-smie-tokens-regexp)
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   ((looking-at magma-smie-special1-regexp)
    (goto-char (match-end 0))
    "@special1")
   ((looking-at magma-smie-special2-regexp)
    (goto-char (match-end 0))
    "@special2")
   ;; ((looking-at "then")
   ;;  (goto-char (match-end 0))
   ;;  (magma-identify-then))
   ((looking-at ":[^=]")
    (let ((token (magma-identify-colon)))
      (forward-char 1)
      token))
   (t (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-forward "w_\"")
              (point))))
   ))

(defun magma-smie-backward-token ()
  "Read the previous token in the magma buffer."
  (forward-comment (- (point)))
  (let ((bolp
         (save-excursion
           (move-beginning-of-line nil)
           (point))))
    (cond
     ((magma-looking-back-fun-openparen)
      (forward-char -1)
      "@fun(")
     ((magma-looking-back-fun-closeparen)
      (forward-char -1)
      "@fun)")
     ((looking-back magma-smie-operators-regexp bolp)
      (goto-char (match-beginning 0))
      (match-string-no-properties 0))
     ((looking-back magma-smie-end-tokens-regexp bolp)
      (goto-char (match-beginning 0))
      (match-string-no-properties 0))
     ((looking-back "\\<catch [[:alnum:]]+")
      (goto-char (match-beginning 0))
      "@catche")
     ((looking-back "else")
      (goto-char (match-beginning 0))
      (magma-identify-else))
     ((looking-back magma-smie-special1-regexp bolp)
      (goto-char (match-beginning 0))
      "@special1")
     ;; Check for @special1 before tokens, because of "error if" and "if"
     ((looking-back magma-smie-special2-regexp bolp)
      (goto-char (match-beginning 0))
      "@special2")
     ((looking-back magma-smie-tokens-regexp bolp)
      (goto-char (match-beginning 0))
      (match-string-no-properties 0))
     ((looking-back ":")
      (forward-char -1)
      (magma-identify-colon))
     (t (buffer-substring-no-properties
         (point)
         (progn (skip-syntax-backward "w_\"")
                (point))))
     )))

(defcustom magma-indent-basic 4 "Indentation of blocks"
  :group 'magma
  :type 'integer)

(defcustom magma-indent-args 4
  "Indentation inside expressions (currently mostly ignored)"
  :group 'magma
  :type 'integer)

(defun magma-smie-rules (kind token)
  "SMIE indentation rules."
  (pcase (cons kind token)
    (`(:elem . basic) magma-indent-basic)
    (`(:elem . args)
     (if (and (boundp 'smie--parent)
              (smie-rule-parent-p "@special1" "@special2"))
         (smie-rule-parent)
       magma-indent-basic))
    (`(:before . ":=") (smie-rule-parent))
    (`(:after . ":=")
     (smie-rule-parent magma-indent-basic))
    (`(:list-intro . ":=") t)
    (`(:after . "@paren:") (smie-rule-parent))
    (`(:after . ,(or `"@special1" `"@special2")) 0)
    (`(:after . "@special:") 0)
    (`(:after . "@when:") magma-indent-basic)
    (`(:before . "when") 0)
    (`(:before . "then")
     (smie-rule-parent magma-indent-basic))
    (`(:after . "then")
     (smie-rule-parent magma-indent-basic))
    (`(:after . "else")
     (smie-rule-parent magma-indent-basic))

    ;; The parent of an "else" in "if then else" is the corresponding
    ;; "then", not the "if"
    (`(:before . "elif") (smie-rule-parent))
    (`(:before . "else")
     (when (smie-rule-parent-p "if" "elif" "case") (smie-rule-parent)))

    ;; Indentation for the functions, with one syntax or the other
    (`(:after . "@fun)")
     magma-indent-basic)
    (`(:before . ,(or `"function" `"procedure"))
     (if (smie-rule-prev-p ":=")
         (progn
           (back-to-indentation)
           (cons 'column (current-column)))))
    (`(:before . ,(or `"end function" `"end procedure"))
     (smie-rule-parent))))

(defun magma-indent-line ()
  "Indent a line according to the SMIE settings."
  (interactive)
  (smie-indent-line))


;; Additional functions for movement

(defconst magma-end-of-expr-tokens
  (list ";" "then" "else" "do" "try" "@catche" "@when:" "@case:" "@fun)")
  "SMIE tokens marking the end of an expression.")

(defun magma-looking-back-end-of-expr-p ()
  "Test whether we are at the beginning of an expression"
  (let ((prevtoken
         (save-excursion (magma-smie-backward-token))))
    (or (-contains? magma-end-of-expr-tokens prevtoken)
        (bobp))))

(defun magma-looking-at-end-of-expr-p ()
  "Test whether we are before the end of an expression"
  (let ((nexttoken
         (save-excursion (magma-smie-forward-token))))
    (-contains? magma-end-of-expr-tokens nexttoken)))

(defun magma-beginning-of-expr ()
  "Go to the beginning of the current expression."
  (interactive)
  (while (not (magma-looking-back-end-of-expr-p))
    (magma-smie-backward-token)))

(defun magma-end-of-expr ()
  "Go to the end of the current expression."
  (interactive)
  (magma-beginning-of-expr)
  (smie-forward-sexp ";")
  (forward-comment (point-max))
  (when (looking-at ";") (forward-char 1)))
;; We should always be looking at a ";" there

(defun magma-previous-expr ()
  "Go to the beginning of the expression, or to the beginning of
  the previous expression if already at the beginning of the
  current one."
  (interactive)
  (let ((prev-point (point)))
    (magma-beginning-of-expr)
    (when (eq prev-point (point))
      (backward-sexp)
      (magma-beginning-of-expr))))

(defun magma-mark-expr ()
  "Mark the current expression"
  (interactive)
  (magma-beginning-of-expr)
  (set-mark-command nil)
  (magma-end-of-expr)
  (setq deactivate-mark nil))

(defconst magma-defun-regexp
  (regexp-opt '("function" "procedure" "intrinsics") 'words)
  "Regexp for words marking the beginning of a defun")

(defun magma-beginning-of-defun (&optional silent)
  "Go to the beginning of the function, procedure or intrinsics
  definition at point"
  (interactive) (condition-case nil
      (search-backward-regexp magma-defun-regexp)
    (error (or silent
               (message "Not in a function, procedure or intrinsics definition")))))

(defun magma-end-of-defun ()
  "Go to the beginning of the function, procedure or intrinsics
  definition at point"
  (interactive)
  (or (looking-at magma-defun-regexp)
      (magma-beginning-of-defun))
  (magma-end-of-expr))


(defun magma-close-block ()
  "Close the innermost open block at point

This function is experimental and cannot reliably be used."
  (interactive)
  (smie-close-block)
  (insert ";"))

;;;;;; Auto-fill

;; WIP


(provide 'magma-smie)


;;; magma-smie.el ends here
