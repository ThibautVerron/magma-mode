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

(defvar magma-smie-verbose nil "Information about syntax state")


;;;;;
;; Here we describe the smie grammar for the magma language. Some
;; tokens are ambiguous, we ask the lexer to lift the ambiguity and
;; pass unique "phony" token names to the parser. These token names
;; are all prefixed with '', so that in case they appear verbatim in
;; a magma buffer, the lexer will not pick them as tokens: by default,
;; the lexer picks "\w+" matches as token, and '' is not a word
;; consistuant. (Proof : place the point at "token" above, then press
;; `C-M-s' and enter "\w". Check that the '' is not highlighted amongst
;; the matches)
;;;;;

(defconst magma-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '(;; Identifier
      (id)

      ;; Several identifiers, separated by commas
      (idlist (id)
              (idlist "," idlist))

      ;; Assignment
      (assign (idlist ":=" expr))

      ;; Instruction
      (inst (assign)
            (expr)
            ("for" dobody "end for")
            ("while" dobody "end while")
            ("repeat" insts "until" expr)
            ("if" ifbody "end if")
            ("case" casebody "end case")
            ("try" trybody "end try")
            ("function" id "fun(" funargs "fun)" insts "end function")
            ("procedure" id "fun(" funargs "fun)" insts "end procedure")
            ("special1" specialargs)
            ("special2" specialargs "special:" specialargs))

      ;; Several instructions
      (insts (insts ";" insts)
             (inst))
      
      ;; Expression
      (expr (id)
            ;;("(" expr ")")
            ("&")
            (expr "where" id "is" expr)
            (expr "select" expr "selectelse" expr)
            ("~" expr)
            ("#" expr)
            (expr "+" expr)
            ;; (expr "::" expr) ;; For intrinsics only
            ("recformat<" recspec ">")
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
            ("function" "fun(" funargs "fun)" insts "end function")
            ("procedure" "fun(" funargs "fun)" insts "end procedure")
            (funcall))

      ;; What appears in a list or a similar construct (basic)
      (listargs (enum)
                (enum "paren:" listargspipe))

      (listargspipe (enum)
                    (enum "|" enum))

      (recspec (id)
               (id "type:" id) ;; In recformat only
               (recspec "," recspec))
      
      (enum (expr)
            (enum "," enum))

      ;; (angleargs (listargs)
      ;;            (id "->" listargspipe))

      ;; (recspec (id)
      ;;          (id ":" expr)
      ;;          (recspec "," recspec)
      ;;          )

      
      ;; What appears in a function or procedure arguments
      ;;(funbody (id "fun(" funargs "fun)" insts))
      (funcall (id "(" funargs ")"))
      (funargs (lfunargs)
               (lfunargs "paren:" rfunargs))
      (lfunargs (expr)
                (lfunargs "," lfunargs))
      (rfunargs (assign)
                (rfunargs "," rfunargs))

      ;; What appears after a special construct
      (specialargs (expr)
                   (specialargs "," specialargs))

      ;; What appears in a "for" of a "while" block
      (dobody (expr "do" insts))

      ;; What appears in a "try" block
      (trybody (insts "catche" insts))
      
      ;; What appears in a "if" block
      (ifbody (ifelsebody) (ifthenbody "elif" ifbody))
      (ifelsebody (ifthenbody) (ifthenbody "else" insts))
      (ifthenbody (expr "then" insts))

      ;; What appears in a "case" block
      (casebody (expr "case:" caseinsts))
      (caseinsts (caseinsts "when" expr "when:" insts) 
                 (caseinsts "else" insts)))
    '((left "if")
      (nonassoc "end if")
      (assoc "then" "else"))
    '((left "case")
      (nonassoc "end case")
      (assoc "case:")
      (assoc "when")
      (assoc "when:"))
    '((nonassoc "end function" "end procedure")
      (assoc ",")
      (left "|") (left "paren:")
      (assoc ":="))
    '((assoc "special:"))
    '((assoc ";")
      (assoc "::")
      (assoc "select" "selectelse")
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
      (assoc "type:")
      (left "#")
      (left "&")
      (left "~"))))
  "BNF grammar for the SMIEngine.")

(defconst magma-smie-tokens-regexp
  (concat
   "\\("
   (regexp-opt '("," "|" ";" ":="))
   "\\|" 
   (regexp-opt '("for" "while" "repeat" "until" "do" "if" "else" "elif" 
                 "case" "when" "try" "catch" "function" "procedure"
                 "then" "where" "is" "select") 'words)
   "\\)")
  "SMIE tokens for magma keywords, except for block ends.")

(defconst magma-smie-end-tokens-regexp
  (regexp-opt '("end while" "end if" "end case" "end try" "end for"
                "end function" "end procedure") 'words)
  "SMIE tokens for block ends.")

(defconst magma-smie-operators-regexp
  (concat
   "\\("
   (regexp-opt '("*" "+" "^" "-" "/" "~" "." "!" "#" "->" "&"))
   "\\|"
   (regexp-opt '("div" "mod" "in" "notin" "cat"
                 "eq" "ne" "lt" "gt" "ge" "le"
                 "and" "or" "not") 'words)
   "\\)")
  "Regexp matching magma operators.")

(defconst magma-smie-special1-regexp
  (regexp-opt
   '("assert" "assert2" "assert3" "break" "clear" "continue" "declare" "delete"
     "error" "error if" "eval" "exit" "forward" "fprintf" "freeze" "iload"
     "import" "load" "local" "print" "printf" "quit" "random" "read" "readi"
     "require" "requirege" "requirerange" "restore" "return" "save")
   'words)
  "Regexp matching special functions requiring no parentheses and no colon")

(defconst magma-smie-special2-regexp
  (regexp-opt '("vprint" "vprintf") 'words)
  "Regexp matching special functions requiring no parentheses but a colon")

;;;;;
;; The grammar alone is not enough to properly read some magma code:
;; some tokens have different roles in the grammar, but the same
;; printed text. So we define here some functions to help the parser
;; separate these tokens.
;;;;;

(defun magma-in-literal ()
  "Return the type of literal point is in, if any.
The return value is `c' if in a C-style comment, `c++' if in a
C++ style comment, `string' if in a string literal, `intrinsic'
if in an intrinsic description or nil if somewhere else."
  (let ((state (parse-partial-sexp (point-min) (point))))
    (cond
     ((and
       (= (elt state 0) 1)
       (= (char-after (elt state 1)) ?{)
       (save-match-data
         (looking-back
          (concat
           "\\<intrinsic\\>[^;]*"
           (regexp-quote
            (buffer-substring-no-properties
             (elt state 1) (point)))) nil )))
      'intrinsic)
     ((elt state 3) (cons 'string (elt state 8)))
     ((elt state 4) (cons (if (elt state 7) 'c++ 'c) (elt state 8)))
     (t nil))))

(defun magma-not-in-comment-p ()
  "Returns true only if we are not in a magma comment"
  (let ((lit (car (magma-in-literal))))
    (and (not (eq lit 'c))
	 (not (eq lit 'c++)))))

(defun magma-looking-at-end-of-line (&optional endchar)
  "Returns t only is the point is at the end of a line."
  (looking-at (concat endchar "[[:space:]]*$")))


(defun magma--smie-identify-colon ()
  "Return the token type for a colon.

If point is at a colon, returns the a xppropriate token for that
  colon. The returned value is :
- \"case:\" if the colon is at the end of a case... : construct
- \"when:\" if the colon is at the end of a when... : construct
- \"special:\" if the colon is a separator for one of the special
  function calls
- \"paren:\" if the colon is a separator in a pair of
  parens (parameter for a function, specification for a list...)
- \":\" otherwise (this shouldn't appear in a syntactically correct buffer)"
  (let ((forward-sexp-function nil)) ;; Do not use the smie table if loaded!
    (save-excursion
      (catch 'token
        (while t
          (condition-case nil 
              (progn
                (forward-comment (- (point)))
                 ;(up-list)
                (backward-sexp)
                (cond
                 ((looking-at "case") (throw 'token "case:"))
                 ((looking-at "when") (throw 'token "when:"))
                 ((looking-at magma-smie-special2-regexp)
                  (throw 'token "special:"))
                 ;; ((up-list)
                 ;;  (backward-sexp)
                 ;;  (cond
                 ((looking-back "recformat<[[:space:]]*" nil)
                  (throw 'token "type:"))
                 ((bobp) (throw 'token ":"))))
            (error (throw 'token "paren:")))
        )))));; ))

(defun magma--smie-identify-else()
  "Return the token type for a else.

Assume the point is before \"else\". Returns:
- \"selectelse\" : if the \"else\" belongs to a select
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
                 ((looking-at "select") (throw 'token "selectelse"))
                 ((looking-at "\\(case\\|if\\)") (throw 'token "else"))
                 ;; We also need to take care of the case of a select
                 ;; in a if. There is no way we can see two
                 ;; selectelse without a select in between.
                 ((looking-at "else") (throw 'token "else"))
                 ((bobp) (throw 'token "else"))
                 ))
            (error (throw 'token "else"))))
        ))))

(defun magma--backward-word-strictly ()
  "Backward-word ignoring e.g. subword-backward

Poorman's implementation of the backward-word-strictly function
added in Emacs 25.1. It is only used to move over function
identifiers, so it does not need to take into account all
possible word separators, only white space."

  (if (version< emacs-version "25.1")
      (progn
        (search-backward-regexp "[[:alnum:]]")
        (search-backward-regexp "[[:space:]]"))
    (backward-word-strictly)
      )
  )

(defun magma--smie-looking-at-fun-openparen ()
  "Returns t if we are currently looking at the open paren of a
  block of function arguments."
  (condition-case nil
      (and (looking-at "(")
           (save-excursion
             (or
              (looking-back "\\<\\(function\\|procedure\\)[[:space:]]*"
                            (- (point) 10))
              (progn
                (magma--backward-word-strictly)
                (looking-back "\\<\\(function\\|procedure\\)[[:space:]]*"
                              (- (point) 10))))))
    (error nil) ))

(defun magma--smie-looking-at-fun-closeparen ()
  "Returns t if we are currently looking at the closing paren of a
  block of function arguments."
  (and
   (not (eobp))
   (save-excursion
     (forward-char)
     (magma--smie-looking-back-fun-closeparen))))

(defun magma--smie-looking-back-fun-openparen ()
  "Returns t if we are currently looking at the open paren of a
  block of function arguments."
  (and
   (not (bobp))
   (save-excursion
    (forward-char -1)
    (magma--smie-looking-at-fun-openparen))))

(defun magma--smie-looking-back-fun-closeparen ()
  "Returns t if we are currently looking at the closing paren of a
  block of function arguments."
  (let ((forward-sexp-function nil))
    (and (looking-back ")" (- (point) 1))
         (save-excursion
           (backward-sexp)
           (magma--smie-looking-at-fun-openparen)))))

(defun magma-smie-forward-token ()
  "Read the next token in the magma buffer"
  (forward-comment (point-max))
  (cond
   ((magma--smie-looking-at-fun-openparen)
    (forward-char)
    "fun(")
   ((magma--smie-looking-at-fun-closeparen)
    (forward-char)
    "fun)")
   ((looking-at magma-smie-operators-regexp)
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   ((looking-at magma-smie-end-tokens-regexp)
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   ((looking-at "\\<catch [[:alnum:]]+")
    (goto-char (match-end 0))
    "catche")
   ((looking-at "else")
    (let ((elsetoken (save-match-data (magma--smie-identify-else))))
      (goto-char (match-end 0))
      elsetoken))
   ((looking-at magma-smie-tokens-regexp)
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   ((looking-at magma-smie-special1-regexp)
    (goto-char (match-end 0))
    "special1")
   ((looking-at magma-smie-special2-regexp)
    (goto-char (match-end 0))
    "special2")
   ;; ((looking-at "then")
   ;;  (goto-char (match-end 0))
   ;;  (magma-identify-then))
   ((looking-at ":[^=]")
    (let ((token (magma--smie-identify-colon)))
      (forward-char 1)
      token))
   (t (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-forward "w_")
              (point))))))

(defun magma-smie-backward-token ()
  "Read the previous token in the magma buffer."
  (forward-comment (- (point)))
  (let ((bolp
         (save-excursion
           (move-beginning-of-line nil)
           (point))))
    (cond
     ((magma--smie-looking-back-fun-openparen)
      (forward-char -1)
      "fun(")
     ((magma--smie-looking-back-fun-closeparen)
      (forward-char -1)
      "fun)")
     ((looking-back magma-smie-operators-regexp bolp)
      (goto-char (match-beginning 0))
      (match-string-no-properties 0))
     ((looking-back magma-smie-end-tokens-regexp bolp)
      (goto-char (match-beginning 0))
      (match-string-no-properties 0))
     ((looking-back "\\<catch [[:alnum:]]+" nil)
      (goto-char (match-beginning 0))
      "catche")
     ((looking-back "else" nil)
      (goto-char (match-beginning 0))
      (magma--smie-identify-else))
     ((looking-back magma-smie-special1-regexp bolp)
      (goto-char (match-beginning 0))
      "special1")
     ;; Check for special1 before tokens, because of "error if" and "if"
     ((looking-back magma-smie-special2-regexp bolp)
      (goto-char (match-beginning 0))
      "special2")
     ((looking-back magma-smie-tokens-regexp bolp)
      (goto-char (match-beginning 0))
      (match-string-no-properties 0))
     ((looking-back ":" nil)
      (forward-char -1)
      (magma--smie-identify-colon))
     (t (buffer-substring-no-properties
         (point)
         (progn (skip-syntax-backward "w_")
                (point)))))))


(defcustom magma-indent-basic 4 "Indentation of blocks"
  :group 'magma
  :type 'integer)

(defcustom magma-indent-args 4
  "Indentation inside expressions (currently mostly ignored)"
  :group 'magma
  :type 'integer)

(defun magma-smie--parent-hanging-p ()
  "Return t if parent of the current token is hanging.

Should only be used in `magma-smie-rules', and probably not
robust in any way."
  (and
   (boundp 'smie--parent)
   (save-excursion
     (goto-char (car (cdr smie--parent)))
     (smie-rule-hanging-p))))

(defun magma-smie-rules (kind token)
  "SMIE indentation rules."
  (when magma-smie-verbose
    (message (format "%s %s parent:%s"
                       kind token
                       (and (boundp 'smie--parent) smie--parent))))
  (pcase (cons kind token)
    (`(:elem . 'basic) magma-indent-basic)
    (`(:elem . 'arg)
     (if (smie-rule-parent-p "special1" "special2")
         (smie-rule-parent)
       ;; This piece of code seems to never be evaluated
       magma-indent-basic))
    (`(:before . ":=") (smie-rule-parent))
    (`(:after . ":=")
     (if (smie-rule-hanging-p)
         magma-indent-basic
         (smie-rule-parent)))
    (`(:before . "paren:")
     (when (magma-smie--parent-hanging-p)
       magma-indent-basic))
    (`(:before . "|") (smie-rule-parent))
    (`(,_ . ",") (smie-rule-separator kind))
    ;; (`(:after . ",")
    ;;  (if (smie-rule-parent-p "(" "{" "[" "<"
    ;;                          "fun(" "paren:"
    ;;                          "special1" "special2" "special:")
    ;;      0
    ;;    (if (or (smie-rule-sibling-p)
    ;;            (smie-rule-prev-p ",")
    ;;            (smie-rule-next-p ",")
    ;;            (smie-rule-bolp)
    ;;            )
    ;;        (smie-rule-parent magma-indent-basic)
    ;;      magma-indent-basic)))

    (`(:after . ";") 0)
    (`(:after . ,(or `"special1" `"special2")) magma-indent-basic)
    (`(:after . "special:") magma-indent-basic)
    (`(:after . "when:") magma-indent-basic)
    (`(:before . "when") (smie-rule-parent (- -1 magma-indent-basic)))
    (`(:after . "do")
     (smie-rule-parent magma-indent-basic))
    (`(:before . "then")
     (smie-rule-parent magma-indent-basic))
    (`(:after . "then")
     (smie-rule-parent magma-indent-basic))
    (`(:after . "else")
     (if (smie-rule-parent-p "if" "elif")
         (smie-rule-parent magma-indent-basic)
       (smie-rule-parent -1)))
    ;; The parent of an "else" in "if then else" is the corresponding
    ;; "then", not the "if"
    (`(:before . "catche") (smie-rule-parent))
    (`(:after . "catche") magma-indent-basic)
    (`(:before . "elif") (smie-rule-parent))
    (`(:before . "else")
     (if (smie-rule-parent-p "if" "elif" "case")
         (smie-rule-parent)))

    ;; (`(:before . "when")
    ;;  (smie-rule-parent 7))
    
    ;; Indentation for the functions, with one syntax or the other
    (`(:after . "fun)")
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
  (list ";" "then" "else" "do" "try" "catche" "when:" "case:" "fun)")
  "SMIE tokens marking the end of an expression.")

(defun magma--smie-looking-back-end-of-expr-p ()
  "Test whether we are at the beginning of an expression"
  (let ((prevtoken
         (save-excursion (magma-smie-backward-token))))
    (or (-contains? magma-end-of-expr-tokens prevtoken)
        (bobp))))

(defun magma--smie-looking-at-end-of-expr-p ()
  "Test whether we are before the end of an expression"
  (let ((nexttoken
         (save-excursion (magma-smie-forward-token))))
    (-contains? magma-end-of-expr-tokens nexttoken)))

(defun magma-beginning-of-expr ()
  "Go to the beginning of the current expression."
  (interactive)
  (let ((lit (magma-in-literal)))
    (when (eq (car lit) 'string)
      (goto-char (- (cdr lit) 1))))
  (let ((last-token "nonempty"))
    (while (not (magma--smie-looking-back-end-of-expr-p))
      (if (equal last-token "")
          (progn
            (condition-case nil
                (backward-sexp)
              ('error (backward-up-list))
            (setq last-token "nonempty")))
        (setq last-token (magma-smie-backward-token))))))
    

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
  (interactive)
  (condition-case nil
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
  "Close the innermost open block at point.

Note: does not work for function and procedure blocks"
  (interactive)
  (smie-close-block)
  (insert ";"))

;;;;;; Auto-fill

;; WIP


(provide 'magma-smie)


;;; magma-smie.el ends here
