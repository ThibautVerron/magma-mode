(provide 'magma-smie)

(require 'smie)

(defvar magma-smie-verbose-p nil "Information about syntax state")
;; Not used atm


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
            ("case" expr "case:" caseinsts "end case")
            ("try" insts "catche" insts "end try")
            ;; ("function" id "(" funargs ")" insts "end function")
            ;; ("procedure" id "(" funargs ")" insts "end procedure")
            ("function" id "fun(" funargs "fun)" insts "end function")
            ("procedure" id "fun(" funargs "fun)" insts "end procedure")
            ("special1" specialargs)
            ("special2" specialargs "special:" specialargs)
            )

      ;; Several instructions
      (insts (insts ";" insts)
             (inst))
      
      ;; Expression
      (expr (id)
            ;;("(" expr ")")
            (expr "where" id "is" expr)
            (expr "select" expr "else" expr)
            ("~" expr)
            ("#" expr)
            (expr "+" expr)
            (expr "::" expr) ;; For intrinsics only
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
            (funcall)
            ("[" listargs "]")
            ("<" listargs ">")
            )

      ;; What appears in a list or a similar construct (basic)
      (listargs (expr)
                (listargs "," listargs)
                (listargs "paren:" listargs)
                (listargs "|" listargs))

      ;; What appears in a function or procedure arguments
      ;;(funbody (id "fun(" funargs "fun)" insts))
      (funcall (id "(" funargs ")"))
      (funargs (lfunargs)
               (lfunargs "paren:" rfunargs))
      (lfunargs (expr)
                (lfunargs "," lfunargs))
      (rfunargs (assign)
                (rfunargs "," rfunargs))

      ;; What appears after a special construct
      (specialargs (expr)
                   (specialargs "," specialargs))

      ;; What appears in a "if" block
      (ifbody (ifelsebody) (ifbody "elif" ifbody))
      (ifelsebody (ifthenbody) (ifthenbody "else" insts))
      (ifthenbody (expr "then" insts))
      
      ;; What appears in a "case" block
      (caseinsts (caseinsts "when" expr "when:" insts) ;; Not sure if this will work
                 (caseinsts "else" insts))
      )
    '((assoc "elif" "when")
      ;;(left "ifthen")
      ;;(left "elifthen" "when:")
      (nonassoc "end if" "end case" "end function" "end procedure")
      (assoc ";")
      (left "(") (right ")")
      ;;(left ":")
      (assoc ",")
      (left "|") (left "paren:")
      (assoc ":=")
      (assoc "where") (assoc "is") (assoc "select")
      (assoc "else")
      (assoc "special:")
      (assoc "::")
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
      (left "~")
      (assoc "ge")
      (assoc "gt")
      (assoc "le")
      (assoc "lt")
      (assoc "ne")
      (assoc "eq")
      (assoc "in")
      (left "not")
      (assoc "or")
      (assoc "and")
      
      )
    )))

(defvar magma-smie-tokens-regexp
  (concat
   "\\("
   (regexp-opt '("," "|" ";" "(" ")" "[" "]" "<" ">" ":="))
   "\\|" 
   (regexp-opt '("for" "while" "do" "if" "else" "elif" 
                 "case" "when" "try" "catch" "function" "procedure"
                 "then" "where" "is" "select") 'words)
   "\\)"))

(defvar magma-smie-end-tokens-regexp
  (regexp-opt '("end while" "end if" "end case" "end try" "end for"
                "end function" "end procedure") 'words))


(defvar magma-smie-operators-regexp
  (concat
   "\\("
   (regexp-opt '("*" "+" "^" "-" "/" "~" "." "!" "#"))
   "\\|"
   (regexp-opt '("div" "mod" "in" "notin" "cat"
                 "eq" "ne" "lt" "gt" "ge" "le"
                 "and" "or" "not"
                 ) 'words)
   "\\)"))


(defvar magma-smie-special1-regexp
  (regexp-opt '("print" "printf" "load" "save" "restore") 'words)
  "Special functions requiring no parentheses and no colon")

(defvar magma-smie-special2-regexp
  (regexp-opt '("vprint" "vprintf") 'words)
  "Special functions requiring no parentheses but a colon")

(defun magma-identify-colon ()
  "If point is at a colon, returns the appropriate token for that
  colon. The returned value is :
- \"case:\" if the colon is at the end of a case... : construct
- \"when:\" if the colon is at the end of a when... : construct
- \"special:\" if the colon is a separator for one of the special
  function calls
- \"paren:\" if the colon is a separator in a pair of
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
                 ((looking-at "case") (throw 'token "case:"))
                 ((looking-at "when") (throw 'token "when:"))
                 ((looking-at magma-smie-special2-regexp)
                  (throw 'token "special:"))
                 ((bobp) (throw 'token ":"))
                 ))
            (error (throw 'token "paren:"))))
        ))))

(defun magma-looking-at-fun-openparen ()
  "Returns t if we are currently looking at the open paren of a
  block of function arguments."
  (condition-case nil
      (and (looking-at "(")
           (save-excursion
             (backward-word)
             (looking-back "\\(function\\|procedure\\)[[:space:]]*" (- (point) 10))))
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

;; The two following defuns are adapted from the GNU emacs manual, section 23.7.1.4 (SMIE / Defining tokens)

(defun magma-smie-forward-token ()
  (interactive)
  (forward-comment (point-max))
  (cond
   ((magma-looking-at-fun-openparen)
    (forward-char)
    "fun(")
   ((magma-looking-at-fun-closeparen)
    (forward-char)
    "fun)")
   ((looking-at magma-smie-operators-regexp)
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   ((looking-at magma-smie-end-tokens-regexp)
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   ((looking-at "\\<catch [[:alnum:]]+")
    (goto-char (match-end 0))
    "catche")
   ((looking-at magma-smie-tokens-regexp)
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   ((looking-at magma-smie-special1-regexp)
    (goto-char (match-end 0))
    "special1")
   ((looking-at magma-smie-special2-regexp)
    (goto-char (match-end 0))
    "special2")
   ((looking-at "then")
    (goto-char (match-end 0))
    (magma-identify-then))
   ((looking-at ":[^=]")
    (let ((token (magma-identify-colon)))
      (forward-char 1)
      token))
   (t (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-forward "w_")
              (point))))
   ))

;; toto

(defun magma-smie-backward-token ()
  (interactive)
  (forward-comment (- (point)))
  (let ((bolp
         (save-excursion
           (move-beginning-of-line nil)
           (point))))
    (cond
     ((magma-looking-back-fun-openparen)
      (forward-char -1)
      "fun(")
     ((magma-looking-back-fun-closeparen)
      (forward-char -1)
      "fun)")
     ((looking-back magma-smie-operators-regexp bolp)
      (goto-char (match-beginning 0))
      (match-string-no-properties 0))
     ((looking-back magma-smie-end-tokens-regexp bolp)
      (goto-char (match-beginning 0))
      (match-string-no-properties 0))
     ((looking-back "\\<catch [[:alnum:]]+")
      (goto-char (match-beginning 0))
      "catche")
     ((looking-back magma-smie-tokens-regexp bolp)
      (goto-char (match-beginning 0))
      (match-string-no-properties 0))
     ((looking-back magma-smie-special1-regexp bolp)
      (goto-char (match-beginning 0))
      "special1")
     ((looking-back magma-smie-special2-regexp bolp)
      (goto-char (match-beginning 0))
      "special2")
     ((looking-back "then")
      (goto-char (match-beginning 0))
      (magma-identify-then))
     ((looking-back ":")
      (forward-char -1)
      (magma-identify-colon))
     (t (buffer-substring-no-properties
         (point)
         (progn (skip-syntax-backward "w_")
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
  (pcase (cons kind token)
    (`(:elem . basic) magma-indent-basic)
    (`(:elem . args)
     (if (and (boundp 'smie--parent)
              (smie-rule-parent-p "special1" "special2"))
         (smie-rule-parent)
       magma-indent-basic))
    (`(,(or `:after `:before) . ":=") (smie-rule-parent))
    (`(:list-intro . ":=") t)
    (`(:after . ,(or `"special1" `"special2")) 0)
    (`(:after . "special:") 0)

    (`(:after . "when:") magma-indent-basic)
    (`(:before . "when") 0)

    (`(:after . ,(or `"then" `"else"))
     (smie-rule-parent magma-indent-basic))
    (`(:before . "elif") (smie-rule-parent))
    (`(:before . "else")
     (when (smie-rule-parent-p "if" "elif" "case") (smie-rule-parent)))

    ;; (`(:after . "paren:")
    ;;  (smie-rule-parent))
    )
  )

(defun magma-indent-line ()
  (interactive)
  (smie-indent-line))
