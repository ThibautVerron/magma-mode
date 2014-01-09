Magma-mode
==========

Basics
------

This is a mode for editting input files for the computer algebra
software [Magma][] in emacs.

It is based on the [magma-mode][] written by Luk Bettale.  The font-lock
(syntax highlight) specifications were taken from this mode, as well
as the interaction with a magma process through term-mode.

The new features of this mode are principally a new indentation
engine, based on [SMIE][] and a new way of interacting with a magma
process, through comint.

On the other hand, this mode lacks a lot of features offered by Luk
Bettale's [magma-mode], for example the ability to easily communicate
with several magma processes.

[Magma]: http://magma.maths.usyd.edu.au/magma/
[magma-mode]: http://www-polsys.lip6.fr/~bettale/magma-mode/
[SMIE]: http://www.gnu.org/software/emacs/manual/html_node/elisp/SMIE.html

General purpose claims
----------------------

This is beta software, all features are not yet fully implemented.
For most usages, Luk Bettale's [magma-mode] offers a more complete and
stable set of features.

Installation instructions
-------------------------

1. Clone the repository in your favorite elisp folder
    
        cd <FavoriteElispFolder>
        git clone 

2. Add the following to your emacs init file :

        (add-to-list 'load-path "<FavoriteElispFolder>/magma-mode")
        (require 'magma-mode "<FavoriteElispFolder>/magma-mode/magma-mode.el")
        ;; Or simply (require 'magma-mode) if you aren't using any other magma-mode

3. Additional setup:

        ;; If you want to load the mode automatically with some file extensions
        (setq auto-mode-alist
          (append '(("\\.mgm$\\|\\.m$" . magma-mode))
                  auto-mode-alist))
    
        ;; If you need to use a different magma path
        (setq magma-interactive-program "magma")
    
        ;; If you need to pass specific arguments to magma
        (setq magma-interactive-arguments '())

        ;; At the moment, the following options need to be inserted
        ;; *before* "(require 'magma-mode')", and you need to either
        ;; restart emacs or reload the magma loadfile after changing
        ;; it. It is a design flaw and should be fixed in the future.

        ;; If you want to use comint instead of term.
        
        (setq magma-interactive-use-comint t)
    
        ;; Electric return key (try RET or C-j, or C-c C-j) while in a
        ;; string or comment. Like it or hate it, enable it or disable
        ;; it. This is beta, might ruin the indentation of your document,
        ;; crash emacs or eat your cat.
        (setq magma-use-electric-newline t)

Comint vs term
--------------

Term-mode will basically render the magma experience you would have in
a regular terminal emulator, outside of emacs. Only the prompt can be
written, `C-p` or `C-n` browse the history instead of scrolling the
window, *et caetera*.

Another specificity of term-mode is that it intercepts some prefix
keys, most notably `C-c` and `C-x`. For most purposes, `C-x` needs to
be replaced with `C-c`.
So for example, if you need to switch from your magma code buffer to the magma process buffer, then switch back, you'll first press `C-x o` (`other-window`), then `C-c o`.

This can be changed by turning on `term-line-mode` (`C-c C-j`), but this changes the behavior of `term-mode` way beyond the mere interception of signals. To change back to the regular behavior, turn on `term-char-mode` (`C-x C-k`).

On the other hand, comint-mode spawns an interactive process in a
full-featured emacs buffer. You can scroll using the usual keys, you can edit the output of previous commands... It is mostly equivalent to `term-mode` with `term-line-mode`, but in my experience, it suffers from less minor bugs.

Another point to note is that `term-mode` sends input to a terminal, and magma is run in that terminal. Getting the whole thing to run under different systems (windows...) can prove tricky. On the other hand, comint starts the magma process directly from emacs, and so does not have this kind of requirements.

Keymap
------

### In an editting window

 - `C-c C-o` (`magma-switch-to-interactive-buffer-same-frame`): 
   start an interactive magma process, in the same frame
   and in a different buffer
 - `C-c o` (`magma-switch-to-interactive-buffer`): 
   start an interactive magma process in a different frame
 - `C-c C-k` (`magma-kill`): kill the magma process
 - `C-c C-i` (`magma-int`): interrupt the magma process
 - `C-c C-a` (`magma-restart`): restart the magma process
 - `C-c C-x` (`magma-send`): send an expression to the magma process
 - `C-c C-l` (`magma-eval-line`): send the current line to the magma process
 - `C-c C-r` (`magma-eval-region`): send the current region to the
   magma process
 - `C-c C-b` (`magma-eval-buffer`): send the current buffer to the
   magma process
 - `C-c C-e` (`magma-eval`): if region is set, send the region to the
   magma process, else send the current line
 - `C-c C-u` (`magma-eval-until`): send the content of the buffer up
   to the point to the magma process
 - `C-c C-p` (`magma-eval-paragraph`): send the current paragraph to
   the magma process

### In an editting window, with electric return activated

 - `RET` or `C-j` (`magma-newline-and-indent`): inserts a visual newline in the buffer. It is a regular `newline-and-indent` in most situations, but if the point is in the middle of the string, it cuts the string in half before inserting the newline.

    Example: (the [] indicate the point)

        x := "a long sentence, really, a long sentence, [a]nd even a few more words";
        
        <RET> --->
        
        x := "a long sentence, really, a long sentence, "
             cat "and even a few more words";"

   This shouldn't change the way your code is evaluated.

 - `C-RET` or `C-c C-j` (`magma-special-newline-and-indent`): inserts an "evaluated" newline in the buffer. It is a regular `newline-and-indent` in most situations, but in a comment, it will assume that you want to continue the comment in the next line:

        // Comment []
        
        <C-RET> --->
        
        // Comment 
        // []

   and in a string, it will insert an newline character:

        x := "a long sentence, really, a long sentence, [a]nd even a few more words";
        
        <RET> --->
        
        x := "a long sentence, really, a long sentence, \\n"
             cat "and even a few more words";"
