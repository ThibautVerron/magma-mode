;;; magma-snippets.el --- 

;; Copyright (C) 2014  Thibaut VERRON

;; Author: Thibaut VERRON <verron@ilithye.calsci.lip6.fr>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Yasnippet snippets for magma-mode

;;; Code:

(defun magma-should-expand-snippet ()
  (and
   (looking-at "$")
   (save-excursion
     (not (and (backward-word 2)
               (looking-at "end"))))))

(defvar magma-snippets-dir (f-join magma-path "snippets")
  "Directory containing magma-mode snippets")

(defun magma-snippets-initialize ()
  (when (boundp 'yas-snippet-dirs)
    (add-to-list 'yas-snippet-dirs magma-snippets-dir t))
  (yas-load-directory magma-snippets-dir))

(eval-after-load "yasnippet"
  (magma-snippets-initialize))

(provide 'magma-snippets)


;;; magma-snippets.el ends here
