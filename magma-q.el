;;; magma-q.el --- Simple queue structure

;; Copyright (C) 2014  Thibaut Verron

;; Author: Thibaut Verron <thibaut.verron@gmail.com>
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

;; Basic mutable queue implementation, used for the magma input queue. 

;;; Code:

(defun magma-q-create ()
  "Create an empty queue"
  (list nil nil))

(defun magma-q-is-empty? (queue)
  "Tests whether the queue is empty"
  (and (null (car queue))
       (null (nth 1 queue))))

(defun magma-q-push (queue elt)
  "Add an element to the queue"
  (setcdr queue (list (cons elt (nth 1 queue)))))

(defun magma-q--flush (queue)
  "Flush the queue"
  (setcar queue (nconc (car queue) (nreverse (nth 1 queue))))
  (setcdr queue '(())))

(defun magma-q--flush-if-needed (queue)
  "Flush the queue if there is nothing available to pop"
  (when (null (car queue)) (magma-q--flush queue)))

(defun magma-q-pop (queue)
  "Pop the first element out of the queue"
  (magma-q--flush-if-needed queue)
  (if (magma-q-is-empty? queue)
      (error "Empty queue.")
    (let ((elt (car (car queue))))
      (setcar queue (cdr (car queue)))
      elt)))


(provide 'magma-q)
;;; magma-q.el ends here
