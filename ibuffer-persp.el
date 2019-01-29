;;; ibuffer-persp.el --- Group ibuffer's list by persp-mode perspectives.
;;
;; Copyright © 2019  Jari Sundell
;;
;; Author: Jari Sundell (Rakshasa) <sundell.software@gmail.com>
;; Keywords: ibuffer, perspectives
;; Version: 0.1-git
;; URL: https://github.com/rakshasa/ibuffer-persp
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Adds functionality to ibuffer for grouping buffers by their
;; persp-mode perspective.
;;
;;; Use:
;;
;; To group buffers by perspective:
;;
;;   M-x ibuffer-persp-set-filter-groups-by-persp-name-not-nil
;;
;; or, by perspectives other than the current:
;;
;;   M-x ibuffer-persp-set-filter-groups-by-other-persp-name-not-nil
;;
;; or, make this the default:
;;
;;   (add-hook 'ibuffer-hook
;;     (lambda ()
;;       (ibuffer-persp-set-filter-groups-by-persp-name-not-nil)
;;       (unless (eq ibuffer-sorting-mode 'alphabetic)
;;         (ibuffer-do-sort-by-alphabetic))))
;;
;; Alternatively, use
;; `ibuffer-persp-generate-filter-groups-by-persp-name-not-nil' or
;; `ibuffer-persp-generate-filter-groups-by-other-persp-name-not-nil'
;; to programmatically obtain a list of filter groups that you can
;; combine with your own custom groups.
;;
;; To include vc status info in the ibuffer list, add persp to
;; `ibuffer-formats':
;;
;; (setq ibuffer-formats
;;       '((mark modified read-only " "
;;               (name 18 18 :left :elide)
;;               " "
;;               (size 9 -1 :right)
;;               " "
;;               (mode 16 16 :left :elide)
;;               " "
;;               (persp 11 -1 :left)
;;               " "
;;               filename-and-process)))
;;
;;; Code:

;; requires


(require 'ibuffer)
(require 'ibuf-ext)
(require 'persp-mode)


(defcustom ibuffer-persp-hook nil
  "Hook run when `ibuffer-persp' is called."
  :type 'hook
  :group 'ibuffer)


(defvar-local ibuffer-persp-name nil
  "Ibuffer perspective name.")


(defun ibuffer-persp-current-persp ()
  "Return persp of current ibuffer."
  (ibuffer-assert-ibuffer-mode)
  (when ibuffer-persp-name
    (persp-get-by-name ibuffer-persp-name)))


(defun ibuffer-persp-get-buffer (buff-or-name)
  "Return BUFF-OR-NAME buffer if it is in ibuffer's persp."
  (ibuffer-assert-ibuffer-mode)
  (let ((buffer (persp-get-buffer-or-null buff-or-name))
        (persp (ibuffer-persp-current-persp)))
    (when (and buffer persp)
      (car (memq buffer (persp-buffers persp))))))


(defun* ibuffer-persp-generate-buffer-name (&optional (persp (get-current-persp)))
  "Name of the ibuffer for this perspective."
  (concat "*Ibuffer " (safe-persp-name persp) "*") )


(defun ibuffer-persp-names-not-nil ()
  "Return non-nil perspective names."
  (delq persp-nil-name (persp-names)))


(defun ibuffer-persp-other-names-not-nil ()
  "Return non-nil perspective names, excluding current perspective."
  (ibuffer-assert-ibuffer-mode)
  (delq persp-nil-name (delq ibuffer-persp-name (persp-names))))


(defun ibuffer-persp-buffer-in-persps (buff-or-name)
  "Return perspectives with buffer."
  (persp-other-persps-with-buffer-except-nil buff-or-name nil))


(defun ibuffer-persp-buffer-in-other-persps (buff-or-name)
  "Return perspectives with buffer, excluding current ibuffer perspective."
  (ibuffer-assert-ibuffer-mode)
  (let ((persp (persp-get-by-name ibuffer-persp-name)))
    (persp-other-persps-with-buffer-except-nil buff-or-name persp)))


(defun ibuffer-persp ()
  "Ibuffer for this perspective."
  (interactive)
  (let* ((current-persp (get-current-persp))
         (ibuf-name (ibuffer-persp-generate-buffer-name current-persp)))
    (ibuffer nil ibuf-name) ;; use hooks instead?
    (persp-add-buffer ibuf-name current-persp nil nil)
    (with-current-buffer ibuf-name
      (setq-local ibuffer-persp-name (safe-persp-name current-persp))
      (run-hooks 'ibuffer-persp-hook)
      )))


(define-ibuffer-filter persp-name-not-nil
    "Toggle current view to buffers with (FILTER-PERSP . IGNORE-PERSP), when in non-nil FILTER-PERSP and not in non-nil IGNORE-PERSP."
  (:description "perspective name not nil")
  (let* ((filter-persp-name (car qualifier))
         (ignore-persp-name (cdr qualifier))
         (filter-persp (persp-get-by-name filter-persp-name))
         (ignore-persp (persp-get-by-name ignore-persp-name)))
    (when filter-persp
      (and (persp-contain-buffer-p buf filter-persp)
           (not (when ignore-persp (persp-contain-buffer-p buf ignore-persp)))))))


(defun ibuffer-persp-generate-filter-groups-by-persp-name-not-nil ()
  "Create a set of ibuffer filter groups based on the perspective of buffers."
  (let ((roots (ibuffer-persp-names-not-nil)))
    (mapcar (lambda (persp-name)
              (cons (format "#%s" persp-name)
                    `((persp-name-not-nil ,persp-name . nil))))
            roots)))


(defun ibuffer-persp-generate-filter-groups-by-other-persp-name-not-nil ()
  "Create a set of ibuffer filter groups based on the perspective of buffers, excluding current perspective."
  (let* ((roots (ibuffer-persp-other-names-not-nil)))
    (mapcar (lambda (persp-name)
              (cons (format "#%s" persp-name)
                    `((persp-name-not-nil ,persp-name . ,ibuffer-persp-name))))
            roots)))


(defun ibuffer-persp-set-filter-groups-by-persp-name-not-nil ()
  "Set the current filter groups to filter by perspective name, ignoring nil."
  (interactive)
  (setq ibuffer-filter-groups (ibuffer-persp-generate-filter-groups-by-persp-name-not-nil))
  (ibuffer-update nil t))


(defun ibuffer-persp-set-filter-groups-by-other-persp-name-not-nil ()
  "Set the current filter groups to filter by perspective name, excluding current perspective and nil."
  (interactive)
  (setq ibuffer-filter-groups (ibuffer-persp-generate-filter-groups-by-other-persp-name-not-nil))
  (ibuffer-update nil t))


(defvar ibuffer-persp-header-map
  (let ((map (make-sparse-keymap)))
    ;;(define-key map [(mouse-1)] 'ibuffer-do-sort-by-persp-name)
    map))


;; IBuffer does not provide 'ibuffer-buf', so create the
;; ibuffer-column directly so we have access to the ibuffer's persp.
(defun ibuffer-make-column-persp (buffer mark)
  "Manually written ibuffer-persp column method."
  (ibuffer-assert-ibuffer-mode)
  (let* ((p-names (mapcar 'persp-name (ibuffer-persp-buffer-in-persps buffer)))
         (p-other-names (mapcar 'persp-name (ibuffer-persp-buffer-in-other-persps buffer)))
         )
    (concat
     (if (ibuffer-persp-get-buffer buffer) "*" " ")
     (or (unless (car p-other-names)
           "")
         (unless (cdr p-other-names)
           (format "%s" (car p-other-names)))
         ;; (when (member ibuffer-persp-name p-other-names)
         ;;   (format "%s(%s)" ibuffer-persp-name (length p-other-names)))
         (format "%s(%s)" (car p-other-names) (length p-other-names))))))


(put 'ibuffer-make-column-persp 'ibuffer-column-name "Perspective")


;; Provide ourselves:
(provide 'ibuffer-persp)
;;; ibuffer-persp.el ends here
