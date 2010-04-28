;;; popup-global-mark-ring.el --- Jumping Interactively through global mark ring

;; Copyright (C) 2010  whitypig

;; Author: whitypig <whitypig@gmail.com>
;; Keywords: lisp popup
;; Version: 0.1

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
;;
;; This software is greatly inspired by popup-kill-ring
;; and created thanks to popup.el,
;; so I'd like to express my gratitude to its authors.
;;
;; Apparently, there still are many bugs, so please be generous.

;;; Requirement:
;;
;; * popup.el   http://github.com/m2ym/auto-complete

;;; Installation:
;;
;; Copy popup-global-mark-ring.el to your load-path and add to your .emacs:
;;
;; (require 'popup-global-mark-ring)
;; 
;; To use popup-global-mark-ring, do M-x popup-global-mark-ring.
;; Or assign the key whatever you want to to 'popup-global-mark-ring.
;; For example,
;; (global-set-key "\C-c\C-g" 'popup-global-mark-ring)

;;; Code:

(require 'popup)

;;; Variables:

(defvar popup-global-mark-ring-menu-width 70
  "Width of popup menu")

;;; Functions:

(defun popup-global-mark-ring ()
  "Show global mark ring menu and go to the place selected."
  (interactive)
  (let ((item nil)
        (num 0))
    ;; Show menu and get selection
    (setq item (and global-mark-ring (popup-menu* (popup-global-mark-ring-menu)
                            :scroll-bar t
                            :margin t
                            :width popup-global-mark-ring-menu-width)))
    (when item
      (when (string-match "^\\([0-9]+\\):.*" item)
        (setq num (1- (string-to-number (match-string 1 item))))
        (setq marker (nth num global-mark-ring))
        ;; Make current-location maker from the current location
        ;; and push it into mark-ring if it is not
        (setq current-marker (point-marker))
        (unless (and (marker-position current-marker) (member current-marker global-mark-ring))
          (push-mark))
        ;; now, switch to and go to the place specified by the marker
        (switch-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker))))))

;; TODO
(defun popup-global-mark-ring-current ())

;; TODO
(defun popup-global-mark-ring-next ())

;; TODO
(defun popup-global-mark-ring-prev ())

(defun popup-global-mark-ring-menu ()
  "Return the list of lines with each line containing marker info.
Iterating `global-mark-ring', make and return a list consisting of
marker information that can be acquired from each element in `global-mark-ring'"
  (interactive)
  (let ((ret nil)
        (i 1))
    (dolist (elt global-mark-ring)
      (let ((pos (marker-position elt))
            (bufname (buffer-name (marker-buffer elt)))
            (linenum 0)
            (start 0)
            (end 0))
        ;; exclude #<marker in no buffer>
        (when (and pos bufname)
          (save-excursion
            (set-buffer bufname)
            (setq linenum (line-number-at-pos pos))
            (goto-char pos)
            (beginning-of-line)
            (setq start (point))
            (end-of-line)
            (setq end (point))
            (add-to-list 'ret
                         (format "%d:(%s:%d): %s"
                                 i bufname linenum
                                 (replace-regexp-in-string "^[ 	]+" ""
                                                           (buffer-substring-no-properties start end)))
                         t))
          (setq i (1+ i)))))
      ret))

(provide 'popup-global-mark-ring)
;;; popup-global-mark-ring.el ends here
