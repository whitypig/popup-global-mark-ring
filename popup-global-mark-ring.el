(require 'popup)

(defvar popup-global-mark-ring-menu-width 70
  "Width of popup menu")

(defun popup-global-mark-ring ()
  "Show global mark ring menu and go to the place selected."
  (interactive)
  (let ((item nil)
        (num 0))
    ;; Show menu and get selection
    (setq item (popup-menu* (popup-global-mark-ring-menu)
                            :scroll-bar t
                            :margin t
                            :width popup-global-mark-ring-menu-width))
    (when item
      (when (string-match "^\\([0-9]+\\):.*" item)
        (setq num (1- (string-to-number (match-string 1 item))))
        (setq marker (nth num global-mark-ring))
        ;; Make current-location maker from the current location
        ;; and push it into mark-ring if it is not
        (setq current-marker (point-marker))
        (unless (and (marker-position current-marker) (member current-marker global-mark-ring))
          (push-mark))
        ;; now, switch to the buffer and go to the pos specified by the marker
        (switch-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker))))))

;;  test code
(ignore-errors
  (popup-menu* (popup-global-mark-ring) :margin t :scroll-bar t))

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
  (let ((ret nil))
    (dotimes (i (length global-mark-ring))
      (let ((pos (marker-position (nth i global-mark-ring)))
            (bufname (buffer-name (marker-buffer (nth i global-mark-ring))))
            (linenum 0)
            (start 0)
            (end 0))
        (save-excursion
          (set-buffer bufname)
          (setq linenum (line-number-at-pos pos))
          (goto-char pos)
          (beginning-of-line)
          (setq start (point))
          (end-of-line)
          (setq end (point))
          (add-to-list 'ret
                       (format "%d:(%s:%d):%s"
                               (1+ i) bufname linenum
                               (buffer-substring-no-properties start end)) t))))
    ret))
