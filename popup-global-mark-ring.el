(require 'popup)

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
                       (format "%d:(%d:%s):%s"
                               (1+ i) linenum bufname
                               (buffer-substring-no-properties start end)) t))))
    ret))

;;  test code
(popup-menu* (popup-global-mark-ring-menu) :margin t :scroll-bar t)

(defun popup-global-mark-ring-current ())

(defun popup-global-mark-ring-next ())

(defun popup-global-mark-ring-prev ())
