;;; emacs-qle.el --- Major mode for editing QLE log files -*- lexical-binding: t; -*-

;;; Commentary:
;; emacs-qle-mode is a major mode for working with Quick Log Entry (.qle) files.
;; It supports automatic file prompting, side-by-side windows,
;; live updating of a read-only contact buffer, and a minor mode
;; for live contact logging with features like auto-save on RET.

;;; Code:

(require 'cl-lib)

;; ----------------------------------------
;; Customization group
;; ----------------------------------------

(defgroup emacs-qle nil
  "Major mode for Quick Log Entry files."
  :prefix "emacs-qle-"
  :group 'applications)

;; ----------------------------------------
;; Helper functions
;; ----------------------------------------

(defun emacs-qle--maybe-switch-to-qle-file ()
  "Ensure visiting a .qle file; if not, prompt for one."
  (unless (and (buffer-file-name)
               (string-suffix-p ".qle" (buffer-file-name) t))
    (let ((file (read-file-name "Open QLE file: " nil nil t nil
                                (lambda (f) (string-suffix-p ".qle" f t)))))
      (find-file file)))
  t)

(defun emacs-qle--setup-windows ()
  "Split the window and set up the contacts display."
  (let ((right (get-buffer-create "*QLE Contacts*"))
        (left (current-buffer)))
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (switch-to-buffer right)
    (read-only-mode 1)
    (other-window -1)
    (switch-to-buffer left)))

(defun emacs-qle--update-contacts-buffer ()
  "Refresh the contents of the contacts buffer from the current file, adding a table header and sorting by date/time."
  (let ((file buffer-file-name)
        (contacts-buffer (get-buffer-create "*QLE Contacts*"))
        (date-regex "\\(\\d{8}\\)")         ; Matches the date (YYYYMMDD)
        (time-regex "\\(\\d{4}\\)")         ; Matches the time (HHMM)
        (band-regex "\\([0-9]+M\\)")        ; Matches the band (e.g., 20m)
        (mode-regex "\\(CW\\|SSB\\|FT8\\)") ; Matches the mode
        (rst-regex "\\(\\d{3}\\)")          ; Matches the RST (e.g., 599)
        (callsign-regex "\\([A-Za-z0-9]+\\)") ; Matches callsign
        (power-regex "\\(\\d+W\\|\\d+\\)")  ; Matches power (e.g., 100W or just 100)
        )
    (when (and file (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((lines (split-string (buffer-string) "\n"))
              (contact-lines '()))  ;; We’ll accumulate formatted lines here

          ;; Process the contact lines, skipping empty lines
          (dolist (line lines)
            (unless (string-match-p "^\\s-*$" line)
              (let* ((date (if (string-match date-regex line) (match-string 1 line) nil))
                     (time (if (string-match time-regex line) (match-string 1 line) nil))
                     (band (if (string-match band-regex line) (match-string 1 line) nil))
                     (mode (if (string-match mode-regex line) (match-string 1 line) nil))
                     (rst-sent (if (string-match rst-regex line) (match-string 1 line) nil))
                     (rst-received (if (string-match rst-regex line) (match-string 1 line) nil))
                     (callsign (if (string-match callsign-regex line) (match-string 1 line) nil))
                     (power (if (string-match power-regex line) (match-string 1 line) nil)))

                ;; Debug: Print values of captured groups
                (message "Parsed values: date=%s, time=%s, band=%s, mode=%s, rst-sent=%s, rst-received=%s, callsign=%s, power=%s"
                         date time band mode rst-sent rst-received callsign power)

                ;; Add formatted contact line to the list
                (setq contact-lines
                      (append contact-lines
                              (list (format "%-10s %-6s %-7s %-6s %-11s %-15s %-10s %-6s"
                                            (or date "N/A")
                                            (or time "N/A")
                                            (or band "N/A")
                                            (or mode "N/A")
                                            (or rst-sent "N/A")
                                            (or rst-received "N/A")
                                            (or callsign "N/A")
                                            (or power "N/A"))))))))

          ;; Sort the contact lines by date and time
          (setq contact-lines (sort contact-lines
                                     (lambda (a b)
                                       (or (string< (substring a 0 8) (substring b 0 8))
                                           (and (string= (substring a 0 8) (substring b 0 8))
                                                (string< (substring a 9 13) (substring b 9 13)))))))

          ;; Insert the header and sorted contact data into the contacts buffer
          (with-current-buffer contacts-buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              ;; Insert the header first
              (insert "Date       Time   Band    Mode    RST Sent    RST Received    Callsign    Power\n")
              ;; Insert the sorted contact data
              (dolist (line contact-lines)
                (insert line "\n"))
              (goto-char (point-min))
              (read-only-mode 1))))))))

(defun emacs-qle--on-save ()
  "Hook to update contact buffer after saving."
  (when (derived-mode-p 'emacs-qle-mode)
    (emacs-qle--update-contacts-buffer)))

(defun emacs-qle-newline-and-save ()
  "Insert a newline and save the buffer."
  (interactive)
  (newline)
  (save-buffer))

(defun emacs-qle-insert-date ()
  "Insert the current date at point in YYYYMMDD format."
  (interactive)
  (insert (format-time-string "%Y%m%d")))

(defun emacs-qle-insert-utc-time ()
  "Insert the current UTC time in HHMM format."
  (interactive)
  (insert (format-time-string "%H%M" (current-time) t)))

(defun emacs-qle-live-mode-insert-date-time-and-newline ()
  "Insert the UTC date and time at the start of the line, then create a newline.
Also save the file after insertion."
  (interactive)
  (beginning-of-line)
  (insert (format-time-string "%Y%m%d %H%M " (current-time) t))
  (end-of-line)
  (newline)
  (save-buffer))

;; ----------------------------------------
;; Minor mode: emacs-qle-live-mode
;; ----------------------------------------

(defvar emacs-qle-live-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'emacs-qle-live-mode-insert-date-time-and-newline)
    map)
  "Keymap for `emacs-qle-live-mode`.")

(define-minor-mode emacs-qle-live-mode
  "Minor mode for live contact logging in QLE files."
  :lighter " Live"
  :keymap emacs-qle-live-mode-map
  :group 'emacs-qle
  (if emacs-qle-live-mode
      (message "Live mode enabled")
    (message "Live mode disabled")))

;; ----------------------------------------
;; Major mode: emacs-qle-mode
;; ----------------------------------------

(defvar emacs-qle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'emacs-qle-newline-and-save)
    (define-key map (kbd "C-i d") #'emacs-qle-insert-date)
    (define-key map (kbd "C-i t") #'emacs-qle-insert-utc-time)
    map)
  "Keymap for `emacs-qle-mode`.")

;;;###autoload
(define-derived-mode emacs-qle-mode fundamental-mode "emacs-qle"
  "Major mode for editing QLE log files."
  (use-local-map emacs-qle-mode-map)
  (setq font-lock-defaults '(emacs-qle-font-lock-keywords))
  (when (emacs-qle--maybe-switch-to-qle-file)
    (emacs-qle--setup-windows)
    (emacs-qle--update-contacts-buffer)))

(add-hook 'emacs-qle-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'emacs-qle--on-save nil t)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qle\\'" . emacs-qle-mode))

(provide 'emacs-qle)

;;; emacs-qle.el ends here
