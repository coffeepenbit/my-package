;;; my-package.el --- my package -*- lexical-binding: t; -*-

;; Copyright (C) 2020  coffeepenbit

;; Author: coffeepenbit <coffeepenbit@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/olicenses/>.

;;; Commentary:

;; For all my functions that don't have a home anywhere else.

;;; Code:
(require 'my-setup)

(defgroup my-package nil
  "My package")

;;;; Navigation
(defun my-package-next-defun nil
  "Move to next defun."
  (interactive)
  ;; NOTE: end-of-defun must be called TWICE
  (end-of-defun)
  (end-of-defun)
  (beginning-of-defun))


(defun my-package-previous-defun nil
  "Move to previous defun."
  (interactive)
  (beginning-of-defun))


;;;; Buffers/Windows
(defun my-package-switch-to-minibuffer nil
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))


;;;; Blank lines
(defun my-package-get-blank-lines-above nil
  (save-excursion
    (beginning-of-line)
    (let ((nblank-lines 0))
      (while (or (looking-back "\n\n")
                 (and (eq (line-number-at-pos) 2)
                      (looking-back "\n")))
        (setq nblank-lines (1+ nblank-lines))
        (forward-line -1))
      nblank-lines)))


(defun my-package-get-blank-lines-below nil
  (save-excursion
    (let ((nblank-lines 0))
      (while (and (eq (forward-line 1) 0)
                  (my-package-at-blank-line-p))
        (setq nblank-lines (1+ nblank-lines)))
      nblank-lines)))


(defun my-package-at-blank-line-p nil
  "Return non-nil if point at a blank line."
  (and (bolp)
       (eolp)))


;;;; Misc
(defvar my-column-break
  100
  "Point where word wrapping occurs.")


(define-minor-mode my-locked-window-mode
  "Make the current window always display this buffer."
  nil " mlw" nil
  (set-window-dedicated-p (selected-window) my-locked-window-mode))

(global-set-key (kbd "C-c `") 'my-locked-window-mode)


;; (defun my-pyvenv-autoload ()
;;   "Load venv when opening projectile project."
;;   (interactive)
;;   (require 'projectile)
;;   (let* ((project_dir (projectile-project-root))
;;          (venv_dir (concat project_dir ".venv")))
;;     (if (file-directory-p venv_dir)
;;         (progn
;;           (pyvenv-activate venv_dir)
;;           (message "Activated venv_dir: %s" (venv_dir))))))


(defun my-copy-line (&optional arg)
  "Copy lines (as many as prefix ARG) in the kill ring.

- `\\[universal-argument]' to continue even if last command was not this function.

  Ease of use features:
  - Move to start of next line.
  - Appends the copy on sequential calls.
  - Use newline as last char even on the last line of the buffer.
  - If region is active, copy its lines."
  ;; TODO deal with reverse situation
  (interactive "P")
  (let* ((nlines ; Number of lines to copy if no region selected
          ;; Only relevant if prefix argument is a number and
          ;; the mark-region is NOT active
          (if
              (and (not mark-active)
                   (fixnump arg))
              arg
            1))
         (beg (if mark-active
                  (mark)
                (point)))
         (end (if mark-active
                  (point)
                (if (> nlines 0)
                    (line-end-position nlines)
                  ;; input argument of 1 for line-beginning-position yields
                  ;; beginning of current line
                  ;;
                  ;; Therefore, if you want the previous line, you would enter 0
                  (line-beginning-position (+ nlines 2))))))

    (if (eq last-command this-command)
        (if (> nlines 0)
            (kill-append (concat "\n" (buffer-substring beg end)) nil)
          (kill-append (concat (buffer-substring beg end) "\n") 'prepend))
      (copy-region-as-kill beg end))

    (if (> nlines 0)
        (goto-char (+ end 1))
      (goto-char (- end 1)))))


(defvar my-package--my-copy-lines-last-direction)


(defun my-split-window-vertically ()
  "Split Emacs window vertically."
  (interactive)
  (split-window-vertically)
  (other-window 1))


(defun my-split-window-horizontally ()
  "Split Emacs window horizontally."
  (interactive)
  (split-window-horizontally)
  (other-window 1))


(defun my-font-exists-p (font)
  "Check if FONT exists."
  (interactive)
  (if (null (x-list-fonts font)) nil t))


(defun my-relative-filename (filepath)
  "Convert absolute FILEPATH to relative filepath."
  (concat (file-name-as-directory "~") (file-relative-name filepath "~")))


(defun my-fix-mixed-line-endings nil
  "Remove ^M from end of lines."
  (interactive)
  (replace-regexp "
" "" nil nil nil nil nil))


(defun my-dired-new-file (new-file-name)
  "Create a new file from Dired mode with NEW-FILE-NAME."
  (interactive "sName of new file: ")
  (let ((new-file-command (concat "touch " new-file-name)))
    (dired-smart-shell-command new-file-command))
  (sleep-for 0.1) ; Allow time for file to be created
  (revert-buffer))


;; (defun my-current-time-microseconds ()
;;   "Return the current time formatted to include microseconds."
;;   (let* ((nowtime (current-time))
;;          (now-ms (nth 2 nowtime)))
;;     (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d]" now-ms))))


;; (defun my-add-timestamp-message (FORMAT-STRING &rest args)
;;   "Advice to run before `message' that prepends a timestamp to each message.

;; Activate this advice with:
;; (advice-add 'message :before 'my-add-timestamp-message)"
;;   (unless (string-equal FORMAT-STRING "%s%s")
;;     (let ((deactivate-mark nil)
;;           (inhibit-read-only t))
;;       (with-current-buffer "*Messages*"
;;         (goto-char (point-max))
;;         (if (not (bolp))
;;             (newline))
;;         (insert (my-current-time-microseconds) " ")))))

;; (advice-add 'message :before 'my-add-timestamp-message)


(defun my-package-verify-init nil
  "Verify my init.el file."
  (message "Verifying my init.el file")
  (my-ensure-inits-present)
  (my-ensure-no-extraneous-modular-inits))


(defun my-ensure-inits-present nil
  "Ensure init files present in init.el."
  (message "Checking for missing init files in init.el")
  (dolist (modular-init-filename (my-modular-init-file-names))
    (let* ((modular-init-file-basename (file-name-sans-extension modular-init-filename))
           (init-require-string-regexp (my-modular-init-require-regexp modular-init-file-basename))
           (nmissing-files 0))
      (unless (or (modular-init-file-name-is-bad-p modular-init-filename)
                  (init-require-string-in-init-content-p init-require-string-regexp))
        (progn
          (display-warning 'my-package (format "%s not in init.el" modular-init-file-basename))
          (setq nmissing-files (1+ nmissing-files)))))))


(defun my-modular-init-file-names nil
  "Get all of my modular init files."
  (directory-files my-setup-modular-inits-directory nil "^init-.*\.el$"))


(defun my-modular-init-require-regexp (init-file-name)
  "Create regexp from INIT-FILE-NAME."
  (format ".*\\(my-\\)?require\\(-softly\\)? '%s)" init-file-name))


(defun modular-init-file-name-is-bad-p (modular-init-filename)
  (string-match nextcloud-conflicted-copy-regexp modular-init-filename))


(defvar nextcloud-conflicted-copy-regexp (regexp-quote "conflicted copy"))


(defun init-require-string-in-init-content-p (init-require-string-regexp)
  (string-match-p init-require-string-regexp (my-init-file-content)))


(defun my-init-file-content nil
  "Get contents of my init.el file."
  (my-read-file (concat user-emacs-directory "init.el")))


(defun my-read-file (file-path)
  "Read contents in FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))


(defun my-ensure-no-extraneous-modular-inits nil
  "Ensure no extraneous inits in my init.el file."
  (message "Checking for extraneous init requires in init.el")
  (let ((modular-init-matches (my-string-find-all
                               "(\\(?:my-\\)?require\\(?:-softly\\)? 'init-\\([a-zA-Z\\-]*\\))"
                               (my-init-file-content)
                               1)))
    (dolist (match modular-init-matches)
      (unless (cl-member match (my-modular-init-file-names) :test 'string-match)
        (display-warning 'my-package (format "%s is an extraneous init modular requirement, consider
removing from init.el" match))))))


(defun my-string-find-all (regex string &optional match-group-ind)
  "Return a list of all REGEX matches in STRING.

Optionally, MATCH-GROUP-IND will select a specific subgroup."
  ;; source: http://emacs.stackexchange.com/questions/7148/get-all-regexp-matches-in-buffer-as-a-list
  (let ((pos 0)        ; string marker
        (matches ())
        (match-group-ind (or match-group-ind 0)))  ; return list
    (while (string-match regex string pos)
      (push (match-string match-group-ind string) matches)
      (setq pos (match-end 0)))
    (setq matches (reverse matches))
    matches))


(defun my-package-move-beginning-of-line (&optional ARG)
  "Move to beginning of line or first indentation.

ARG gets passed to `beginning-of-line'."
  (interactive "P")
  (if (and (eq last-command 'my-package-move-beginning-of-line)
           (not (bolp)))
      (call-interactively 'beginning-of-line ARG)
    (back-to-indentation)))


(defun my-package-insert-lines-above (&optional nlines)
  "Insert NLINES empty lines above the current line."
  (save-excursion
    (beginning-of-line)
    (open-line (or nlines 1))))


(defun my-package-insert-lines-below (&optional nlines)
  "Insert NLINES empty line below the current line."
  (save-excursion
    (end-of-line)
    (open-line (or nlines 1))))


;;;; Eldev
(defun my-package-eldev-run-test nil
  "Run project's tests and place output in coverage folder for `coverage'."
  (interactive)
  (shell-command "eldev test -U coverage/coverage.json"
                 "*Eldev Test Output*"))


;;;; Straight
(defcustom my-package-straight-push-packages nil
  "List of packages to push with `my-package-straight-push-packages'."
  :group 'my-package)


(defcustom my-package-straight-pull-packages nil
  "List of packages to push with `my-package-straight-pull-packages'."
  :group 'my-package)


(with-eval-after-load 'straight
  (defun my-package-straight-push-packages nil
    (interactive)
    (if my-package-straight-push-packages
        (dolist (package my-package-straight-push-packages)
          (message "Pushing \"%s\"" package)
          (straight-push-package package))
      (display-warning 'my-package "No packages listed in my-package-straight-push-packages")))


  (defun my-package-straight-pull-packages nil
    (interactive)
    (if my-package-straight-pull-packages
        (dolist (package my-package-straight-pull-packages)
          (message "Pulling \"%s\"" package)
          (straight-pull-package package))
      (display-warning 'my-package "No packages listed in my-package-straight-pull-packages"))))


;;;; Read only
(defun my-package-set-buffer-read-only nil
  "Set buffer to read only."
  (setq buffer-read-only t))

;;;; Provide
(provide 'my-package)
;;; my-package.el ends here
