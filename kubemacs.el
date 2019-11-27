;;; kubemacs.el --- Kubernetes Emacs UI -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Christopher Mark Gore, all righs reserved.

;; Author: Christopher Mark Gore <cgore@cgore.com>
;; Version: 0.0.0
;; Keywords: Kubernetes
;; Package-Requires: ((dash "2.16.0") (dash-functional "1.2.0") (emacs "24") (ht "2.3"))

;;; Commentary:

;;; Code:

(require 'dash) ; https://github.com/magnars/dash.el
(require 'dash-functional) ; https://github.com/magnars/dash.el
(require 'ht) ; https://github.com/Wilfred/ht.el
(require 's) ; https://github.com/magnars/s.el

(defconst kubemacs-buffer-name "*kubemacs*"
  "This is the buffer name for the standard Kubemacs buffer, defaulting to *kubemacs*.")

(defun kubemacs-buffer ()
  "Return: The kubemacs buffer, creating it if necessary first."
  (get-buffer-create kubemacs-buffer-name))

(defvar kubemacs-selected-config-key
  (--> kubemacs-config ht-keys car)
  "This is the currently selected kubemacs configuration.")

(defun kubemacs-selected-config-map ()
  "Return the currently selected Kubemacs config map."
  (ht-get kubemacs-config kubemacs-selected-config-key))

(defun kubemacs-select-config ()
  (interactive)
  (let ((new-key (x-popup-menu t (list (format "Select a new Kubemacs config (currently %s):"
                                               kubemacs-selected-config-key)
                                       (cons "" (--map (cons (format "%s" it) it)
                                                       (ht-keys kubemacs-config)))))))
    (when new-key
      (setq kubemacs-selected-config-key new-key)
      (kubemacs))))

(defvar kubemacs-mode-map
  (let* ((map (make-sparse-keymap))
         (k (lambda (keys def)
              (define-key map (kbd keys) def))))
    (funcall k "C" 'kubemacs-select-config)
    map)
  "Keymap for `kubemacs-mode'.")

(define-derived-mode kubemacs-mode special-mode "Kubemacs"
  "Major Mode for use in the Kubemacs buffer."
  (setq buffer-read-only t
        truncate-lines -1
        cursor-type nil)
  (use-local-map kubemacs-mode-map))

(defun kubemacs ()
  "Kubernetes Emacs UI."
  (interactive)
  (with-current-buffer (kubemacs-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((conf (kubemacs-selected-config-map)))
      (insert (propertize "Current Kubemacs Config: " 'face 'bold)
              (format "%s" kubemacs-selected-config-key) " ")
      (insert-text-button "[change]"
                          'face 'bold
                          'help-echo "Change the currently selected Kubemacs config."
                          :follow-link t
                          'action `(lambda (button) (kubemacs-select-config)))
      (insert "\n")
      (--map (insert "\t" (propertize (format "%s" it) 'face 'bold)
                     " " (gethash it conf)
                     "\n")
             '(:kubectl-path :context))
      (insert (propertize "Namespaces:\n" 'face 'bold))
      (call-process (gethash :kubectl-path conf)
                    nil t nil
                    "--context" (gethash :context conf)
                    "get" "namespaces"))
    (setq buffer-read-only t)
    (kubemacs-mode)))

;;; kubemacs.el ends here
