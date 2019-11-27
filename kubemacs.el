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
  nil
  "This is the currently selected kubemacs configuration.")

(defun kubemacs-select-config ()
  (interactive)
  (setq kubemacs-selected-config-key
        (x-popup-menu t (list (format "Select a new Kubemacs config (currently %s):"
                                      kubemacs-selected-config-key)
                              (cons "Kubemacs Config Keys"
                                    (mapcar (lambda (config-key)
                                              (cons (format "%s" config-key) config-key))
                                            (ht-keys kubemacs-config)))))))

(maphash (lambda (config-key config-map)
           (cons (format "%s" :foo) :foo))
         kubemacs-config)

(defvar kubemacs-mode-map
  (let* ((map (make-sparse-keymap))
         (k (lambda (keys def)
              (define-key map (kbd keys) def))))
    (funcall k "C" 'kubemacs-select-config)
    map))

(define-derived-mode kubemacs-mode special-mode "Kubemacs"
  "Major Mode for use in the Kubemacs buffer."
  (use-local-map kubemacs-mode-map))

(defun kubemacs ()
  "Kubernetes Emacs UI."
  (interactive)
  (with-current-buffer (kubemacs-buffer)
    (erase-buffer)
    (maphash (lambda (env-key env-config)
               (insert (propertize (format "%s" env-key) 'face 'bold) "\n")
               (insert (propertize "--------------------------------------------------------------\n"
                                   'face 'bold))
               (mapcar (lambda (config-key)
                         (insert "\t" (propertize (format "%s" config-key) 'face 'bold)
                                 " " (gethash config-key env-config)
                                 "\n"))
                       '(:kubectl-path :context))
               (insert (propertize "get namespaces\n" 'face 'italic))
               (call-process (gethash :kubectl-path env-config)
                             nil t nil
                             "--context" (gethash :context env-config)
                             "get" "namespaces"))
             kubemacs-config)
    (kubemacs-mode)))

;;; kubemacs.el ends here
