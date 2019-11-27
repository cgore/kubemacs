;;; kubemacs.el --- Kubernetes Emacs UI

;;; Commentary:

;;; Code:

(defconst kubemacs-buffer-name "*kubemacs*"
  "This is the buffer name for the standard Kubemacs buffer, defaulting to *kubemacs*.")

(defun kubemacs-buffer ()
  "Return: The kubemacs buffer, creating it if necessary first."
  (get-buffer-create kubemacs-buffer-name))

(defun kubemacs ()
  "Kubernetes Emacs UI."
  (interactive)
  (with-current-buffer (kubemacs-buffer)
    (erase-buffer)
    (maphash (lambda (env-key env-config)
               (insert (propertize "--------------------------------------------------------------\n"
                                   'face 'bold))
               (insert (propertize (format "%s" env-key) 'face 'bold) "\n")
               (mapcar (lambda (config-key)
                         (insert "\t" (propertize (format "%s" config-key) 'face 'bold)
                                 " " (gethash config-key env-config)
                                 "\n"))
                       '(:kubectl-path :context))
               (call-process (gethash :kubectl-path env-config)
                             nil t nil
                             "--context" (gethash :context env-config)
                             "get" "namespaces"))
             kubemacs-config)))

;;; kubemacs.el ends here
