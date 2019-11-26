;;; kubemacs.el --- Kubernetes Emacs UI

;;; Commentary:

;;; Code:

(defvar kubemacs-kubectl-path "kubectl"
  "Path of the kubectl executable to use.")

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
    (call-process kubemacs-kubectl-path nil t nil "version")
    ))

;;; kubemacs.el ends here
