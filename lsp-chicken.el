;;; lsp-chicken.el --- lsp-mode CHICKEN integration    -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Ricardo Gabriel Herdt

;; Author: Ricardo Gabriel Herdt
;; Keywords: languages, lisp, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>

;; Keywords: lsp, languages

;;; Commentary

;; LSP support for CHICKEN 5.

;;; Version: 0.0.1

(require 'lsp-scheme)
(require 'seq)

;;; Code:
(defvar lsp-scheme--chicken-target-dir
  "lsp-chicken-server/")

(defun lsp-scheme--chicken-install-egg (egg-name target-name error-callback)
  "Ensure EGG-NAME is installed at provided TARGET-NAME."
  (condition-case err
      (let ((target-dir (concat user-emacs-directory target-name)))
        (lsp--info (format "Installing software and its dependencies..."))
        (call-process-shell-command
         (format
          "CHICKEN_INSTALL_REPOSITORY=%s CHICKEN_INSTALL_PREFIX=%s chicken-install %s"
          (expand-file-name target-dir)
          (expand-file-name target-dir)
          egg-name)
         nil
         "*Shell Command Output*"
         t)
        (lsp--info "Installation finished."))
    (error (funcall error-callback err))))

(defun lsp--chicken-server-installed-p ()
  "Check if CHICKEN LSP server is installed."
  (or (executable-find "chicken-lsp-server")
      (locate-file "chicken-lsp-server" load-path)
      (locate-file "bin/chicken-lsp-server" load-path)))

(defun lsp-scheme--chicken-ensure-server
    (_client callback error-callback _update?)
  "Ensure LSP Server for Chicken is installed and running."
  (condition-case err
      (progn (when (f-exists? lsp-scheme--chicken-target-dir)
               (f-delete lsp-scheme--chicken-target-dir t))
             (lsp-scheme--chicken-install-egg "lsp-server"
                                              lsp-scheme--chicken-target-dir
                                              error-callback)
             (lsp-scheme)
             (run-with-timer 0.0
                             nil
                             #'lsp-scheme--restart-buffers)
             (funcall callback))
    (error (funcall error-callback err))))

(defun lsp-chicken--setup-environment ()
  "Set environment variables nedded to run local install."
  (setenv "CHICKEN_REPOSITORY_PATH"
          (concat
           (expand-file-name
            (concat user-emacs-directory lsp-scheme--chicken-target-dir))
           ":"
           (shell-command-to-string
            "csi -e '(import (chicken platform)) (for-each (lambda (p) (display p) (display \":\")) (repository-path))'"))))

;;;###autoload
(defun lsp-chicken ()
  "Setup and start CHICKEN's LSP server."
  (add-to-list 'load-path
               (concat user-emacs-directory
                       lsp-scheme--chicken-target-dir))

  (lsp-chicken--setup-environment)
  (let ((client (gethash 'lsp-chicken-server lsp-clients)))
    (when (and client (lsp--server-binary-present? client))
      (lsp-scheme-run "chicken"))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   #'lsp-scheme--connect
                                   #'lsp--chicken-server-installed-p)
                  :major-modes '(scheme-mode)
                  :priority 1
                  :server-id 'lsp-chicken-server
                  :download-server-fn #'lsp-scheme--chicken-ensure-server))

(provide 'lsp-chicken)

;;; lsp-chicken.el ends here
