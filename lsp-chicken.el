;;; lsp-chicken.el --- CHICKEN support for lsp-mode    -*- lexical-binding: t; -*-

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
;; Package-Requires: ((emacs "25.1") (dash "2.18.0") (f "0.20.0") (ht "2.3") (spinner "1.7.3") (markdown-mode "2.3") (lv "0.1.0"))

;;; Commentary:

;; LSP support for CHICKEN 5.

;;; URL: https://codeberg.org/rgherdt/emacs-lsp-scheme
;;; Version: 0.0.2

(require 'lsp-scheme)
(require 'seq)

;;; Code:
(defvar lsp-chicken--install-dir
  (f-join lsp-scheme-install-dir "lsp-chicken-server/"))

(defun lsp-chicken--install-egg (egg-name install-dir error-callback)
  "Ensure EGG-NAME is installed at provided INSTALL-DIR.
This function is meant to be used by lsp-mode's `lsp--install-server-internal`,
and thus calls its ERROR-CALLBACK in case something is wrong"
  (condition-case err
      (progn
        (lsp--info (format "Installing software and its dependencies..."))
        (call-process-shell-command
         (format
          "CHICKEN_INSTALL_REPOSITORY=%s CHICKEN_INSTALL_PREFIX=%s chicken-install %s"
          install-dir
          install-dir
          egg-name)
         nil
         "*Shell Command Output*"
         t)
        (lsp--info "Installation finished."))
    (error (funcall error-callback err))))

(defun lsp-chicken--server-installed-p ()
  "Check if CHICKEN LSP server is installed."
  (or (executable-find "chicken-lsp-server")
      (locate-file "chicken-lsp-server" load-path)
      (locate-file "bin/chicken-lsp-server" load-path)))

(defun lsp-chicken--ensure-server
    (_client callback error-callback _update?)
  "Ensure LSP Server for Chicken is installed and running.
This function is meant to be used by lsp-mode's `lsp--install-server-internal`,
and thus calls its CALLBACK and ERROR-CALLBACK in case something wents wrong.
_CLIENT and _UPDATE? are ignored."
  (condition-case err
      (progn (when (f-exists? lsp-chicken--install-dir)
               (f-delete lsp-chicken--install-dir t))
             (lsp-chicken--install-egg "lsp-server"
                                       lsp-chicken--install-dir
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
           lsp-chicken--install-dir
           ":"
           (shell-command-to-string
            "csi -e '(import (chicken platform)) (for-each (lambda (p) (display p) (display \":\")) (repository-path))'"))))

;;;###autoload
(defun lsp-chicken ()
  "Setup and start CHICKEN's LSP server."
  (add-to-list 'load-path
               lsp-chicken--install-dir)

  (lsp-chicken--setup-environment)
  (let ((client (gethash 'lsp-chicken-server lsp-clients)))
    (when (and client (lsp--server-binary-present? client))
      (lsp-scheme-run "chicken"))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   #'lsp-scheme--connect
                                   #'lsp-chicken--server-installed-p)
                  :major-modes '(scheme-mode)
                  :priority 1
                  :server-id 'lsp-chicken-server
                  :download-server-fn #'lsp-chicken--ensure-server))

(provide 'lsp-chicken)

;;; lsp-chicken.el ends here
