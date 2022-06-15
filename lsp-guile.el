;;; lsp-guile.el --- Guile support for lsp-mode    -*- lexical-binding: t; -*-

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

;; LSP support for Guile.

;;; URL: https://codeberg.org/rgherdt/emacs-lsp-scheme
;;; Version: 0.0.2

(require 'lsp)
(require 'lsp-scheme)

;;; Code:
(defcustom lsp-guile-start-command
  "guile --r7rs"
  "Command to start guile's interpreter."
  :group 'lsp-guile
  :type 'string)

(defvar lsp-guile--target-dir
  "lsp-guile-server/")

(defun lsp-guile--ensure-server (_client callback error-callback _update?)
  "Ensure LSP Server for Guile is installed and running."
  (condition-case err
      (progn (lsp-scheme--install-tarball lsp-scheme--json-rpc-url
                                          lsp-guile--target-dir
                                          "scheme-json-rpc"
                                          error-callback
                                          "/guile/")
             (lsp-scheme--install-tarball lsp-scheme-server-url
                                          lsp-guile--target-dir
                                          "scheme-lsp-server"
                                          error-callback
                                          "/guile/")
             (lsp-scheme)
             (run-with-timer 0.0
                             nil
                             #'lsp-scheme--restart-buffers)
             (funcall callback))
    (error (funcall error-callback err))))

(defun lsp-guile--server-installed-p ()
  "Check if Guile LSP server is installed."
  (or (executable-find "guile-lsp-server")
      (locate-file "guile-lsp-server" load-path)
      (locate-file "bin/guile-lsp-server" load-path)))

(defun lsp-guile--setup-environment ()
  "Set environment variables nedded to run local install."
  (add-to-list 'load-path
               (expand-file-name
                (concat user-emacs-directory lsp-guile--target-dir)))
  (setenv "GUILE_LOAD_COMPILED_PATH"
          (concat
           (expand-file-name
            (concat user-emacs-directory
                    (format "%s/:" lsp-guile--target-dir)))
           (expand-file-name
            (concat user-emacs-directory
                    (format "%s/lib/guile/3.0/site-ccache/:"
                            lsp-guile--target-dir)))
           (getenv "GUILE_LOAD_COMPILED_PATH")))
  (setenv "GUILE_LOAD_PATH"
          (concat
           (expand-file-name
            (concat user-emacs-directory
                    (format "%s/:" lsp-guile--target-dir)))
           (expand-file-name
            (concat user-emacs-directory
                    (format "%s/share/guile/3.0/:"
                            lsp-guile--target-dir)))
           (getenv "GUILE_LOAD_PATH"))))

;;;###autoload
(defun lsp-guile ()
  "Setup and start Guile's LSP server."
  (lsp-guile--setup-environment)
  (let ((client (gethash 'lsp-guile-server lsp-clients)))
    (when (and client (lsp--server-binary-present? client))
      (lsp-scheme-run "guile"))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   #'lsp-scheme--connect
                                   #'lsp-guile--server-installed-p)
                  :major-modes '(scheme-mode)
                  :priority 1
                  :server-id 'lsp-guile-server
                  :download-server-fn #'lsp-guile--ensure-server))

(provide 'lsp-guile)
;;; lsp-guile.el ends here
