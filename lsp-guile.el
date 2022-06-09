;;; lsp-guile.el --- lsp-mode Guile integration    -*- lexical-binding: t; -*-

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

;; LSP support for Guile.

(require 'lsp)
(require 'lsp-scheme)

;;; Code:
(defcustom lsp-scheme-guile-start-command
  "guile --r7rs"
  "Command to start guile's interpreter."
  :group 'lsp-guile
  :type 'string)

(defun lsp-scheme--guile-start (port)
  "Return list containing a command to run and its arguments based on PORT.
The command requests from a running command server (started with
 `lsp-scheme-run') an LSP server for the current scheme buffer."

  (list (or (locate-file "lsp-guile-connect" load-path)
            (locate-file "bin/lsp-guile-connect" load-path))
        (format "%d" lsp-scheme--command-port)
        (format "%d" port)
        (format "%d" lsp-scheme--lsp-err-port)
        lsp-scheme-log-level))

(defvar lsp-scheme--guile-target-dir
  "lsp-guile-server/")

(defvar lsp-scheme--guile-server-version "0.2.0")

(defun lsp-scheme--guile-ensure-server (_client callback error-callback _update?)
  "Ensure LSP Server for Guile is installed."
  (condition-case err
      (progn (lsp-scheme--install-tarball lsp-scheme--json-rpc-url
                                          lsp-scheme--guile-target-dir
                                          "scheme-json-rpc"
                                          error-callback
                                          "/guile/")
             (lsp-scheme--install-tarball lsp-scheme-server-url
                                          lsp-scheme--guile-target-dir
                                          "scheme-lsp-server"
                                          error-callback
                                          "/guile/")
             (run-with-timer
              0.0
              nil
              (lambda ()
                (let* ((buffers (buffer-list))
                       (scheme-buffers
                        (seq-filter
                         (lambda (b)
                           (eq (buffer-local-value 'major-mode b)
                               'scheme-mode))
                         buffers)))
                  (dolist (b scheme-buffers)
                    (with-current-buffer b
                      (revert-buffer nil t))))))
             (funcall callback))
    (error (funcall error-callback err))))

;;;###autoload
(defun lsp-guile ()
  "Setup and start Guile's LSP server."
  (add-to-list 'load-path
             (expand-file-name
              (concat user-emacs-directory lsp-scheme--guile-target-dir)))
  (setenv "GUILE_LOAD_COMPILED_PATH"
          (concat
           (expand-file-name
            (concat user-emacs-directory
                    (format "%s/:" lsp-scheme--guile-target-dir)))
           (expand-file-name
            (concat user-emacs-directory
                    (format "%s/lib/guile/3.0/site-ccache/:"
                            lsp-scheme--guile-target-dir)))
           (getenv "GUILE_LOAD_COMPILED_PATH")))
  (setenv "GUILE_LOAD_PATH"
          (concat
           (expand-file-name
            (concat user-emacs-directory
                    (format "%s/:" lsp-scheme--guile-target-dir)))
           (expand-file-name
            (concat user-emacs-directory
                    (format "%s/share/guile/3.0/:"
                            lsp-scheme--guile-target-dir)))
           (getenv "GUILE_LOAD_PATH")))
  (let ((client (gethash 'lsp-guile-server lsp-clients)))
    (when (and client (lsp--server-binary-present? client))
      (lsp-scheme-run "guile"))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-tcp-connection
                                   #'lsp-scheme--guile-start)
                  :major-modes '(scheme-mode)
                  :priority 1
                  :server-id 'lsp-guile-server
                  :download-server-fn #'lsp-scheme--guile-ensure-server))

(provide 'lsp-guile)
;;; lsp-guile.el ends here
