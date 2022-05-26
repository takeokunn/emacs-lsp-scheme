;;; lsp-guile.el --- lsp-mode Guile integration    -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Ricardo Gabriel Herdt

;; Author: Ricardo Gabriel Herdt
;; Keywords: languages

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

(require 'lsp-scheme)

(defcustom lsp-scheme-guile-start-command
  "guile --r7rs"
  "Command to start guile's interpreter."
  :group 'lsp-guile
  :type 'string)

(defun lsp-scheme--guile-start (port)
  (list (locate-file "bin/lsp-guile-connect-to-server" load-path)
        (format "%d" lsp-scheme-command-port)
        (format "%d" port)))

(defvar lsp-scheme--guile-target-dir
  "guile-lsp-server/")

(defun lsp-scheme--guile-ensure-server (_client callback error-callback _update?)
  "Ensure LSP Server for Guile is installed."
  (condition-case err
      (progn (lsp-scheme--install-tarball lsp-scheme-json-rpc-root-url
                                    "guile-json-rpc-0.2.0.tar.gz"
                                    lsp-scheme--guile-target-dir)
             (lsp-scheme--install-tarball lsp-scheme-lsp-server-root-url
                                          "guile-lsp-server-0.2.0.tar.gz"
                                          lsp-scheme--guile-target-dir)
             (funcall callback))
    (error (funcall error-callback err))))


(add-to-list 'load-path (concat user-emacs-directory lsp-scheme--guile-target-dir))
(setenv "GUILE_LOAD_COMPILED_PATH"
        (concat
         (expand-file-name (concat user-emacs-directory (format "%s/:" lsp-scheme--guile-target-dir)))
         (expand-file-name (concat user-emacs-directory (format "%s/lib/guile/3.0/site-ccache/:"
                                                                lsp-scheme--guile-target-dir)))
         (getenv "GUILE_LOAD_COMPILED_PATH")))
(setenv "GUILE_LOAD_PATH"
        (concat
         (expand-file-name (concat user-emacs-directory (format "%s/:" lsp-scheme--guile-target-dir)))
         (expand-file-name (concat user-emacs-directory (format "%s/share/guile/3.0/:"
                                                                lsp-scheme--guile-target-dir)))
         (getenv "GUILE_LOAD_PATH")))

(eval-after-load 'lsp-guile
  '(lsp-scheme-ensure-running))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-tcp-connection
                                   #'lsp-scheme--guile-start)
                  :major-modes '(scheme-mode)
                  :priority 1
                  :server-id 'guile-lsp-server
                  :download-server-fn #'lsp-scheme--guile-ensure-server))

(provide 'lsp-guile)
;;; lsp-guile.el ends here
