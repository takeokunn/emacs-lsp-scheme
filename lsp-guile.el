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
;;; Package-Version: 0.0.2

(require 'lsp)
(require 'lsp-scheme)

;;; Code:

(defvar lsp-guile--install-dir
  (f-join lsp-server-install-dir "lsp-guile-server/"))

(defun lsp-guile--make-install (decompressed-path)
  "Install automake based project at DECOMPRESSED-PATH."
  (let ((cmd (format
              "cd %s && ./configure --prefix=%s && make && make install && cd -"
              decompressed-path
              (f-join lsp-guile--install-dir))))
    (message cmd)
    (lsp--info "Building software...")
    (call-process-shell-command cmd
                                nil
                                "*Shell command output*"
                                t)))

(defun lsp-guile--ensure-server (_client callback error-callback _update?)
  "Ensure LSP Server for Guile is installed and running.
This function is meant to be used by lsp-mode's `lsp--install-server-internal`,
and thus calls its CALLBACK and ERROR-CALLBACK in case something wents wrong.
If a server is already installed, reinstall it.  _CLIENT and _UPDATE? are
ignored."
  (condition-case err
      (let ((tmp-dir (make-temp-file "lsp-scheme-install" t)))
        (f-delete lsp-guile--install-dir t)
        (mkdir lsp-guile--install-dir t)

        (lsp-download-install
         (lambda ()
           (lsp-guile--make-install
            (f-join tmp-dir
                    "scheme-json-rpc"
                    "guile"))
           (lsp-download-install
            (lambda ()
              (lsp-guile--make-install
               (f-join tmp-dir
                       "scheme-lsp-server"
                       "guile"))
              (lsp-scheme)
              (run-with-timer 0.0
                              nil
                              #'lsp-scheme--restart-buffers)
              (funcall callback))
                                 error-callback
                                 :url lsp-scheme-server-url
                                 :decompress :zip
                                 :store-path (f-join tmp-dir "scheme-lsp-server")))
                              error-callback
                              :url lsp-scheme--json-rpc-url
                              :decompress :zip
                              :store-path (f-join tmp-dir "scheme-json-rpc")))
    (error (funcall error-callback err))))

(defun lsp-guile--server-installed-p ()
  "Check if Guile LSP server is installed."
  (or (executable-find "guile-lsp-server")
      (locate-file "guile-lsp-server" load-path)
      (locate-file "bin/guile-lsp-server" load-path)))

(defun lsp-guile--setup-environment ()
  "Set environment variables nedded to run local install."
  (add-to-list 'load-path
               lsp-guile--install-dir)
  (setenv "GUILE_LOAD_COMPILED_PATH"
          (concat
           (f-join lsp-guile--install-dir ":")
           (f-join lsp-guile--install-dir
                   "lib/guile/3.0/site-ccache/:")
           (getenv "GUILE_LOAD_COMPILED_PATH")))
  (setenv "GUILE_LOAD_PATH"
          (concat
           (f-join lsp-guile--install-dir ":")
           (f-join lsp-guile--install-dir
                   "share/guile/3.0/:")
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
