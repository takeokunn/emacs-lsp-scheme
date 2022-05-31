;;; lsp-chicken.el --- lsp-mode CHICKEN integration    -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Ricardo Gabriel Herdt

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
(require 'seq)

(defcustom lsp-scheme-chicken-connect-command
  (locate-file "lsp-chicken-connect" load-path)
  "Command to spawn a new LSP connection."
  :type 'string
  :group 'lsp-scheme
  :package-version '(lsp-scheme . "0.0.1"))

(defun lsp-scheme--chicken-start (port)
  (list (or (locate-file "lsp-chicken-connect" load-path)
            (locate-file "bin/lsp-chicken-connect" load-path))
        (format "%d" lsp-scheme--command-port)
        (format "%d" port)))

(defvar lsp-scheme--chicken-target-dir
  "lsp-chicken-server/")

(defvar lsp-scheme--chicken-server-version "0.2.0")

(defun lsp-scheme--chicken-install-egg (url target-name error-callback)
  "Ensure EGG is installed at provided target."
  (condition-case err
      (let* ((tmp-dir (make-temp-file "lsp-chicken-install" t))
             (tarball-name (file-name-nondirectory url))
             (download-path (concat tmp-dir "/"  tarball-name))
             (decompressed-path
              (concat tmp-dir "/"
                      (lsp-scheme--get-root-name-from-tarball
                       tarball-name)))
             (target-dir (concat user-emacs-directory target-name)))
        (when (f-exists? download-path)
          (lsp--info "Deleting previously installed software at %s..." download-path)
          (f-delete download-path))
        (lsp--info "Starting to download %s to %s..." url download-path)
        (url-copy-file url download-path)
        (lsp--info "Finished downloading %s..." download-path)
        (lsp--info "Uncompressing file %s into %s..."
                   download-path
                   tmp-dir)
        (lsp-scheme--untar download-path tmp-dir)
        (lsp--info "Switching to installation directory %s..."
                   decompressed-path)
        (lsp--info (format "Building software..."))
        (call-process-shell-command (format
                                     "cd %s && CHICKEN_INSTALL_REPOSITORY=%s CHICKEN_INSTALL_PREFIX=%s chicken-install && cd -"
                                     decompressed-path
                                     (expand-file-name target-dir)
                                     (expand-file-name target-dir))
                                    nil
                                    "*Shell Command Output*"
                                    t)
        (lsp--info "Installation finished."))
    (error (funcall error-callback err))))

(defun lsp-scheme--chicken-ensure-server (_client callback error-callback _update?)
  "Ensure LSP Server for Chicken is installed."
  (message "lsp-scheme--chicken-ensure-server called")
  
  (condition-case err
      (progn (when (f-exists? lsp-scheme--chicken-target-dir)
               (f-delete lsp-scheme--chicken-target-dir t))
             (lsp-scheme--chicken-install-egg
              lsp-scheme--json-rpc-url
              lsp-scheme--chicken-target-dir
              error-callback)
             (lsp-scheme--chicken-install-egg
              lsp-scheme-server-url
              lsp-scheme--chicken-target-dir
              error-callback)
             (lsp-chicken)
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


(defun lsp-chicken ()
  "Setup and start Guile's LSP server."
  (add-to-list 'load-path
               (concat user-emacs-directory
                       lsp-scheme--chicken-target-dir))

  (setenv "CHICKEN_REPOSITORY_PATH"
          (concat
           (expand-file-name
            (concat user-emacs-directory lsp-scheme--chicken-target-dir))
           ":"
           (shell-command-to-string
            "csi -e '(import (chicken platform)) (for-each (lambda (p) (display p) (display \":\")) (repository-path))'")))
  (let ((client (gethash 'lsp-chicken-server lsp-clients)))
    (when (and client (lsp--server-binary-present? client))
      (lsp-scheme-run "chicken"))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-tcp-connection
                                   #'lsp-scheme--chicken-start)
                  :major-modes '(scheme-mode)
                  :priority 1
                  :server-id 'lsp-chicken-server
                  :download-server-fn #'lsp-scheme--chicken-ensure-server))

(provide 'lsp-chicken)

;;; lsp-chicken.el ends here
