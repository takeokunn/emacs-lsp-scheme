;;; lsp-scheme.el --- Scheme support for lsp-mode    -*- lexical-binding: t; -*-

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

;; Author: Ricardo G. Herdt <r.herdt@posteo.de>
;; Keywords: languages, lisp, tools
;; Package-Version: 20220609.2002
;; Package-Requires: ((emacs "25.1") (dash "2.18.0") (f "0.20.0") (ht "2.3") (spinner "1.7.3") (markdown-mode "2.3") (lv "0.1.0"))

;;; URL: https://codeberg.org/rgherdt/emacs-lsp-scheme
;;; Version: 0.0.2

;;; Commentary:

;; Client for the Scheme LSP server.

;;; Code:

(require 'lsp-mode)
(require 'comint)
(require 'cmuscheme)

(defgroup lsp-scheme-lsp-server nil
  "LSP support for Scheme, using scheme-lsp-server"
  :group 'lsp-mode
  :link '(url-link "https://gitlab.com/rgherdt/scheme-lsp-server"))

(defcustom lsp-scheme-implementation "guile"
  "Scheme implementation to be used. Supported options: guile, chicken."
  :type 'string
  :group 'lsp-scheme
  :package-version '(lsp-scheme . "0.0.1"))

(defcustom lsp-scheme-log-level "error"
  "Log level verbosity.  One of \"error\", \"warning\", \"info\" or \"debug\"."
  :type 'string
  :group 'lsp-scheme
  :package-version '(lsp-scheme . "0.0.1"))

(defcustom lsp-scheme-chicken-start-command
  "csi -R r7rs"
  "Command to start chicken's interpreter."
  :group 'lsp-scheme
  :type 'string)

(defcustom lsp-scheme-guile-start-command
  "guile --r7rs"
  "Command to start guile's interpreter."
  :group 'lsp-scheme
  :type 'string)


(defcustom lsp-scheme-spawner-port
  "Starting port that spawner server will listen to.
   This extensions relies on a 'spawner' server. It is basically a server that
listens on this port and spawns LSP servers upon each incoming connection.
In case this port is used, the client will try subsequent ports.
"
  6251
  :group 'lsp-scheme
  :type 'integer)

(defconst lsp-scheme--json-rpc-version
  "master"
  "Version of JSON-RPC implementation used.")

(defconst lsp-scheme--lsp-server-version
  "master"
  "Version of LSP Server implementation used.")

(defvar lsp-scheme--json-rpc-url
  (format "https://codeberg.org/rgherdt/scheme-json-rpc/archive/%s.tar.gz"
          lsp-scheme--json-rpc-version)
  "Path to JSON-RPC library.")

(defcustom lsp-scheme-server-url
  (format "https://codeberg.org/rgherdt/scheme-lsp-server/archive/%s.tar.gz"
          lsp-scheme--lsp-server-version)
  "Path to Scheme's LSP server."
  :type 'string
  :group 'lsp-scheme
  :package-version '(lsp-scheme . "0.0.1"))


(defconst lsp-scheme-ext-untar-script "tar -xzvf %s -C %s"
  "Script to decompress tar.gz tarballs.
The script should take two arguments: the path to the tarball and the target
directory to decompress the tarball into.")

(defcustom lsp-scheme-untar-script
  (cond ((executable-find "tar") lsp-scheme-ext-untar-script)
        (t nil))
  "External script to decompress a tar.gz tarball.
Should be a format string with one argument for the file to be decompressed in
place."
  :group 'lsp-scheme
  :type 'string
  :package-version '(lsp-scheme . "0.0.1"))

(defun lsp-scheme--connect ()
  "Return list containing a command to run and its arguments based on PORT.
The command requests from a running command server (started with
 `lsp-scheme-run') an LSP server for the current scheme buffer."
  (list (locate-file "lsp-server-connect.sh" load-path)
        (format "%d" lsp-scheme-spawner-port)))

(defun lsp-scheme--untar (tar-file target-dir)
  "Decompress tar.gz tarball (TAR-FILE) into TARGET-DIR.
Uses command defined in `lsp-scheme-untar-script'."
  (unless lsp-scheme-untar-script
    (error "Unable to find `tar' on the path, please either customize
           `lsp-scheme--untar-script' or manually decompress %s"
           tar-file))
  (call-process-shell-command
   (format lsp-scheme-untar-script tar-file target-dir)
   nil
   "*Shell Command Output*"
   t))

(defun lsp-scheme--install-tarball
    (url target-name project-name error-callback &optional subdir)
  "Ensure tarball at URL is installed at provided TARGET-NAME.
PROJECT-NAME should match the name of the uncompressed tarball.  In case the
installer is not present in the root directory of the uncompressed
tarball, SUBDIR can be used to provide a relative directory containing the
Makefile."
  (condition-case err
      (let* ((tmp-dir (make-temp-file "lsp-scheme-install" t))
             (tarball-name (file-name-nondirectory url))
             (download-path (concat tmp-dir "/"  project-name "-" tarball-name))
             (decompressed-path
              (concat tmp-dir "/"
                      project-name
                      (if subdir
                          (concat "/" subdir)
                        "/")))
             (target-dir (concat user-emacs-directory target-name)))
        (when (f-exists? download-path)
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
        (lsp--info "Building software...")
        (let ((cmd (format
                    "cd %s && ./configure --prefix=%s && make && make install && cd -"
                    decompressed-path
                    (expand-file-name target-dir))))
          (message cmd)
          (lsp--info "Building software...")
          (call-process-shell-command cmd
                                      nil
                                      "*Shell command output*"
                                      t))

        (lsp--info "Installation finished."))
    (error (funcall error-callback err))))

(defun lsp-scheme--select-start-command (implementation)
  "Select a command to launch an interpreter for the selected IMPLEMENTATION."
  (cond ((string-equal implementation "chicken")
         lsp-scheme-chicken-start-command)
        ((string-equal implementation "guile")
         lsp-scheme-guile-start-command)
        (t (error "Implementation not supported: %s" implementation))))

;;;###autoload
(defun lsp-scheme ()
  "Setup and start Scheme's LSP server."
  ;; TODO: any better idea to deal with this circular dependency?
  ;; I could create yet another package with code needed by
  ;; implementation-specific parts, but I'm not sure it's worth the
  ;; trouble. Flycheck complains about missing definitions of lsp-chicken
  ;; and lsp-guile though.
  (cond ((equal lsp-scheme-implementation "chicken")
         (require 'lsp-chicken)
         (lsp-chicken))
        ((equal lsp-scheme-implementation "guile")
         (require 'lsp-guile)
         (lsp-guile))
        (t (error (format "Implementation not supported: %s"
                          lsp-scheme-implementation)))))

(defun lsp-scheme--install-lsp-server-spawn (target-dir)
  "Copy script lsp-server-connect.sh to TARGET-DIR."
  (let* ((source-path
          (locate-file "scripts/lsp-server-connect.sh" load-path))
         (target-path (concat user-emacs-directory
                              target-dir
                              "lsp-server-connect.sh")))
    (lsp--info "Copying %s to %s..." source-path target-path)
    (copy-file source-path target-path)))


(defun lsp-scheme--restart-buffers ()
  "Restart lsp-scheme buffers."
  (let* ((buffers (buffer-list))
            (scheme-buffers
             (seq-filter
              (lambda (b)
                (eq (buffer-local-value 'major-mode b)
                    'scheme-mode))
              buffers)))
       (dolist (b scheme-buffers)
         (with-current-buffer b
           (revert-buffer nil t)))))

(defun lsp-scheme-run (implementation)
  "Start the selected Scheme IMPLEMENTATION.
A REPL is opened in an *lsp-scheme* buffer, and a spawner server is launched
in the same instance, which spwans LSP servers for each incoming connection."
  (interactive "sScheme implementation: \n")
  (let ((cmd (lsp-scheme--select-start-command implementation)))
    (when (not (comint-check-proc "*lsp-scheme*"))
      (let ((cmdlist (split-string-and-unquote cmd))
            (port-num (lsp--find-available-port
                       "localhost"
                       lsp-scheme-spawner-port)))
        (setq lsp-scheme-spawner-port port-num)
        (apply 'make-comint "lsp-scheme"
               (car cmdlist)
               nil
               (cdr cmdlist))

        (comint-send-string
         "*lsp-scheme*"
         (format
          "(import (lsp-server))
           (parameterize ((lsp-server-log-level '%s))
              (lsp-spawner-start %s))"
           lsp-scheme-log-level
           lsp-scheme-spawner-port))

        (run-with-timer
         0.1
         nil
         (lambda ()
           (comint-send-string
            "*lsp-scheme*"
            "#t\n")))))

    (setq scheme-program-name cmd)
    (setq scheme-buffer "*lsp-scheme*")
    (or (display-buffer-reuse-window (get-buffer "*lsp-scheme*") '())
        (display-buffer "*lsp-scheme*" '(display-buffer-pop-up-window)))))

(push '(scheme-mode . "scheme")
      lsp-language-id-configuration)

(provide 'lsp-scheme)
;;; lsp-scheme.el ends here
