;;; lsp-scheme.el --- lsp-mode scheme integration    -*- lexical-binding: t; -*-

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
;; Package-Requires: ((emacs "25.1") (dash "2.18.0") (f "0.20.0") (ht "2.3") (spinner "1.7.3") (markdown-mode "2.3") (lv "0"))

;;; URL: https://codeberg.org/rgherdt/emacs-lsp-scheme
;;; Version: 0.0.1

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

(defcustom lsp-scheme-log-level "debug"
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

(defvar lsp-scheme--command-port
  6251)

(defvar lsp-scheme--lsp-err-port
  7129)

(defconst lsp-scheme--json-rpc-version
  "0.2.2"
  "Version of JSON-RPC implementation used.")

(defconst lsp-scheme--lsp-server-version
  "0.2.2"
  "Version of JSON-RPC implementation used.")

(defvar lsp-scheme--json-rpc-url
  (format "https://codeberg.org/rgherdt/scheme-json-rpc/archive/%s.tar.gz"
          lsp-scheme--json-rpc-version lsp-scheme--json-rpc-version)
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


(defun lsp-scheme--get-root-name-from-tarball (file-name)
  "Exclude extension .tar.gz or .tar from FILE-NAME."
  (let ((root (file-name-sans-extension file-name)))
    (if (string-equal (file-name-extension root) "tar")
        (file-name-sans-extension root)
      root)))

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

(defun lsp-scheme ()
  "Setup and start Scheme's LSP server."
  (cond ((equal lsp-scheme-implementation "chicken")
         (require 'lsp-chicken)
         (lsp-chicken))
        ((equal lsp-scheme-implementation "guile")
         (require 'lsp-guile)
         (lsp-guile))
        (t (error (format "Implementation not supported: %s"
                          lsp-scheme-implementation)))))

(defun lsp-scheme-run (implementation)
  "Start the selected Scheme IMPLEMENTATION.
A REPL is opened in an *lsp-scheme* buffer, and a command server is launched
in the same instance, wait for commands to spawn LSP servers as needed."
  (interactive "sScheme implementation: \n")
  (let ((cmd (lsp-scheme--select-start-command implementation)))
    (when (not (comint-check-proc "*lsp-scheme*"))
      (let ((cmdlist (split-string-and-unquote cmd))
            (port-num (lsp--find-available-port
                       "localhost"
                       lsp-scheme--command-port))
            (err-port-num
             (lsp--find-available-port "localhost"
                                       lsp-scheme--lsp-err-port)))
        (setq lsp-scheme--command-port port-num)
        (setq lsp-scheme--lsp-err-port
              err-port-num)

        (apply 'make-comint "lsp-scheme"
               (car cmdlist)
               nil
               (cdr cmdlist))

        (comint-send-string
         "*lsp-scheme*"
         (format "\n(import (lsp-server))\n
                  (parameterize ((lsp-server-log-level '%s))
                    (lsp-command-server-start %d))\n#t\n"
                 lsp-scheme-log-level
                 port-num))

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
