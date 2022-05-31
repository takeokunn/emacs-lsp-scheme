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


;;; Commentary:

;; Client for the Scheme LSP server.

;;; Code:

(require 'lsp-mode)
(require 'comint)
(require 'cmuscheme)

;; scheme-langserver

(defgroup lsp-scheme-lsp-server nil
  "LSP support for Scheme, using scheme-lsp-server"
  :group 'lsp-mode
  :link '(url-link "https://gitlab.com/rgherdt/scheme-lsp-server"))

(defcustom lsp-scheme-implementation "chicken"
  "Scheme implementation used."
  :type 'string
  :group 'lsp-scheme
  :package-version '(lsp-scheme . "0.0.1"))

(defcustom lsp-scheme-log-level "debug"
  "Log level verbosity. One of \"warning\", \"info\", \"debug\"."
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

(defcustom lsp-scheme-command-port
  8888
  "Port of the command server."
  :type 'integer
  :group 'lsp-scheme
  :package-version '(lsp-scheme . "0.0.1"))

(defconst lsp-scheme--json-rpc-version
  "master"
  "Version of JSON-RPC implementation used.")

(defconst lsp-scheme--lsp-server-version
  "master"
  "Version of JSON-RPC implementation used.")

(defvar lsp-scheme--json-rpc-url
  (format "https://gitlab.com/rgherdt/scheme-json-rpc/-/archive/%s/scheme-json-rpc-%s.tar.gz"
          lsp-scheme--json-rpc-version lsp-scheme--json-rpc-version)
  "Path to JSON-RPC library.")

(defcustom lsp-scheme-server-url
  (format "https://gitlab.com/rgherdt/scheme-lsp-server/-/archive/%s/scheme-lsp-server-%s.tar.gz"
          lsp-scheme--lsp-server-version lsp-scheme--lsp-server-version)
  "Path to Scheme's LSP server."
  :type 'string
  :group 'lsp-scheme
  :package-version '(lsp-scheme . "0.0.1"))


(defconst lsp-scheme-ext-untar-script "tar -xzvf %s -C %s"
  "Script to decompress tar.gz tarballs. The script should take two arguments: the path to
 the tarball and the target directory to decompress the tarball into.")

(defcustom lsp-scheme-untar-script
  (cond ((executable-find "tar") lsp-scheme-ext-untar-script)
        (t nil))
  "Script to decompress tar.gz tarballs. Should be a format string with one argument
 for the file to be decompressed in place."
  :group 'lsp-scheme
  :type 'string
  :package-version '(lsp-scheme . "0.0.1"))

(defun lsp-scheme--untar (tar-file target-dir)
  "Decompress tar.gz tarball in place."
  (unless lsp-scheme-untar-script
    (error "Unable to find `tar' on the path, please either customize `lsp-scheme--untar-script' or manually decompress %s" tar-file))
  (call-process-shell-command (format lsp-scheme-untar-script tar-file target-dir)
                              nil
                              "*Shell Command Output*"
                              t))


(defun lsp-scheme--get-root-name-from-tarball (file-name)
  (let ((root (file-name-sans-extension file-name)))
    (if (string-equal (file-name-extension root) "tar")
        (file-name-sans-extension root)
      root)))

(defun lsp-scheme--install-tarball (url target-name error-callback &optional subdir)
  "Ensure tarball is installed at provided target."
  (condition-case err
      (let* ((tmp-dir (make-temp-file "lsp-scheme-install" t))
             (tarball-name (file-name-nondirectory url))
             (download-path (concat tmp-dir "/"  tarball-name))
             (decompressed-path
              (concat tmp-dir "/"
                      (lsp-scheme--get-root-name-from-tarball
                       tarball-name)
                      (if subdir
                          (concat "/" subdir)
                        "/")))
             (target-dir (concat user-emacs-directory target-name)))
        (when (f-exists? download-path)
          (f-delete download-path))
        (when (f-exists? target-dir)
          (f-delete target-dir t))
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
        (call-process-shell-command (format
                                     "cd %s && ./configure --prefix=%s && make && make install && cd -"
                                     decompressed-path
                                     (expand-file-name target-dir))
                                    nil
                                    "*Shell command output*"
                                    t)
        (lsp--info "Installation finished."))
    (error (funcall error-callback err))))

(defun lsp-scheme-select-start-command (impl)
  "Select a command to launch an interpreter for the selected implementation"
  (cond ((string-equal impl "chicken")
         lsp-scheme-chicken-start-command)
        ((string-equal impl "guile")
         lsp-scheme-guile-start-command)
        (t (error "Implementation not supported: %s"
                  lsp-scheme-implementation))))

(defun lsp-scheme-ensure-running (implementation)
  (interactive)
  (save-excursion
    (lsp-scheme-run implementation lsp-scheme-command-port)))

(defun lsp-scheme-run (implementation port-num)
  (interactive "sScheme implementation: \nnPort number: ")
  (message (format "%d" port-num))
  (let ((cmd (lsp-scheme-select-start-command implementation)))
    (when (not (comint-check-proc "*lsp-scheme*"))
      (let ((cmdlist (split-string-and-unquote cmd)))
        (set-buffer (apply 'make-comint "lsp-scheme"
                           (car cmdlist)
                           nil
                           (cdr cmdlist)))
        (comint-send-string
         "*lsp-scheme*"
         (format "(import (lsp-server)) (lsp-command-server-start %d)\n#t\n"
                 port-num))))
    
    (setq scheme-program-name cmd)
    (setq scheme-buffer "*lsp-scheme*")
    (pop-to-buffer-same-window "*lsp-scheme*")))

(push '(scheme-mode . "scheme")
      lsp-language-id-configuration)


(provide 'lsp-scheme)
;;; lsp-scheme.el ends here
