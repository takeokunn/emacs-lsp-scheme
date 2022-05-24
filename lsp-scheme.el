;;; lsp-scheme.el --- lsp-mode scheme integration    -*- lexical-binding: t; -*-

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
  :package-version '(lsp-mode . "0.0.1"))

(defcustom lsp-scheme-log-level "debug"
  "Log level verbosity. One of \"warning\", \"info\", \"debug\"."
  :type 'string
  :package-version '(lsp-mode . "0.0.1"))

(defcustom lsp-scheme-listening-port "37146"
  "Port of Scheme process that shall start the LSP server")

(defcustom lsp-scheme-chicken-start-command
  "csi -R r7rs"
  "Command to start chicken's interpreter."
  :type 'string)

(defcustom lsp-scheme-guile-start-command
  "guile --r7rs"
  "Command to start guile's interpreter."
  :type 'string)

(defun lsp-scheme-select-start-command ()
  "Select a command to launch an interpreter for the selected implementation"
  (cond ((string-equal lsp-scheme-implementation "chicken")
         lsp-scheme-chicken-start-command)
        ((string-equal lsp-scheme-implementation "guile")
         lsp-scheme-guile-start-command)
        (t (error "Implementation not supported: %s"
                  lsp-scheme-implementation))))

(defcustom lsp-scheme-connect-command
  (locate-file "lsp-chicken-connect-to-server" load-path)
  "Command to spawn a new LSP connection."
  :type 'string
  :package-version '(lsp-mode . "0.0.1"))

(defcustom lsp-scheme-guile-connect
  (locate-file "lsp-guile-connect-to-server" load-path)
  "Command to spawn a new LSP connection."
  :type 'string
  :package-version '(lsp-mode . "0.0.1"))

(defvar lsp-scheme-command-port 8888)

(defun lsp-scheme-select-connect-command (port)
  "Dispatches language server command based on selected implementation."
  (cond ((string-equal lsp-scheme-implementation "chicken")
         (list lsp-scheme-chicken-connect
               (format "%d" lsp-scheme-command-port)
               (format "%d" port)))
        ((string-equal lsp-scheme-implementation "guile")
         (list lsp-scheme-guile-connect
               (format "%d" lsp-scheme-command-port)
               (format "%d" port)))
        (t (error "Implementation not supported: %s"
                  lsp-scheme-implementation))))

(defun lsp-scheme-ensure-running ()
  (interactive)
  (when (not (comint-check-proc "*lsp-scheme*"))
    (save-excursion
      (call-interactively 'lsp-scheme-run))))

(defun lsp-scheme-run (port-num)
  (interactive "nPort number: ")
  (message (format "%d" port-num))
  (let ((cmd (lsp-scheme-select-start-command)))
    (if (not (comint-check-proc "*lsp-scheme*"))
        (let ((cmdlist (split-string-and-unquote cmd)))
          (set-buffer (apply 'make-comint "lsp-scheme"
                             (car cmdlist)
                             nil
                             (cdr cmdlist))))
      (inferior-scheme-mode))
    (comint-send-string
     "*lsp-scheme*"
     (format "(import (lsp-server)) (lsp-command-server-start %d)\n#t\n"
             port-num))
    (setq scheme-program-name cmd)
    (setq scheme-buffer "*lsp-scheme*")
    (pop-to-buffer-same-window "*lsp-scheme*")))


(push '(scheme-mode . "scheme")
      lsp-language-id-configuration)


(lsp-register-client
 (make-lsp-client :new-connection (lsp-tcp-connection
                                   #'lsp-scheme-select-connect-command)
                  :major-modes '(scheme-mode)
                  :priority 1
                  :server-id 'scheme-lsp-server))


(provide 'lsp-scheme)
;;; lsp-scheme.el ends here
