;;; lsp-scheme.el --- lsp-mode scheme integration    -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Ricardo Gabriel Herdt

;; Author: Ricardo G. Herdt
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Client for the Scheme language server.

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

(defcustom lsp-scheme-chicken-langserver-command
  (locate-file "lsp-chicken-start-server" load-path)
  "Command to start the server."
  :type 'string
  :package-version '(lsp-mode . "0.0.1"))

(defcustom lsp-scheme-guile-langserver-command
  (locate-file "lsp-guile-start-server" load-path)
  "Command to start the server."
  :type 'string
  :package-version '(lsp-mode . "0.0.1"))

(defvar lsp-command-port 8888)
(defvar lsp-repl-server-port 37146)

(defun select-lsp-scheme-langserver-command (port)
  "Dispatches language server command based on selected implementation."
  (cond ((string-equal lsp-scheme-implementation "chicken")
         (list lsp-scheme-chicken-langserver-command
               (format "%d" port)
               (format "%d" lsp-command-port)
               (format "%d" lsp-repl-server-port))
         ;;(list "nc" "localhost" "10001")
         )
        ((string-equal lsp-scheme-implementation "guile")
         (list lsp-scheme-guile-langserver-command
               (format "%d" port)
               (format "%d" lsp-command-port)
               (format "%d" lsp-repl-server-port)))
        (t (error "Implementation not supported: %s"
                  lsp-scheme-implementation))))

;; (defun lsp-connect-repl ()
;;   (run-scheme (format "nc localhost %d" lsp-repl-server-port)))

(defun lsp-run-scheme (cmd)
  "Run an inferior Scheme process, input and output via buffer `*scheme*'.
If there is a process already running in `*scheme*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `scheme-program-name').
If the file `~/.emacs_SCHEMENAME' or `~/.emacs.d/init_SCHEMENAME.scm' exists,
it is given as initial input.
Note that this may lose due to a timing error if the Scheme processor
discards input when it starts up.
Runs the hook `inferior-scheme-mode-hook' (after the `comint-mode-hook'
is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (if current-prefix-arg
			 (read-string "Run Scheme: " scheme-program-name)
			 scheme-program-name)))
  (if (not (comint-check-proc "*lsp-scheme*"))
      (let ((cmdlist (split-string-and-unquote cmd)))
	(set-buffer (make-comint "lsp-scheme"
                                 `("127.0.0.1" . ,lsp-repl-server-port)))
	(inferior-scheme-mode)))
  (setq scheme-program-name cmd)
  (setq scheme-buffer "*lsp-scheme*")
  (pop-to-buffer-same-window "*lsp-scheme*"))

;; (defun lsp-run-scheme-guile (port)
;;   (interactive)
;;   (run-scheme (format "lsp-guile-start-server %d" port)))

;; (defun lsp-run-scheme-guile (port)
;;   (interactive)

;;   (run-scheme (format "lsp-guile-start-server %d" port))
  
;;   (run-with-idle-timer 2 nil
;;      (lambda ()
;;       (let ((count 0))
;;         (while (and (not (comint-check-proc "*scheme*"))
;;                     (< count 100))
;;           (progn
;;             (setq count (1+ count))
;;             ;;(setq lsp-repl-side-port (1+ lsp-repl-side-port))
;;             (run-scheme (format "lsp-guile-start-server %d" port))))))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-tcp-connection
                                   #'select-lsp-scheme-langserver-command)
                  ;;:activation-fn (lsp-activate-on "scheme")
                  ;;:initialized-fn #'lsp-run-scheme-guile
                  :major-modes '(scheme-mode)
                  :priority 1
                  :server-id 'scheme-lsp-server))

(push '(scheme-mode . "scheme")
      lsp-language-id-configuration)

;;(lsp-run-scheme-guile)
;;(lsp-consistency-check lsp-scheme)

(provide 'lsp-scheme)
;;; lsp-scheme.el ends here
