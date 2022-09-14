;;; lsp-scheme.el --- Scheme support for lsp-mode    -*- lexical-binding: t; -*-

;; Author: Ricardo G. Herdt <r.herdt@posteo.de>
;; Keywords: languages, lisp, tools
;; Version: 0.2.1
;; Package-Requires: ((emacs "26.1") (f "0.20.0") (lsp-mode "8.0.0"))

;; Copyright (C) 2022 Ricardo Gabriel Herdt

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

;; URL: https://codeberg.org/rgherdt/emacs-lsp-scheme

;;; Commentary:

;; Client for the Scheme LSP server.
;; Currently this client only supports CHICKEN 5, Gambit 4.9.4+ and Guile 3.

;;;; Installation

;;Make sure your chosen Scheme implementation is installed and on your
;;load-path.  Implementation support depends on availability of a corresponding
;;LSP server, as mentioned, for now only CHICKEN, Gambit and Guile are
;;supported.

;;For Gambit and Guile, the extension will prompt you for auto-installing the
;;LSP server.  In case something goes wrong (or you are using CHICKEN),
;;manually install the correposnding server following the instructions at
;;https://codeberg.org/rgherdt/scheme-lsp-server.


;;;; Setup

;; Add the following lines to your Emacs configuration file:

;; ```
;; (require 'lsp-scheme)
;; (add-hook 'scheme-mode-hook #'lsp-scheme)

;; (setq lsp-scheme-implementation "guile") ;;; also customizable
;; ```

;; Alternatively you can add the specific command as hook, for example:
;; ```
;;    (add-hook 'scheme-mode-hook #'lsp-scheme-guile)
;; ```
;; In this case, `lsp-scheme-implementation` is ignored.


;; This should in the first run automatically install the corresponding LSP server
;; and start the client.  In case you have trouble, you can manually install the
;; server following the corresponding instructions.



;;;; Usage
;; Starting with version 0.1.0, this LSP client works on the background and does
;; not rely on interaction with the user to keep LSP-related information on sync.
;; It also does not provide a custom REPL (as in the first version) anymore, you
;; can use Emacs' built-in Scheme support instead (`run-scheme`).  You may customize
;; `scheme-program-name` to run the interpreter of your Scheme of choice.

;; The LSP server works best if your code is packed inside a library definition
;; (either R6RS, R7RS or implementation specific). Depending on the implementation,
;; you can improve the experience by adding your library to a path where your
;; implementation can find it (see note regarding Guile below).

;;; Code:

;;;; Requirements

(require 'lsp-mode)
(require 'comint)
(require 'cmuscheme)

;;;; Constants

(defconst lsp-scheme--json-rpc-version
  "0.2.6"
  "Version of JSON-RPC implementation used.")

(defconst lsp-scheme--chicken-server-minimum-version
  "0.1.8"
  "Minimum LSP Server server required for CHICKEN.")

(defconst lsp-scheme--gambit-server-minimum-version
  "0.2.0"
  "Minimum LSP Server server required for Gambit.")

(defconst lsp-scheme--guile-server-minimum-version
  "0.1.10"
  "Minimum LSP Server server required for Guile.")

(defconst lsp-scheme--shell-output-name
  "*Shell Command Output*"
  "Name used for calls to `call-process-shell-command`.")

;;;; General Customization

(defgroup lsp-scheme nil
  "LSP support for Scheme, using scheme-lsp-server."
  :group 'lsp-mode
  :link '(url-link "https://gitlab.com/rgherdt/scheme-lsp-server"))

(defcustom lsp-scheme-implementation "guile"
  "Scheme implementation to be used.  Supported options: guile, chicken."
  :type 'string
  :group 'lsp-scheme
  :package-version '(lsp-scheme . "0.0.1"))

(defcustom lsp-scheme-log-level "error"
  "Log level verbosity.  One of \"error\", \"warning\", \"info\" or \"debug\"."
  :type 'string
  :group 'lsp-scheme
  :package-version '(lsp-scheme . "0.0.1"))

(defcustom lsp-scheme-json-rpc-url
  (format "https://codeberg.org/rgherdt/scheme-json-rpc/archive/%s.zip"
          lsp-scheme--json-rpc-version)
  "Path to JSON-RPC library."
  :type 'string
  :group 'lsp-scheme
  :package-version '(lsp-scheme . "0.0.1"))

(defcustom lsp-scheme-server-url
  "https://codeberg.org/rgherdt/scheme-lsp-server/archive/master.zip"
  "Path to Scheme's LSP server."
  :type 'string
  :group 'lsp-scheme
  :package-version '(lsp-scheme . "0.0.1"))

(defcustom lsp-scheme-repl-port
  36821
  "Port to open a REPL in the running LSP server."
  :type 'integer
  :group 'lsp-scheme
  :package-version '(lsp-scheme . "0.2.0"))

;;;; CHICKEN
(defun lsp-scheme--chicken-server-installed-p ()
  "Check if LSP server for chicken is installed."
  (lsp-scheme--accepted-installed-server-p
   "chicken-lsp-server"
   lsp-scheme--chicken-server-minimum-version
   ""))

(defun lsp-scheme--chicken-start ()
  "Return list containing a command to run and its arguments based on PORT.
The command requests from a running command server (started with
 `lsp-scheme--run') an LSP server for the current scheme buffer."
  (list (or (locate-file "chicken-lsp-server" load-path)
            (locate-file (f-join "bin" "chicken-lsp-server") load-path))
        "--log-level"
        lsp-scheme-log-level))

;;;###autoload
(defun lsp-scheme-chicken ()
  "Register CHICKEN's LSP server if needed."
  (lsp-scheme--initialize)
  (unless (gethash 'lsp-chicken-server lsp-clients)
    (lsp-scheme--chicken-register-client))
  (lsp))

;;;; Gambit
(defun lsp-scheme--gambit-check-compiler ()
  "Check if compiler is recent enough to be used to compile the library."
  (let ((version (car (split-string (shell-command-to-string "gsi -v")))))
    (not (string-version-lessp version "v4.9.4-89"))))

(defun lsp-scheme--gambit-compile-library ()
  "Compile installed LSP library for Gambit."
  (cond ((lsp-scheme--gambit-check-compiler)
         (lsp--info (format "Compiling library..."))
         (let* ((userlib (shell-command-to-string
                          "gsi -e '(display (path-expand \"~~userlib\"))'"))
                (compile-script
                 (locate-file
                  "compile.sh"
                  (list (f-join
                         userlib
                         "codeberg.org/rgherdt/scheme-lsp-server/@/gambit")))))
           (call-process-shell-command compile-script
                                       nil
                                       lsp-scheme--shell-output-name
                                       t)))
        (t (lsp--info
            (concat "Old compiler detected. Please build a newer version of "
                    "GSC in order to compile the LSP library")))))

(defun lsp-scheme--gambit-generate-executable ()
  "Compile gambit-lsp-server."
  (let ((source (locate-file (f-join "scripts" "gambit-lsp-server.scm") load-path)))
    (call-process-shell-command (format "gsc -exe -nopreload %s" source)
                                nil
                                lsp-scheme--shell-output-name
                                t)))

(defun lsp-scheme--gambit-ensure-server
    (_client callback error-callback _update?)
  "Ensure LSP Server for Gambit is installed and running.
This function is meant to be used by lsp-mode's `lsp--install-server-internal`,
and thus calls its CALLBACK and ERROR-CALLBACK in case something wents wrong.
If a server is already installed, reinstall it.  _CLIENT and _UPDATE? are
ignored"
  (condition-case err
      (let* ((install-script
              (locate-file (f-join "scripts" "install-gambit-lsp-server.sh")
                           load-path))
             (compile-p (lsp-scheme--gambit-check-compiler))
             (install-cmd
              (if compile-p
                  (format "%s compile" install-script)
                install-script))
             (lib-installed-p
              (lsp-scheme--gambit-library-installed-p))
             (executable-found-p
              (lsp-scheme--gambit-executable-installed-p)))
        (cond ((not lib-installed-p)
               (lsp--info (format "Installing LSP server for Gambit"))
               (call-process-shell-command install-cmd
                                           nil
                                           lsp-scheme--shell-output-name
                                           t))
              ((not executable-found-p)
               (lsp--info (format "LSP Library found, but executable missing. Compiling it."))
               (lsp-scheme--gambit-generate-executable))
              (t
               (lsp--info (format "LSP server already installed."))))
        (funcall callback))
    (error (funcall error-callback err))))

(defun lsp-scheme--gambit-library-installed-p ()
  "Check if lsp-server library is installed."
  (let ((res (call-process-shell-command
              "gsi -e '(import (codeberg.org/rgherdt/scheme-lsp-server gambit util)) (exit)'")))
    (= res 0)))

(defun lsp-scheme--gambit-executable-installed-p ()
  "Check if compiled guile-lsp-server is available."
  (locate-file (f-join "scripts" "gambit-lsp-server") load-path))

(defun lsp-scheme--gambit-server-installed-p ()
  "Check if LSP server for Gambit is installed."
  (and (lsp-scheme--gambit-library-installed-p)
       (lsp-scheme--accepted-installed-server-p
        "gambit-lsp-server"
        lsp-scheme--gambit-server-minimum-version
        "")))

(defun lsp-scheme--gambit-find-lsp-server ()
  "Return path to gambit-lsp-server if found."
  (or (locate-file "gambit-lsp-server" load-path)
      (locate-file (f-join "scripts" "gambit-lsp-server") load-path)
      (locate-file (f-join "bin" "gambit-lsp-server") load-path)))

(defun lsp-scheme--gambit-start ()
  "Return list containing a command to run and its arguments based on PORT.
The command requests from a running command server (started with
 `lsp-scheme--run') an LSP server for the current scheme buffer."
  (add-to-list 'load-path
               "/home/rgherdt/.local/bin")

  (list (lsp-scheme--gambit-find-lsp-server)
        "--log-level"
        lsp-scheme-log-level))

;;;###autoload
(defun lsp-scheme-gambit ()
  "Regist Guile's LSP server if needed."
  (lsp-scheme--initialize)
  (unless (gethash 'lsp-gambit-server lsp-clients)
    (lsp-scheme--gambit-register-client))
  (lsp))


;;;; Guile
(defvar lsp-scheme--guile-install-dir
  (f-join lsp-server-install-dir "lsp-guile-server/"))

(defun lsp-scheme--guile-ensure-server (_client callback error-callback _update?)
  "Ensure LSP Server for Guile is installed and running.
This function is meant to be used by lsp-mode's `lsp--install-server-internal`,
and thus calls its CALLBACK and ERROR-CALLBACK in case something wents wrong.
If a server is already installed, reinstall it.  _CLIENT and _UPDATE? are
ignored."
  (condition-case err
      (let ((tmp-dir (make-temp-file "lsp-scheme-install" t)))
        (f-delete lsp-scheme--guile-install-dir t)
        (mkdir lsp-scheme--guile-install-dir t)

        (lsp-download-install
         (lambda ()
           (lsp-scheme--make-install
            (f-join tmp-dir
                    "scheme-json-rpc"
                    "guile")
            (lambda ()
              (lsp-download-install
               (lambda ()
                 (lsp-scheme--make-install
                  (f-join tmp-dir
                          "scheme-lsp-server"
                          "guile")
                  (funcall callback)
                  error-callback))
               error-callback
               :url lsp-scheme-server-url
               :decompress :zip
               :store-path (f-join tmp-dir "scheme-lsp-server")))
            error-callback))
         error-callback
         :url lsp-scheme-json-rpc-url
         :decompress :zip
         :store-path (f-join tmp-dir "scheme-json-rpc")))
    (error (funcall error-callback err))))

(defun lsp-scheme--guile-environment ()
  "Setup environment for calling Guile's LSP server.
Return an alist of ((ENV-VAR . VALUE)), where VALUE is appropriated to be
consumed by lsp-mode (see ENVIRONMENT-FN argument to LSP--CLIENT)."
  `(("GUILE_LOAD_COMPILED_PATH" .
     (list ,lsp-scheme--guile-install-dir
            ,(f-join lsp-scheme--guile-install-dir
                    "lib/guile/3.0/site-ccache/")
            ,(or (getenv "GUILE_LOAD_COMPILED_PATH") "")))
    ("GUILE_LOAD_PATH" .
     (list ,lsp-scheme--guile-install-dir ":"
           ,(f-join lsp-scheme--guile-install-dir
                    "share/guile/site/3.0/")
           ,(or (getenv "GUILE_LOAD_PATH") "")))))

(defun lsp-scheme--guile-server-installed-p ()
  "Check if LSP server for Guile is installed."
  (lsp-scheme--accepted-installed-server-p
   "guile-lsp-server"
   lsp-scheme--guile-server-minimum-version
   (format "GUILE_LOAD_COMPILED_PATH=%s:%s:%s GUILE_LOAD_PATH=%s:%s:%s"
           lsp-scheme--guile-install-dir
           (f-join lsp-scheme--guile-install-dir
                   "lib/guile/3.0/site-ccache/")
           (getenv "GUILE_LOAD_COMPILED_PATH")

           lsp-scheme--guile-install-dir
           (f-join lsp-scheme--guile-install-dir
                   "share/guile/site/3.0/")
           (getenv "GUILE_LOAD_PATH"))
   lsp-scheme--guile-install-dir))

(defun lsp-scheme--guile-start ()
  "Return list containing a command to run and its arguments based on PORT.
The command requests from a running command server (started with
 `lsp-scheme--run') an LSP server for the current scheme buffer."
  (list (or (locate-file "guile-lsp-server" load-path)
            (locate-file (f-join "bin" "guile-lsp-server") load-path))
        "--log-level"
        lsp-scheme-log-level
        "--listen"
        (format "%d" lsp-scheme-repl-port)))

;;;###autoload
(defun lsp-scheme-guile ()
  "Regist Guile's LSP server if needed."
  (lsp-scheme--initialize)
  (unless (gethash 'lsp-guile-server lsp-clients)
    (lsp-scheme--guile-register-client))
  (lsp))

;;;; Common functions

(defun lsp-scheme--get-version-from-string (str)
  "Get LSP server version number out of multi-line STR.
Used to extract version from output of <>-lsp-server --version."
  (let* ((lines (split-string str "\n"))
         (version-line (seq-find (lambda (line)
                                   (string-prefix-p "Version " line))
                                 lines)))
    (replace-regexp-in-string "\\(Version \\)" "" version-line)))

(defun lsp-scheme--accepted-installed-server-p (server-name target-version env &rest extra-paths)
  "Check if LSP server SERVER-NAME with correct TARGET-VERSION are installed.
ENV must be a string setting environment variables needed by the LSP server.
The caller may provide EXTRA-PATHS to search for."
  (let ((bin-path (or (executable-find server-name)
                      (locate-file server-name load-path)
                      (locate-file (f-join "bin" server-name) load-path)
                      (locate-file (f-join "scripts" server-name) load-path)
                      (locate-file server-name extra-paths)
                      (locate-file (f-join "bin" server-name) extra-paths)
                      (locate-file (f-join "scripts" server-name) extra-paths))))
    (if (not bin-path)
        nil
      (let ((res (shell-command-to-string
                  (format "%s %s %s" env bin-path "--version"))))
        (if (not res)
            nil
          (let ((installed-version (lsp-scheme--get-version-from-string res)))
            (or (string-equal installed-version target-version)
                (string-version-lessp target-version installed-version))))))))

(defun lsp-scheme--make-install (decompressed-path callback error-callback)
  "Install automake based project at DECOMPRESSED-PATH.
The caller shall provide a CALLBACK to execute after finishing installing
the tarball, and an ERROR-CALLBACK to be called in case of an error."
  (let* ((env-string (format "GUILE_LOAD_PATH=.:...:%s:$GUILE_LOAD_PATH GUILE_LOAD_COMPILED_PATH=.:...:%s:$GUILE_COMPILE_LOAD_PATH"
                             (f-join lsp-scheme--guile-install-dir
                                     "share/guile/site/3.0/")
                             (f-join lsp-scheme--guile-install-dir
                                     "lib/guile/3.0/site-ccache/")))
         (cmd (format
               "cd %s && %s ./configure --prefix=%s && %s make && %s make install && cd -"
               decompressed-path
               env-string
               lsp-scheme--guile-install-dir
               env-string
               env-string)))
    (message cmd)
    (lsp--info "Building software...")
    (let ((res (call-process-shell-command cmd
                                           nil
                                           "*Shell Command output*"
                                           t)))
      (if (= res 0)
          (funcall callback)
        (funcall error-callback
                 (format "Error building %s" decompressed-path))))))

(defun lsp-scheme--initialize ()
  "Initialize extension (setup 'load-path' and environment)."
  (add-to-list 'load-path
               lsp-scheme--guile-install-dir))

;;;###autoload
(defun lsp-scheme ()
  "Setup and start Scheme's LSP server."
  (cond ((equal lsp-scheme-implementation "chicken")
         (lsp-scheme-chicken))
        ((equal lsp-scheme-implementation "gambit")
         (lsp-scheme-gambit))
        ((equal lsp-scheme-implementation "guile")
         (lsp-scheme-guile))
        (t (user-error "Implementation not supported: %s"
                       lsp-scheme-implementation))))

;;;; Register clients

(defun lsp-scheme--chicken-register-client ()
  "Register CHICKEN LSP client."
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     #'lsp-scheme--chicken-start
                                     #'lsp-scheme--chicken-server-installed-p)
                    :major-modes '(scheme-mode)
                    :priority 1
                    :server-id 'lsp-chicken-server)))

(defun lsp-scheme--gambit-register-client ()
  "Register Gambit LSP client."
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     #'lsp-scheme--gambit-start
                                     #'lsp-scheme--gambit-server-installed-p)
                    :major-modes '(scheme-mode)
                    :priority 1
                    :server-id 'lsp-gambit-server
                    :download-server-fn #'lsp-scheme--gambit-ensure-server)))


(defun lsp-scheme--guile-register-client ()
  "Register Guile LSP client."
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     #'lsp-scheme--guile-start
                     #'lsp-scheme--guile-server-installed-p)
    :major-modes '(scheme-mode)
    :environment-fn #'lsp-scheme--guile-environment
    :priority 1
    :server-id 'lsp-guile-server
    :download-server-fn #'lsp-scheme--guile-ensure-server)))



(push '(scheme-mode . "scheme")
      lsp-language-id-configuration)

(provide 'lsp-scheme)
;;; lsp-scheme.el ends here
