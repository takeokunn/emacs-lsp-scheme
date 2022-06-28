;;; lsp-scheme.el --- Scheme support for lsp-mode    -*- lexical-binding: t; -*-

;; Author: Ricardo G. Herdt <r.herdt@posteo.de>
;; Keywords: languages, lisp, tools
;; Version: 0.1.1
;; Package-Requires: ((emacs "25.1") (f "0.20.0") (lsp-mode "8.0.0"))

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
;; Currently this client only supports CHICKEN 5 and Guile 3, since
;; those are supported by scheme-lsp-server.

;;;; Installation

;;Make sure your chosen Scheme implementation is installed and on your
;;load-path.  Implementation support depends on availability of a corresponding
;;LSP server, as mentioned, for now only CHICKEN and Guile are supported.

;;On first run you should be prompted to install an lsp server.  The
;;extension will install it to its cache directory.
;;In case something goes wrong, manually install the server available at
;;https://codeberg.org/rgherdt/scheme-lsp-server (and make sure to create
;;an issue at our repository).

;;In order to achieve better results, follow these instructions to update
;;CHICKEN's documentation:

;;- install needed eggs: chicken-install -s r7rs apropos chicken-doc srfi-18 srfi-130
;;- update documentation database:
;;   $ cd `csi -R chicken.platform -p '(chicken-home)'`
;;   $ curl https://3e8.org/pub/chicken-doc/chicken-doc-repo-5.tgz | sudo tar zx


;;;; Setup

;;Add the following lines to your Emacs configuration file:

;;   (require lsp-scheme)
;;   (add-hook 'scheme-mode-hook #'lsp-scheme)
;;   (setq lsp-scheme-implementation "guile") ;;; customizable
;;
;;Alternatively you can add the specific command as hook, for example:
;;   (add-hook 'scheme-mode-hook #'lsp-scheme-guile)
;;In this case lsp-scheme-implementation is ignored.



;;;; Usage
;;This LSP client tries to implement an workflow similar to other Lisp-related
;;Emacs modes.  For instance, it relies on the interaction between the user and
;;the REPL to load information needed.  The interaction is currently based on
;;Emacs built-in Scheme inferior-mode.  So, for instance, in order to load the
;;current buffer you can just issue C-c C-l.  Since the REPL is connected to the
;;LSP server, this will allow it to fetch symbols defined in the buffer, as well
;;as libraries imported by it.

;;; Code:

;;;; Requirements

(require 'lsp-mode)
(require 'comint)
(require 'cmuscheme)

;;;; Constants

(defconst lsp-scheme--json-rpc-version
  "0.2.4"
  "Version of JSON-RPC implementation used.")

(defconst lsp-scheme--lsp-server-version
  "0.0.6"
  "Version of LSP Server implementation used.")

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
  (format "https://codeberg.org/rgherdt/scheme-lsp-server/archive/%s.zip"
          lsp-scheme--lsp-server-version)
  "Path to Scheme's LSP server."
  :type 'string
  :group 'lsp-scheme
  :package-version '(lsp-scheme . "0.0.1"))

;;;; CHICKEN
(defvar lsp-scheme--chicken-install-dir
  (f-join lsp-server-install-dir "lsp-chicken-server/"))

(defun lsp-scheme--chicken-install-egg (egg-name install-dir callback error-callback)
  "Ensure EGG-NAME is installed at provided INSTALL-DIR.
This function is meant to be used by lsp-mode's `lsp--install-server-internal`,
and thus calls its CALLBACK after completing, or ERROR-CALLBACK in case
something is wrong."
  (condition-case err
      (progn
        (f-delete install-dir t)
        (lsp--info (format "Installing software and its dependencies..."))
        (call-process-shell-command
         (format
          "CHICKEN_INSTALL_REPOSITORY=%s CHICKEN_INSTALL_PREFIX=%s chicken-install %s"
          install-dir
          install-dir
          egg-name)
         nil
         "*Shell Command Output*"
         t)
        (lsp--info "Installation finished.")
        (funcall callback))
    (error (funcall error-callback err))))

(defun lsp-scheme--chicken-ensure-server
    (_client callback error-callback _update?)
  "Ensure LSP Server for Chicken is installed and running.
This function is meant to be used by lsp-mode's `lsp--install-server-internal`,
and thus calls its CALLBACK and ERROR-CALLBACK in case something wents wrong.
If a server is already installed, reinstall it.  _CLIENT and _UPDATE? are
ignored"
  (condition-case err
      (progn
        (when (f-exists? lsp-scheme--chicken-install-dir)
          (f-delete lsp-scheme--chicken-install-dir t))
        (lsp-scheme--chicken-install-egg "lsp-server"
                                         lsp-scheme--chicken-install-dir
                                         (lambda ()
                                           (funcall callback))
                                         error-callback))
    (error (funcall error-callback err))))

(defun lsp-scheme--chicken-setup-environment ()
  "Set environment variables nedded to run local install."
  (setenv "CHICKEN_REPOSITORY_PATH"
          (concat
           lsp-scheme--chicken-install-dir
           ":"
           (shell-command-to-string
            "csi -e '(import (chicken platform)) (for-each (lambda (p) (display p) (display \":\")) (repository-path))'"))))

(defun lsp-scheme--chicken-server-installed-p ()
  "Check if LSP server for chicken is installed."
  (lsp-scheme--accepted-installed-server-p "chicken-lsp-server"
                                           lsp-scheme--chicken-install-dir))

(defun lsp-scheme--chicken-start ()
  "Return list containing a command to run and its arguments based on PORT.
The command requests from a running command server (started with
 `lsp-scheme--run') an LSP server for the current scheme buffer."
  (add-to-list 'load-path
               lsp-scheme--chicken-install-dir)

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

(defun lsp-scheme--guile-setup-environment ()
  "Set environment variables nedded to run local install."
  (add-to-list 'load-path
               lsp-scheme--guile-install-dir)
  (setenv "GUILE_LOAD_COMPILED_PATH"
          (concat
           (f-join lsp-scheme--guile-install-dir ":")
           (f-join lsp-scheme--guile-install-dir
                   "lib/guile/3.0/site-ccache/:")
           (getenv "GUILE_LOAD_COMPILED_PATH")))
  (setenv "GUILE_LOAD_PATH"
          (concat
           (f-join lsp-scheme--guile-install-dir ":")
           (f-join lsp-scheme--guile-install-dir
                   "share/guile/3.0/:")
           (getenv "GUILE_LOAD_PATH"))))

(defun lsp-scheme--guile-server-installed-p ()
  "Check if LSP server for Guile is installed."
  (lsp-scheme--accepted-installed-server-p "guile-lsp-server"
                                           lsp-scheme--guile-install-dir))

(defun lsp-scheme--guile-start ()
  "Return list containing a command to run and its arguments based on PORT.
The command requests from a running command server (started with
 `lsp-scheme--run') an LSP server for the current scheme buffer."
  (list (or (locate-file "guile-lsp-server" load-path)
            (locate-file (f-join "bin" "guile-lsp-server") load-path))
        "--log-level"
        lsp-scheme-log-level))

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

(defun lsp-scheme--accepted-installed-server-p (server-name &rest extra-paths)
  "Check if LSP server SERVER-NAME with correct version is installed.
The caller may provide EXTRA-PATHS to search for."
  (let ((bin-path (or (executable-find server-name)
                      (locate-file server-name load-path)
                      (locate-file (f-join "bin" server-name) load-path)
                      (locate-file server-name extra-paths)
                      (locate-file (f-join "bin" server-name) extra-paths))))
    (if (not bin-path)
        nil
      (let ((res (shell-command-to-string (format "%s %s" bin-path "--version"))))
        (if (not res)
            nil
          (let ((installed-version (lsp-scheme--get-version-from-string res)))
            (or (string-equal installed-version
                              lsp-scheme--lsp-server-version)
                (string-greaterp installed-version
                                 lsp-scheme--lsp-server-version))))))))

(defun lsp-scheme--make-install (decompressed-path callback error-callback)
  "Install automake based project at DECOMPRESSED-PATH.
The caller shall provide a CALLBACK to execute after finishing installing
the tarball, and an ERROR-CALLBACK to be called in case of an error."
  (let ((cmd (format
              "cd %s && ./configure --prefix=%s && make && make install && cd -"
              decompressed-path
              lsp-scheme--guile-install-dir)))
    (message cmd)
    (lsp--info "Building software...")
    (let ((res (call-process-shell-command cmd
                                           nil
                                           "*Shell command output*"
                                           t)))
      (if (= res 0)
          (funcall callback)
        (funcall error-callback
                 (format "Error building %s" decompressed-path))))))

(defun lsp-scheme--initialize ()
  "Initialize extension (setup 'load-path' and environment)."
  (add-to-list 'load-path
               lsp-scheme--chicken-install-dir)
  (add-to-list 'load-path
               lsp-scheme--guile-install-dir)
  (lsp-scheme--guile-setup-environment)
  (lsp-scheme--chicken-setup-environment))

;;;###autoload
(defun lsp-scheme ()
  "Setup and start Scheme's LSP server."
  (cond ((equal lsp-scheme-implementation "chicken")
         (lsp-scheme-chicken))
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
                    :server-id 'lsp-chicken-server
                    :download-server-fn #'lsp-scheme--chicken-ensure-server)))

(defun lsp-scheme--guile-register-client ()
  "Register Guile LSP client."
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     #'lsp-scheme--guile-start
                                     #'lsp-scheme--guile-server-installed-p)
                    :major-modes '(scheme-mode)
                    :priority 1
                    :server-id 'lsp-guile-server
                    :download-server-fn #'lsp-scheme--guile-ensure-server)))



(push '(scheme-mode . "scheme")
      lsp-language-id-configuration)

(provide 'lsp-scheme)
;;; lsp-scheme.el ends here
