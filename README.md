# emacs-lsp-scheme

Emacs LSP client for the Scheme programming language.

This plugin requires an Scheme LSP server in order to work. Currently it uses
https://gitlab.com/rgherdt/scheme-lsp-server, which is also in an early stage of
development. Currently only Guile and CHICKEN are supported.

## Requirements

Please follow the instructions specific to your chosen Scheme implementation
before proceeding with the installation of this extension:

### CHICKEN 5

- make sure CHICKEN 5 is installed.
- install needed eggs:
  `chicken-install -s r7rs apropos chicken-doc srfi-18`
- update documentation database:
```
$ cd `csi -R chicken.platform -p '(chicken-home)'`
$ curl https://3e8.org/pub/chicken-doc/chicken-doc-repo-5.tgz | sudo tar zx
```

### Guile 3

All you need is to install Guile 3.


## Installing

This software is available on MELPA. Make sure MELPA is configured, and install
it with `M-x package-install RET lsp-scheme`. Alternatively, download this
repository and add it to your load path, for example:

`(add-to-list 'load-path "~/.emacs.d/lisp/lsp-scheme/")`


## Setup

Add the following lines to your Emacs configuration file:

```
(require lsp-scheme)
(add-hook 'scheme-mode-hook #'lsp-scheme)

(setq lsp-scheme-implementation "guile") ;;; also customizable
```

Alternatively you can add the specific command as hook, for example:
```
   (add-hook 'scheme-mode-hook #'lsp-scheme-guile)
```
In this case, `lsp-scheme-implementation` is ignored.


This should in the first run automatically install the corresponding LSP server
and start the client. In case you have trouble, you can manually install the
server following the corresponding instructions.

## Usage

Starting with version 0.1.0, this LSP client works on the background and does
not rely on interaction with the user to keep LSP-related information on sync.
It also does not provide a custom REPL (as in the first version) anymore, you
can use Emacs' built-in Scheme support instead (`run-scheme`).

The LSP server works best if your code is packed inside a library definition
(either R6RS, R7RS or implementation specific). Depending on the implementation,
you can improve the experience by adding your library to a path where your
implementation can find it (see note regarding Guile below).


## Implementation specific notes

### CHICKEN

Since CHICKEN's run-time does not provide information regarding the location of
defined symbols, we implemented an workaround that scans existing source code
for symbols. This is done for any project opened in Emacs. By setting
the environment variable CHICKEN_SOURCE_PATH to point to the source code of
CHICKEN itself, the LSP server is able to provide location information of
symbols defined in it.

### Guile

As noted above, the LSP server can provide better language support if your
code is defined in a library, and if it's able to locate your libraries in
general. To achieve this, you can add paths to the `GUILE_LOAD_PATH` and
`GUILE_LOAD_COMPILED_PATH` environment variables. Remember to add a "`...`" to
the environment path to reuse the default paths, for example:

`export GUILE_LOAD_PATH=/home/user/src/8sync:...:$GUILE_LOAD_PATH`

See the section "Load Paths" in Guile's manual for more information.


## Contributing

Any contribution to this extension or the LSP server is welcome. Feel
free to open issues or contact me (rgherdt on Libera's #chicken, #guile,
#scheme) to discuss ideas and improvements.
