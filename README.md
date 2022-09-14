# emacs-lsp-scheme

Emacs LSP client for the Scheme programming language.

This plugin requires an Scheme LSP server in order to work. Currently it uses
https://gitlab.com/rgherdt/scheme-lsp-server, which supports Gambit, Guile and
CHICKEN for now.

## Requirements

You can either install an LSP server manually, or in case you are using Gambit
or Guile leave it to the extension to automatically download and install a
corresponding server according to `lsp-scheme-implementation`. Please follow 
the instructions specific to your chosen Scheme implementation before
installing/activating this extension.

### CHICKEN 5

Starting with version 0.2.1 we don't auto-install the LSP server for CHICKEN
anymore (since installing it locally to the extension was suboptimal in some
cases). Installing it manually is trivial though:

- install needed eggs:
  `chicken-install -s r7rs apropos chicken-doc srfi-18`
- update documentation database:
```
$ cd `csi -R chicken.platform -p '(chicken-home)'`
$ curl https://3e8.org/pub/chicken-doc/chicken-doc-repo-5.tgz | sudo tar zx
```
- install the lsp server:
```
$ chicken-install -s lsp-server
```

### Gambit

You need Gambit 4.9.4+ to install and use this library. For better results,
install the latest gambit version provided in the `master` branch, since it
contains fixes that allow compiling the library for better performance.
Please make sure to also compile its modules using `make modules`.

### Guile 3

You need both Guile 3.x and its development libraries. For ex. in Debian 11,
make sure guile-3.0 and guile-3.0-dev are installed.


## Installing

This software is available on MELPA. Make sure MELPA is configured, and install
it with <kbd>M-x</kbd> `package-install` <kbd>RET</kbd> `lsp-scheme`.
Alternatively, download this repository and add it to your load path, for
example:

`(add-to-list 'load-path "~/.emacs.d/lisp/lsp-scheme/")`


## Setup

Add the following lines to your Emacs configuration file:

```
(require 'lsp-scheme)
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
