# emacs-lsp-scheme

Emacs LSP client for the Scheme programming language. This is an unreleased
work in progress.

This plugin requires an Scheme LSP server in order to work. Currently it uses
https://gitlab.com/rgherdt/scheme-lsp-server (developed by the same author), which
is also in an early stage of development. Currently only Guile and CHICKEN are supported.

## Requirements

Please follow the instructions specific to your chosen Scheme implementation
before proceeding with the installation of this extensions:

### CHICKEN 5

- make sure CHICKEN 5 is installed.
- install needed eggs:
  `chicken-install -s r7rs apropos chicken-doc srfi-18 srfi-130`
- update documentation database:
```
$ cd `csi -R chicken.platform -p '(chicken-home)'`
$ curl https://3e8.org/pub/chicken-doc/chicken-doc-repo-5.tgz | sudo tar zx
```


## Installing

Currently this software is only available through this repository. Simply check it out 
and add it to your load path, for example:

`(add-to-list 'load-path "~/.emacs.d/lisp/lsp-scheme/")`


## Setup

Add the following lines to your Emacs configuration file:

`(require lsp-chicken)`
;; alternatively `(require lsp-guile)`
`(add-hook 'scheme-mode-hook #'lsp)`

`(add-hook 'scheme-mode-hook #'lsp-chicken)`
;; alternatively `(require lsp-guile)`

This should on first run automatically install the corresponding LSP server and
start the client.

## Usage

This LSP client tries to implement an workflow similar to other Lisp-related Emacs
modes. For instance, it relies on the interaction between the user and the REPL
to load information needed. The interaction is currently based on Emacs built-in
Scheme `inferior-mode`. So, for instance, in order to load the current buffer you
can just issue `C-c C-l`. Since the REPL is connected to the LSP server, this
will allow it to fetch symbols defined in the buffer, as well as libraries
imported by it.


## Implementation specific notes

### CHICKEN

Since CHICKEN's run-time does not provide information regarding the location of
defined symbols, we implemented an workaround that scans existing source code
for symbols. This is done for any project opened in Emacs. By setting
the environment variable CHICKEN_SRC to point to the source code of CHICKEN
itself, the LSP server is able to provide location information of symbols defined
in it.

The current scanning algorithm is pretty simple and "imports" symbols from
our modules found, regardless of it being actually used by your project
or not. In the future we may consider a more sophisticated solution that actually
keeps track of imported modules by parsing .EGG files and possibly any includes.
Suggestions for improving this are obviously welcome.

## Design notes

Here some notes to explain some design decisions of this extension.

The emacs-lsp plugin usually works by spawning a new LSP server for each
open project. On a previous stage of this extension, this meant that each file
had its own Scheme interpreter running. The problem of this approach is that
we want to rely on the implementation's run-time and the interaction of the user
with it to provide information like symbol location. In other words, what the
user loads in a REPL should affect what the LSP server delivers.

In order to solve this, the LSP server was designed to provide a function
that launches a "command server". It is basically a regular REPL that listens
on an specific port for socket commands that spawn LSP servers on different threads.
This way we have a single Scheme instance that provides the REPL functionality
and manages multiple LSP server on different threads.

## Contributing

Any contribution to this extension or the LSP server is welcome. Feel
free to open issues or contact me (rgherdt on Libera's #chicken, #guile, #scheme)
to discuss ideas and improvements.
