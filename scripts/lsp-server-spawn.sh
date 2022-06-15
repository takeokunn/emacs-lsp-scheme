#!/bin/bash

set -x

LOG_FILE="lsp-server-$1.log"
MAX_TRIES=5
REPL_PORT=$1

>&2 echo "starting LSP client on port $1"

## Netcat bundles both stderr and stdout from the called process into
## stdout, leaving stderr for its own notifications. This causes the LSP
## communication to error out, since debugging messages are sent back to the
## client. A solution is to redirect logging to a file, and send changes to it
## to stderr. This way logging messages can be seen in the corresponding
## *lsp-...::stderr buffer.
start_code="(import (lsp-server)) \
            (parameterize ((lsp-server-log-level 'debug) \
                           (lsp-server-log-file \"$LOG_FILE\")) \
               (lsp-server-start/stdio))"

rm -f $LOG_FILE
touch $LOG_FILE && tail -f $LOG_FILE >&2 &

i=0
start_netcat () {
    cat <(echo $start_code) - | nc localhost $REPL_PORT;
}


## In some cases netcat fails to connect on first try, probabley due to timing
## while starting the client. For this reason we retry connecting a couple of
## times before giving up.
while ! start_netcat; do
    if [ $i -gt $MAX_TRIES ]; then
        >&2 echo "Limit of retries reached. Aborting"
        exit 1
    fi
    i=$((i+1))
    >&2 echo "Error starting netcat. Retrying"
    sleep 1
done

>&2 echo "LSP server started on port $1"
