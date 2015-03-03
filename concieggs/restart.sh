#!/bin/sh
#
# Restart one of concieggsd's subsystems.

if [ $# -lt 1 ]; then
    echo "usage: $0 event|irc"
fi

case "$1" in
    event)
        kill $(ps | grep irc_event_reader.awk | cut -d ' ' -f 1)
        ;;
    irc)
        pkill sic
        ;;
    *)
        exit 1
        ;;
esac
