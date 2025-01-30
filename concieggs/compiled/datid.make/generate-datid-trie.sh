#!/bin/sh
#
# Lav en trie der går fra nutidsudsagnsord til tilhørende datidsudsagnsord.

{
    cat $CONCIEGGS_DB_DIR/ordbog-med-bøjninger/unikke_udsagnsord_nutid_datid*
} | generate-trie > "$(dirname "$0")/kode.c"
