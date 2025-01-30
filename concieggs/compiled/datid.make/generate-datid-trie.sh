#!/bin/sh
#
# Lav en trie der går fra nutidsudsagnsord til tilhørende datidsudsagnsord.

generate-trie < $CONCIEGGS_DB_DIR/ordbog-med-bøjninger/unikke_udsagnsord_nutid_datid > "$(dirname "$0")/kode.c"
