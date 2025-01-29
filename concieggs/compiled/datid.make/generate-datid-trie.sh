#!/bin/sh

generate-trie < $CONCIEGGS_DB_DIR/ordbog-med-bøjninger/unikke_udsagnsord_nutid_datid > "$(dirname "$0")/kode.c"
