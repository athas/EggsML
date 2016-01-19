#!/bin/sh
#
# Spread wealth from one inactive member to all active members.

cd "$(dirname "$0")/.."

active_days_threshold=31

user_source="$(./fakeconcieggs randomName "$1")"
if ! [ "$user_source" ]; then
    echo "error: source user not specified"
    exit 1
fi

user_regex="$user_source"
for alias in "$(./fakeconcieggs eggstool aliases "$user_source")"; do
    user_regex="$user_regex|$alias"
done

amount_source="$(./fakeconcieggs eggstool balances | grep -E "$user_regex" | cut -d' ' -f1)"

if [ "$(expr "$amount_source" '>' 0)" != 1 ]; then
    echo "error: source does not have a positive balance"
    exit 1
fi

echo "Source amount: $amount_source"

members_current=""
members_current_n=0
members="$(./fakeconcieggs eggstool balances | cut -d' ' -f2)"
for member in $members; do
    days_old="$(expr "$(expr "$(date +%s)" - "$(date +%s -d "$(./fakeconcieggs eggstool lunches "$member" 2>/dev/null| head -n 1)")")" / "$(expr 60 '*' 60 '*' 24)")"
    if [ "$(expr "$days_old" '<=' "$active_days_threshold")" = 1 ]; then
        members_current="$members_current $member"
        members_current_n="$(expr "$members_current_n" + 1)"
    fi
done

echo "Number of current members: $members_current_n"
echo "Current members:$members_current"

amount_per_target="$({ echo 'scale=2'; echo "$amount_source" / "$members_current_n"; } | bc)"

echo "Amount per target: $amount_per_target"

when="$(date '+%Y-%m-%d')"
for member in $members_current; do
    echo "Transaction of $amount_per_target kr. from $user_source to $member."
    ./fakeconcieggs transfer "$when" "$amount_per_target" "$user_source" "$member" 2>&1 >/dev/null
done

echo "All transactions were successful."

if ./fakeconcieggs gitRefresh; then
    if ./fakeconcieggs tryGitChange "Forældelses-overførsel fra $user_source til de aktive medlemmer $members_current, godkendt af $(whoami)" slashdotfrokost; then
        true
    else
        ./fakeconcieggs gitRepair slashdotfrokost
        exit 1
    fi
else
    exit 1
fi

echo "The changes have been committed and pushed."
