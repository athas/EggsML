#!/bin/sh

# finish at your own risk, something like this:

{
    IIPASS=concieggsersej ii -s localhost -i ircdir -n concieggs -f 'Concieggs Nippenauer' -k IIPASS

    sleep 2

    cd ircdir/localhost

    wait $!
} &

concieggs() {
    while read -r line; do
        user=$(echo "$line" | cut -d' ' -f3 | sed s'/^<//' | sed s'/>$//')
        message=$(echo "$line" | cut -d' ' -f4-)

        if [ $user = concieggs ]; then
            continue # ignore concieggs' own messages
        fi

        # call concieggs in some way
        ... ./fakeconcieggs runcmd ...
    done
}

inotifywait -e create --monitor --format %w%f . \
    | while read -r dir; do
    echo inotify "$dir"
    dir="$(basename "$dir")"
    if [ -d "$dir" ] && ! [ -r "$dir/watched" ] && [ "$dir" != "nickserv" ]; then
        touch "$dir/watched"
        (cd "$dir"; touch out; (tail -f out -n 1 | concieggs > in)) &
    fi
done
