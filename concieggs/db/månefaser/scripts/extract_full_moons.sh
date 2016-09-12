#!/bin/sh

month_from_name() {
    month_name="$1"

    case "$month_name" in
        Jan)
            echo 01;;
        Feb)
            echo 02;;
        Mar)
            echo 03;;
        Apr)
            echo 04;;
        May)
            echo 05;;
        Jun)
            echo 06;;
        Jul)
            echo 07;;
        Aug)
            echo 08;;
        Sep)
            echo 09;;
        Oct)
            echo 10;;
        Nov)
            echo 11;;
        Dec)
            echo 12;;
        *)
            exit
    esac
}

parse_date() {
    # Input like: 12 Jan
    day="$1"
    if [ "$day" = "&nbsp;" ]; then
        exit
    fi
    day="$(printf %02d "$day")"
    month_name="$2"
    month="$(month_from_name "$month_name")"
    echo "$month-$day"
}

extract_yearly_full_moons() {
    echo "Extracting full moons from $year." > /dev/stderr
    year="$1"
    curl --header 'Accept-Language: en-US' \
         --silent https://www.timeanddate.com/moon/phases/denmark/copenhagen?year=$year \
        | grep -oP '<table class="tb-sm zebra fw tb-hover">.+?</table>' \
        | grep -oP '<td.+?>.+?</td>' \
        | grep -v colspan \
        | grep -oP '(?<=>).+?(?=<)' \
        | {
        for i in $(seq 1 13); do
            read new_moon_date
            read new_moon_time
            read first_quarter_date
            read first_quarter_time
            read full_moon_date
            read full_moon_time
            read third_quarter_date
            read third_quarter_time
            read duration
            full_moon_date="$(parse_date $full_moon_date)"
            if [ "$full_moon_date" ]; then
                echo "$year-$full_moon_date"
            fi
        done
    }
}

extract_full_moons() {
    start_year="$1"
    end_year="$2"
    for year in $(seq $start_year $end_year); do
        extract_yearly_full_moons $year
        sleep 1
    done
}

scripts_dir="$(dirname "$0")"

extract_full_moons 2016 2039 > "$scripts_dir/../full_moons_copenhagen.txt"
