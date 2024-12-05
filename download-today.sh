#!/bin/bash
day_of_month=${2:-${AOC_DAY:-$(date +%-d)}}
year=${1:-${AOC_YEAR:-$(date +%Y)}}
cookies=${AOC_COOKIES:-"/home/a.belo/personal/clojure/aoc-2023/aoc_cookies.txt"}
directory="/home/a.belo/personal/clojure/aoc-2023/resources/$year"
filepath="$directory/$day_of_month"
aoc_url="https://adventofcode.com/$year/day/$day_of_month/input"
max_attempts=5
attempt=1
echo "Downloading adventofcode input for $year (DAY $day_of_month)"
mkdir -p $directory
while (( attempt <= max_attempts )); do
  response_code=$(curl -s -w "%{http_code}" -b $cookies -o $filepath $aoc_url)
  if [[ $response_code -eq 200 ]]; then
    echo "Successfully downloaded input for $year-$day file to $directory "
    exit 0
  fi
  echo "Curl request failed with response code: $response_code"
  attempt=$((attempt + 1))
  echo "Retrying in 5 seconds..."
  sleep 5
  echo "Attempt $attempt of $max_attempts..."
done
echo "All attempts failed. Exiting."
exit 1
