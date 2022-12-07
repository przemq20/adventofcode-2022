day=7
mkdir "src/main/scala/day$day"
mkdir "src/main/resources/day$day"
touch "src/main/resources/day$day/input.txt"
touch "src/main/resources/day$day/test-input.txt"
touch "src/main/resources/day$day/README.md"
# shellcheck disable=SC2027
touch "src/test/scala/Day"$day"Spec.scala"