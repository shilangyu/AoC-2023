import std/os/file
import std/os/path
import std/text/regex

pub fun main()
  val input = read-text-file("input" / "day01.txt").split("\n")

  println("\tPart 1:" ++ part1(input).show)
  println("\tPart 2:" ++ part2(input).show)


fun part1(input : list<string>)
  input.map fn(e) {
    val digits = e.list.filter(is-digit)
    [digits.head('0'), digits.last().default('0')].string.parse-int.default(0)
  }.sum

fun part2(input : list<string>)
  fun as-digit(s : sslice)
    val d = s.string
    
    if d == "one" then "1"
    else if d == "two" then "2"
    else if d == "three" then "3"
    else if d == "four" then "4"
    else if d == "five" then "5"
    else if d == "six" then "6"
    else if d == "seven" then "7"
    else if d == "eight" then "8"
    else if d == "nine" then "9"
    else d

  input.map fn(e) {
    val first = regex("^.*?(one|two|three|four|five|six|seven|eight|nine|\\d).*").exec(e).tail.head("0".slice).as-digit
    val last = regex("^.*(one|two|three|four|five|six|seven|eight|nine|\\d).*?").exec(e).tail.head("0".slice).as-digit
    (first ++ last).parse-int.default(0)
  }.sum
  
