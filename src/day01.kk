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
    [digits.head('0'), digits.last('0')].string.parse-int-default(0)
  }.sum

fun part2(input : list<string>)
  fun repl(s : list<sslice>)
    val pre = s.tail.head("".slice).string
    val d = s.tail.tail.head("".slice).string
    val post = s.tail.tail.tail.head("".slice).string
    
    pre ++ (if d == "one" then "1"
    else if d == "two" then "2"
    else if d == "three" then "3"
    else if d == "four" then "4"
    else if d == "five" then "5"
    else if d == "six" then "6"
    else if d == "seven" then "7"
    else if d == "eight" then "8"
    else if d == "nine" then "9"
    else "") ++ post

  val mapped = input.map fn(e)
    e.replace(regex("^(\\D*?)(one|two|three|four|five|six|seven|eight|nine)(.*)$"), repl)
     .replace(regex("^(.*)(one|two|three|four|five|six|seven|eight|nine)(\\D*?)$"), repl)
  part1(mapped)
  
