import std/os/file
import std/os/path
import std/text/parse

type spring
  Operational
  Damaged
  Unknown

value struct record
  springs : list<spring>
  damaged : list<int>

fun parse-input() : <parse> record
  fun newline()
    char('\n')

  fun separated1(p, s)
    Cons(p(), many({ s(); p() }))
  
  val springs = many1
    choose([
      { char('.'); Operational },
      { char('#'); Damaged },
      { char('?'); Unknown },
    ])
  
  char(' ')

  val damaged = separated1(pnat, { char(',') })

  Record(springs, damaged)

pub fun main()
  val content = read-text-file("input" / "day12.txt").split("\n")
  val input = content.map fn(e)
    match parse(e.slice, parse-input)
      ParseOk(res, _) -> res
      ParseError(msg, _) -> throw(msg)

  println("\tPart 1:" ++ part1(input).show)
  println("\tPart 2:" ++ part2(input).show)


fun arrangement-count(record : record)
  fun count(springs : list<spring>, damaged : list<int>, streak : bool)
    match springs
      Cons(h, t) -> match h
        Operational ->
          if streak then (if damaged.head(-1) != 0 then 0 else count(t, damaged.tail, False))
          else count(t, damaged, False)
        Damaged ->
          if streak then (if damaged.head(-1) == 0 then 0 else count(t, Cons(damaged.head(-1) - 1, damaged.tail), True))
          else (if damaged.is-empty then 0 else count(t, Cons(damaged.head(-1) - 1, damaged.tail), True))
        Unknown ->
          if streak then (if damaged.head(-1) == 0 then count(t, damaged.tail, False)
          else count(t, Cons(damaged.head(-1) - 1, damaged.tail), True)) else (if damaged.is-empty then count(t, [], False) /* thats where we split into two possibilities */ else (count(t, Cons(damaged.head(-1) - 1, damaged.tail), True)) + count(t, damaged, False))
      Nil ->
        if damaged.is-empty || (damaged.length == 1 && damaged.head(-1) == 0) then 1
        else 0
  
  count(record.springs, record.damaged, False)

fun part1(input : list<record>)
  input.foldl(0, fn(acc, curr) acc + arrangement-count(curr))

fun part2(input : list<record>)
  val augmented = input.map fn(e)
    Record(
      e.springs ++ [Unknown] ++ e.springs ++ [Unknown] ++ e.springs ++ [Unknown] ++ e.springs ++ [Unknown] ++ e.springs,
      e.damaged ++ e.damaged ++ e.damaged ++ e.damaged ++ e.damaged
    )
  augmented.foldl(0, fn(acc, curr) acc + arrangement-count(curr))
