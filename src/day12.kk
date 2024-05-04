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

fun spring/(==)(a : spring, b : spring) : bool
  match (a, b)
    (Operational, Operational)   -> True
    (Damaged, Damaged) -> True
    (Unknown, Unknown) -> True
    _              -> False

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


fun arrangement-count(record : record) : int
  fun count(springs : list<spring>, damaged : list<int>) : div int
    match (springs, damaged)
      (Nil, _) -> damaged.is-empty.int
      (_, Nil) -> springs.all(fn(e) !(e == Damaged)).int
      (Cons(Operational, t), _) -> count(t, damaged)
      (Cons(Unknown, t), _) -> count(t, damaged) + count(Cons(Damaged, t), damaged)
      (Cons(Damaged, st), Cons(dh, dt)) -> if st.length >= dh - 1 && st.take(dh - 1).all(fn(e) !(e == Operational)) && st[dh - 1].maybe(True, fn(e) !(e == Damaged)) then count(st.drop(dh), dt) else 0
  unsafe-total({ count(record.springs, record.damaged) })

fun part1(input : list<record>)
  input.foldl(0, fn(acc, curr) acc + arrangement-count(curr))

fun part2(input : list<record>)
  val augmented = input.map fn(e)
    Record(
      e.springs ++ [Unknown] ++ e.springs ++ [Unknown] ++ e.springs ++ [Unknown] ++ e.springs ++ [Unknown] ++ e.springs,
      e.damaged ++ e.damaged ++ e.damaged ++ e.damaged ++ e.damaged
    )
  part1(augmented)
