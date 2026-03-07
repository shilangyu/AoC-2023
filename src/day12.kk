import std/os/file
import std/os/path
import std/data/hash
import std/data/mutable/hashmap-mut
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
    (Operational, Operational) -> True
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

alias cache-key = (springs : list<spring>, damaged : list<int>)

fun hash-cache-key(key : cache-key, ?seed : int64) : int
  val (_, damaged) = key
  // just hashing damaged is approximate enough
  damaged.list/hash(int/hash)

effect cache
  fun lookup(key : cache-key) : maybe<int>
  fun save(key : cache-key, value : int) : ()

fun arrangement-count(record : record) : <cache,div> int
  fun count(springs : list<spring>, damaged : list<int>) : <cache,div> int
    val key = (springs, damaged)
    match lookup(key)
      Just(v) -> v
      Nothing ->
        if springs.length == 0 then return damaged.is-empty.int
        if damaged.length == 0 then return springs.any(fn(e) e == Damaged).xor(True).int

        val val1 = match springs
          Cons(Operational, rest) -> count(rest, damaged)
          Cons(Unknown, rest) -> count(rest, damaged)
          _ -> 0

        val val2 = match (springs, damaged)
          (Cons(Unknown, _), Cons(n, dt)) ->
            if n <= springs.length && springs.take(n).all(fn(e) !(e == Operational)) && (n == springs.length || springs[n] != Just(Damaged)) then
              count(springs.drop(n+1), dt)
            else 0
          (Cons(Damaged, _), Cons(n, dt)) ->
            if n <= springs.length && springs.take(n).all(fn(e) !(e == Operational)) && (n == springs.length || springs[n] != Just(Damaged)) then
              count(springs.drop(n+1), dt)
            else 0
          _ -> 0

        val value = val1 + val2
        save(key, value)
        value

  count(record.springs, record.damaged)

fun part1(input : list<record>)
  var cache := hash-map-mut(1000)

  with
    fun lookup(key) cache.get(key, hash-cache-key)
    fun save(key, value) cache := cache.insert(key, value, hash-cache-key)

  input.foldl(0, fn(acc, curr) acc + arrangement-count(curr))

fun part2(input : list<record>)
  val augmented = input.map fn(e)
    Record(
      e.springs ++ [Unknown] ++ e.springs ++ [Unknown] ++ e.springs ++ [Unknown] ++ e.springs ++ [Unknown] ++ e.springs,
      e.damaged ++ e.damaged ++ e.damaged ++ e.damaged ++ e.damaged
    )
  part1(augmented)
