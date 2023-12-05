import std/os/file
import std/os/path
import std/text/parse

value struct almanac
  seeds : list<(int, int)>
  maps : list<list<map-entry>>

value struct map-entry
  dst-start : int
  src-start : int
  length : int

fun parse-input() : <parse> almanac
  fun newline()
    char('\n')

  fun separated1(p, s)
    Cons(p(), many({ s(); p() }))
  
  fun mapping()
    many1(alpha)
    pstring("-to-")
    many1(alpha)
    pstring(" map:")
    newline()

    many1({
      val dst-start = pnat()
      char(' ')
      val src-start = pnat()
      char(' ')
      val length = pnat()
      newline()
      Map-entry(dst-start, src-start, length)
    })

  pstring("seeds: ")
  val seeds = separated1({ val a = pnat(); char(' '); val b = pnat(); (a, b) }, { char(' ') })
  newline()
  
  newline()

  val maps = separated1(mapping, newline)

  Almanac(seeds, maps)

pub fun main()
  val input = match parse(read-text-file("input" / "day05.txt").slice, parse-input)
    ParseOk(res, _) -> res
    ParseError(msg, _) -> throw(msg)

  println("\tPart 1:" ++ part1(input).show)
  println("\tPart 2:" ++ part2(input).show)

fun part1(input : almanac)
  input.seeds.map(fn((a, b)) [a, b]).concat.map(fn(e) follow(e, input.maps)).foldl1(min)

fun part2(input : almanac)
  // could be done by splitting ranges and constructing a tree where leaves are destination ranges
  // then we just take the minimum. However, cryptography has taught me that bruteforcing is ok,
  // so here we just go through smallest locations until we find something that maps to a seed.
  fun is-seed(seed : int, seeds : list<(int, int)>)
    match seeds
      Nil -> False
      Cons((s, l), h) -> seed >= s && seed < s + l || is-seed(seed, h)

  fun find(location : int, seeds : list<(int, int)>, maps : list<list<map-entry>>)
    val seed = retreat(location, maps)
    if is-seed(seed, seeds) then location else find(location+1, seeds, maps)
  
  find(0, input.seeds, input.maps.reverse)

fun follow(num : int, maps : list<list<map-entry>>)
  fun map(n : int, entries : list<map-entry>)
    match entries
      Nil -> n
      Cons(h, t) -> if n >= h.src-start && n < h.src-start + h.length then
          h.dst-start + (n - h.src-start)
        else
          map(n, t)
  match maps
    Nil -> num
    Cons(h, t) -> follow(map(num, h), t)

fun retreat(num : int, maps : list<list<map-entry>>)
  fun map(n : int, entries : list<map-entry>)
    match entries
      Nil -> n
      Cons(h, t) -> if n >= h.dst-start && n < h.dst-start + h.length then
          h.src-start + (n - h.dst-start)
        else
          map(n, t)
  match maps
    Nil -> num
    Cons(h, t) -> retreat(map(num, h), t)
