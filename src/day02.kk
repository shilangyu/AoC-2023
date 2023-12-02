import std/os/file
import std/os/path
import std/text/parse

value struct game
  identifier : int
  sets : list<cubes>

value struct cubes
  red : int = 0
  green : int = 0
  blue : int = 0

fun (+)(a : cubes, b : cubes) : cubes
  Cubes(a.red + b.red, a.green + b.green, a.blue + b.blue)

fun (<=)(a : cubes, b : cubes) : bool
  a.red <= b.red && a.green <= b.green && a.blue <= b.blue

fun show(a : cubes) : string
  "Cubes(" ++ a.red.show ++ ", " ++ a.green.show ++ ", " ++ a.blue.show ++ ")"

fun show(a : list<a>, s : (a) -> string) : string
  "[" ++ a.map(s).join(", ") ++ "]"

fun show(a : game) : string
  "Game(" ++ a.identifier.show ++ ", " ++ show(a.sets, show) ++ ")"

fun supremum(g : game) : cubes
  g.sets.foldr(Cubes(), fn(c, e) Cubes(max(c.red, e.red), max(c.green, e.green), max(c.blue, e.blue)))

fun power(g : cubes) : int
  g.red * g.green * g.blue

fun parse-input() : <div,parse> game
  pstring("Game ")
  val identifier = pnat()
  pstring(": ")

  fun set()
    fun picks()
      val amount = pnat()
      char(' ')
      val res = choose([
        { pstring("red"); Cubes(red = amount) },
        { pstring("green"); Cubes(green = amount) },
        { pstring("blue"); Cubes(blue = amount) }
      ])

      val rest = optional(Nothing, {
        pstring(", ")
        Just(picks())
      })
      
      match rest
        Just(r) -> Cons(res, r)
        Nothing -> Cons(res, [])

    picks().foldl(Cubes(), (+))
  
  val sets = many1
    val s = set()
    choose([{ pstring("; "); () }, eof])
    s
  
  Game(identifier, sets)


pub fun main()
  val input = read-text-file("input" / "day02.txt").split("\n").map fn(line)
    match parse(line.slice, parse-input)
      ParseOk(res, _) -> res
      ParseError(msg, _) -> throw(msg)

  println("\tPart 1:" ++ part1(input).show)
  println("\tPart 2:" ++ part2(input).show)

fun part1(input : list<game>)
  val allowed = Cubes(12, 13, 14)
  input.filter(fn(e) e.sets.all(fn(set) set <= allowed)).map(fn(e) e.identifier).sum

fun part2(input : list<game>)
  input.map(supremum).map(power).sum
