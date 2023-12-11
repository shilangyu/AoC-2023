import std/os/file
import std/os/path
import std/text/parse

type move
  L
  R

value struct node
  head : int
  left : int
  right : int

value struct network
  navigation : list<move>
  nodes : list<node>

fun int-coord(x1, x2, x3)
  // base26 representation of the coord
  (x3 - 'A').int + 26 * (x2 - 'A').int + 26^2 * (x1 - 'A').int

fun ends-with(coord, c)
  (coord % 26) == (c - 'A').int

fun find-node(coord : int, nodes : list<node>) : maybe<node>
  match nodes
    Cons(h, t) -> if h.head == coord then Just(h) else find-node(coord, t)
    Nil -> Nothing

fun gcd(a, b)
  if a == 0 then b else gcd(b % a, a)

fun lcm(a, b)
  (a / gcd(a, b)) * b

fun follow-moves(network : network, start-coord : int, end-cond : (int) -> bool)
  fun follow(all-moves : list<move>, left-moves : list<move>, coord : int, nodes : list<node>, move-count : int)
    if end-cond(coord) then move-count else match left-moves
      Cons(h, t) -> match find-node(coord, nodes)
        Just(n) -> match h
          L -> follow(all-moves, t, n.left, nodes, move-count+1)
          R -> follow(all-moves, t, n.right, nodes, move-count+1)
        Nothing -> throw("could not find node")
      Nil -> follow(all-moves, all-moves, coord, nodes, move-count)
  follow(network.navigation, network.navigation, start-coord, network.nodes, 0)

fun parse-input() : <parse> network
  fun newline()
    char('\n')

  fun separated1(p, s)
    Cons(p(), many({ s(); p() }))
  
  val navigation = many1
    ({ char('R'); R }) || ({ char('L'); L })

  newline()
  newline()

  fun coord()
    int-coord(alpha(), alpha(), alpha())

  val nodes = many1
    val head = coord()
    pstring(" = (")
    val left = coord()
    pstring(", ")
    val right = coord()
    pstring(")")
    newline()

    Node(head, left, right)

  Network(navigation, nodes)

pub fun main()
  val input = match parse(read-text-file("input" / "day08.txt").slice, parse-input)
    ParseOk(res, _) -> res
    ParseError(msg, _) -> throw(msg)

  println("\tPart 1:" ++ part1(input).show)
  println("\tPart 2:" ++ part2(input).show)

fun part1(input : network)
  val aaa = int-coord('A', 'A', 'A')
  val zzz = int-coord('Z', 'Z', 'Z')

  follow-moves(input, aaa, fun(c) c == zzz)

fun part2(input : network)
  fun starting-coords(nodes : list<node>) : list<int>
    match nodes
      Cons(h, t) -> if h.head.ends-with('A') then Cons(h.head, starting-coords(t)) else starting-coords(t)
      Nil -> []

  val cycle-lengths = starting-coords(input.nodes).map fn(start)
    follow-moves(input, start, fun(c) c.ends-with('Z'))

  cycle-lengths.foldl(1, lcm)
