import std/os/file
import std/os/path
import std/text/parse

value struct step
  label : list<char>
  operation : operation

value type operation
  Assign(value : int)
  Remove

fun parse-input() : <parse> list<step>
  fun separated1(s, p)
    Cons(p(), many({ s(); p() }))
  
  val steps = separated1({ char(',') })
    val label = many1(alpha)
    val operation = choose([
      { char('-'); Remove },
      { char('='); Assign(pnat()) }
    ])

    Step(label, operation)
  
  steps

pub fun main()
  val content = read-text-file("input" / "day15.txt").split("\n").head("")
  val input = match parse(content.slice, parse-input)
      ParseOk(res, _) -> res
      ParseError(msg, _) -> throw(msg)
  
  println("\tPart 1:" ++ part1(content.split(",").map(list)).show)
  println("\tPart 2:" ++ part2(input).show)

fun hash(s : list<char>)
  s.foldl(0, fn(acc, curr) ((acc + curr.int) * 17) % 256)

fun part1(input : list<list<char>>)
  match input
    Cons(h, t) -> hash(h) + part1(t)
    Nil -> 0

value struct slot
  label : list<char>
  focal-length : int

fun list-eq(a : list<t>, b : list<t>, eq : (t, t) -> bool) : bool
  a.length == b.length && a.zip(b).all(fn((p, q)) eq(p, q))

fun indexed(l : list<a>, start=0) : list<(int, a)>
  match l
    Cons(h, t) -> Cons((start, h), indexed(t, start+1))
    Nil -> Nil

fun part2(input : list<step>)
  fun run(idx : int, step : step, boxes : list<list<slot>>)
    fun removed(l : list<char>, slots : list<slot>)
      slots.filter(fn(e) !list-eq(e.label, l, (==))) 
    fun inserted(l : list<char>, v : int, slots : list<slot>)
      match slots
        Cons(h, t) -> if list-eq(h.label, l, (==)) then Cons(Slot(l, v), t) else Cons(h, inserted(l, v, t))
        Nil -> [Slot(l, v)]

    if idx != 0 then Cons(boxes.head([]), run(idx - 1, step, boxes.tail))
    else match step.operation
      Assign(value) -> Cons(inserted(step.label, value, boxes.head([])), boxes.tail)
      Remove -> Cons(removed(step.label, boxes.head([])), boxes.tail)
  
  val boxes = input.foldl([], fn(acc, curr) run(hash(curr.label), curr, acc))
  
  boxes.indexed(1).foldl(0) fn(acc, (i, curr))
    acc + curr.indexed(1).foldl(0) fn(sum, (j, slt))
      sum + (i * j * slt.focal-length)
