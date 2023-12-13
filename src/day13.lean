partial def transpose [Inhabited α] : List (List α) → List (List α)
  | []
  | [] :: _ => []
  | rows    =>
      List.map List.head! rows :: transpose (List.map List.tail! rows)

def parseInput (s : String) : List (List Bool) :=
  List.map (fun e => List.map (· == '#') e.toList) (s.splitOn "\n")

def symmetryCount (l : List (List Bool)) := count [] l
where
  count left right :=
    let s := Nat.min left.length right.length
    (if left.reverse.take s == right.take s then left.length else 0) + match right with
      | h1 :: h2 :: t => count (left ++ [h1]) (h2 :: t)
      | _ => 0

def part1 (l : List (List (List Bool))) :=
  (List.foldl (fun acc curr => acc + (symmetryCount (transpose curr))) 0 l) +
  100 * (List.foldl (fun acc curr => acc + (symmetryCount curr)) 0 l)

def part2 (l : List (List (List Bool))) :=
  0

def main : IO Unit := do
  let file ← IO.FS.readFile "input/day13.txt"
  let input := List.map parseInput (file.splitOn "\n\n")

  println! s!"\tPart 1: {(part1 input)}"
  println! s!"\tPart 2: {(part2 input)}"
