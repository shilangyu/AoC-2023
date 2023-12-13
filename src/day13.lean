partial def transpose [Inhabited α] : List (List α) → List (List α)
  | []
  | [] :: _ => []
  | rows    =>
      List.map List.head! rows :: transpose (List.map List.tail! rows)

def parseInput (s : String) : List (List Bool) :=
  List.map (fun e => List.map (· == '#') e.toList) (s.splitOn "\n")

def differenceCountRow (left right : List Bool) := match left, right with
  | h1 :: t1, h2 :: t2 => (h1 != h2).toUInt64.toNat + differenceCountRow t1 t2
  | _, _ => 0

def differenceCount (left right : List (List Bool)) := match left, right with
  | h1 :: t1, h2 :: t2 => differenceCountRow h1 h2 + (differenceCount t1 t2)
  | _, _ => 0

def symmetryCount (l : List (List Bool)) (smudgeCount : Nat) := count [] l smudgeCount
where
  count left right smudgeCount :=
    let s := Nat.min left.length right.length
    (if  differenceCount (left.reverse.take s) (right.take s) == smudgeCount then left.length else 0) + match right with
      | h1 :: h2 :: t => count (left ++ [h1]) (h2 :: t) smudgeCount
      | _ => 0

def summary (l : List (List (List Bool))) (smudgeCount : Nat) :=
  (List.foldl (fun acc curr => acc + (symmetryCount (transpose curr) smudgeCount)) 0 l) +
  100 * (List.foldl (fun acc curr => acc + (symmetryCount curr smudgeCount)) 0 l)

def part1 (l : List (List (List Bool))) :=
  summary l 0

def part2 (l : List (List (List Bool))) :=
  summary l 1

def main : IO Unit := do
  let file ← IO.FS.readFile "input/day13.txt"
  let input := List.map parseInput (file.splitOn "\n\n")

  println! s!"\tPart 1: {(part1 input)}"
  println! s!"\tPart 2: {(part2 input)}"
