def List.pairs : (l : List α) → List (α × α)
  | h1 :: h2 :: t => (h1, h2) :: List.pairs (h2 :: t)
  | _ => []

def parseInput (s : String) : List Int :=
  List.map String.toInt! (s.splitOn " ")

def reduce (l : List Int) : List Int :=
    List.map (fun (a, b) => b-a) (List.pairs l)

partial def part1 (input : List (List Int)) :=
  List.map reconstruct input |> List.foldl (· + ·) 0
where
  reconstruct (l : List Int) :=
    let r := reduce l
    (l.getLastD 0) + if r.all (BEq.beq 0) then
      0
    else
      reconstruct r

partial def part2 (input : List (List Int)) :=
  List.map reconstruct input |> List.foldl (· + ·) 0
where
  reconstruct (l : List Int) :=
    let r := reduce l
    (l.headD 0) - if r.all (BEq.beq 0) then
      0
    else
      reconstruct r

def main : IO Unit := do
  let file ← IO.FS.readFile "input/day09.txt"
  let input := List.map parseInput (file.splitOn "\n")

  println! s!"\tPart 1: {(part1 input)}"
  println! s!"\tPart 2: {(part2 input)}"
