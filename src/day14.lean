inductive Space where
  | rolling
  | fixed
  | empty
deriving Repr, Inhabited, BEq

instance : ToString (List (List Space)) where
  toString := (String.intercalate "\n") ∘ List.map (String.mk ∘ List.map (fun | Space.fixed => '#' | Space.empty => '.' | Space.rolling => 'O'))


partial def transpose [Inhabited α] : List (List α) → List (List α)
  | []
  | [] :: _ => []
  | rows    =>
      List.map List.head! rows :: transpose (List.map List.tail! rows)

def rotateLeft [Inhabited α] : List (List α) → List (List α) := List.reverse ∘ transpose
def rotateRight [Inhabited α] : List (List α) → List (List α) := transpose ∘ List.reverse


def parseInput (s : String) : List (List Space) :=
  List.map (fun e => List.map parse e.toList) (s.splitOn "\n")
where
  parse := fun
    | 'O' => Space.rolling
    | '#' => Space.fixed
    | _ => Space.empty

partial def part1 (l : List (List Space)) :=
  List.map load (transpose l) |> List.foldl (· + ·) 0
where
  load row :=
    let untilStuck := List.takeWhile (· != Space.fixed) row
    let count := List.filter (· == Space.rolling) untilStuck |> List.length
    -- arithmetic sequence of length `count` starting from row.length and decreasing by one:
    -- ∑_k=0^count (row.length - k)
    let thisLoad := List.iota row.length |> List.take count |> List.foldl (· + ·) 0

    thisLoad + (if row.isEmpty then 0 else load (List.drop (untilStuck.length + 1) row))

partial def rollWest (l : List (List Space)) : List (List Space) :=
  List.map rollRow l
where
  rollRow := fun
    | [] => []
    | Space.fixed :: t => Space.fixed :: rollRow t
    | row =>
      let untilStuck := List.takeWhile (· != Space.fixed) row
      let count := List.filter (· == Space.rolling) untilStuck |> List.length
      (List.replicate count Space.rolling)
      ++ (List.replicate (untilStuck.length - count) Space.empty)
      ++ rollRow (List.drop untilStuck.length row)

def cycle : (l : List (List Space)) → List (List Space) :=
  rotateRight ∘ rotateRight ∘ rollWest ∘ rotateRight ∘ rollWest ∘ rotateRight ∘ rollWest ∘ rotateRight ∘ rollWest ∘ rotateLeft

def part2 (l : List (List Space)) :=
  0

def main : IO Unit := do
  let file ← IO.FS.readFile "input/day14.txt"
  let input := parseInput file

  println! s!"\tPart 1: {(part1 input)}"
  println! s!"\tPart 2: {(part2 input)}"
