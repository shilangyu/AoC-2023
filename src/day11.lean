inductive Space where
  | empty
  | galaxy
deriving Repr, BEq, Inhabited

instance : ToString (List (List Space)) where
  toString := (String.intercalate "\n") ∘ List.map (String.mk ∘ List.map (fun | Space.galaxy => '#' | Space.empty => '.'))

partial def transpose [Inhabited α] : List (List α) → List (List α)
  | []
  | [] :: _ => []
  | rows    =>
      List.map List.head! rows :: transpose (List.map List.tail! rows)

def parseInput (s : String) : List Space :=
  List.map (fun | '#' => Space.galaxy | _ => Space.empty) s.toList

def emptyRows (l : List (List Space)) : List Nat :=
  List.filterMap (fun (i, e) => if List.all e (BEq.beq Space.empty) then some i else none) l.enum

def flatten (l : List (List α)) : List α := List.foldl List.append List.nil l

def manhattanDistance (p1 p2 : Nat × Nat) : Nat :=
  (diff p1.1 p2.1) + (diff p1.2 p2.2)
where
  diff a b := (Nat.max a b) - (Nat.min a b)

def countInBetween (low high : Nat) : (items : List Nat) → Nat
  | h :: t => (h <= high && h >= low).toUInt64.toNat + countInBetween low high t
  | [] => 0

def extraExpansionCount (p1 p2 : Nat × Nat) (emptyRows : List Nat) (emptyColumns : List Nat) : Nat :=
  countInBetween (Nat.min p1.1 p2.1) (Nat.max p1.1 p2.1) emptyRows + countInBetween (Nat.min p1.2 p2.2) (Nat.max p1.2 p2.2) emptyColumns

def expandedDistances (e : (Nat × Nat)) (H₀ : Nat) (emptyRows : List Nat) (emptyColumns : List Nat) : (l : List (Nat × Nat)) → Nat
  | h :: t => (manhattanDistance e h) + (extraExpansionCount e h emptyRows emptyColumns)*(H₀-1) + expandedDistances e H₀ emptyRows emptyColumns t
  | [] => 0

def galaxyPositions (l : List (List Space)) := List.map (
  fun (i, e) => List.map (fun
    | (_, Space.empty) => none
    | (j, Space.galaxy) => some (i, j)
  ) e.enum) l.enum |> flatten |> List.filterMap id

def expandingDistance (input : List (List Space)) (H₀ : Nat) : Nat :=
  let r := emptyRows input
  let c := emptyRows (transpose input)
  let pos := galaxyPositions input

  let rec d := fun
    | h :: t => (expandedDistances h H₀ r c t) + d t
    | [] => 0

  d pos

def part1 := (expandingDistance · 2)
def part2 := (expandingDistance · 1000000)

def main : IO Unit := do
  let file ← IO.FS.readFile "input/day11.txt"
  let input := List.map parseInput (file.splitOn "\n")

  println! s!"\tPart 1: {(part1 input)}"
  println! s!"\tPart 2: {(part2 input)}"
