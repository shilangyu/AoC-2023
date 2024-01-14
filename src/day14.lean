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

def load (l : List (List Space)) :=
  List.map aux (transpose l) |> List.foldl (· + ·) 0
where
  aux row :=
    let rec aux2
      | [], _ => 0
      | h :: t, i => aux2 t (i-1) + if h == Space.rolling then i else 0
    aux2 row (List.length row)

partial def rollWest (l : List (List Space)) : List (List Space) :=
  List.map rollRow l
where
  rollRow
    | [] => []
    | Space.fixed :: t => Space.fixed :: rollRow t
    | row =>
      let untilStuck := List.takeWhile (· != Space.fixed) row
      let count := List.filter (· == Space.rolling) untilStuck |> List.length
      (List.replicate count Space.rolling)
      ++ (List.replicate (untilStuck.length - count) Space.empty)
      ++ rollRow (List.drop untilStuck.length row)

def part1 := load ∘ transpose ∘ rollWest ∘ transpose

def cycle : (l : List (List Space)) → List (List Space) :=
  rotateRight ∘ rotateRight ∘ rollWest ∘ rotateRight ∘ rollWest ∘ rotateRight ∘ rollWest ∘ rotateRight ∘ rollWest ∘ rotateLeft

abbrev Memory := List (List (List Space))

def indexOf := aux 0
where
  aux i l (m : Memory) := match m with
    | [] => none
    | h :: t => if h == l then some i else aux (i+1) l t

partial def part2 (l : List (List Space)) :=
  acc [l] l
where
  acc mem curr :=
    let new := cycle curr
    match indexOf new mem with
    | some i => load (List.get! mem (i + (1000000000 - i) % (List.length mem - i)))
    | none => acc (mem ++ [new]) new

def main : IO Unit := do
  let file ← IO.FS.readFile "input/day14.txt"
  let input := parseInput file

  println! s!"\tPart 1: {(part1 input)}"
  println! s!"\tPart 2: {(part2 input)}"
