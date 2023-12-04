structure ScratchCard where
  winning : List Nat
  picked : List Nat
deriving Repr

instance : ToString ScratchCard where
  toString := reprStr


def extractNumbers (s : String) : List Nat :=
  List.map (String.toNat!) (List.filter (· != "") (List.map String.trim (s.splitOn " ")))

def parseInput (s : String) : List ScratchCard :=
  let lines := s.split (· == '\n')
  let numbers := List.map (
    fun e => e.splitOn ": " |>
    (List.get! · 1) |>
    (String.splitOn · " | ") |>
    (fun e => (List.get! e 0 |> String.trim, List.get! e 1 |> String.trim))) lines

  List.map (fun (a, b) => ScratchCard.mk (extractNumbers a) (extractNumbers b)) numbers

def matching (sc : ScratchCard) : Nat := List.length (List.filter sc.picked.contains sc.winning)

def part1 (input : List ScratchCard) : Nat :=
  List.foldl (· + ·) 0 (List.map (fun e => 2^(e-1)) (List.filter (· != 0) (List.map matching input)))

def addWins : (matchings : List (Nat × Nat)) → (amount : Nat) → (count : Nat) → List (Nat × Nat)
  | rest, 0, _ => rest
  | [], _, _ => []
  | (wins, am) :: t, n+1, c => (wins, am+c) :: addWins t n c

partial def accumulateWins (matchings : List (Nat × Nat)) := match matchings with
  | (wins, count) :: rest => count + (accumulateWins (addWins rest wins count))
  | [] => 0

def part2 (input : List ScratchCard)  :=
  List.map (fun e => (e, 1)) (List.map matching input) |> accumulateWins


def main : IO Unit := do
  let file ← IO.FS.readFile "input/day04.txt"
  let input := parseInput file

  println! s!"\tPart 1: {(part1 input)}"
  println! s!"\tPart 2: {(part2 input)}"
