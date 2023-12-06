structure Race where
  time : Nat
  best : Nat
deriving Repr

instance : ToString Race where
  toString := reprStr


def extractNumbers (s : String) : List Nat :=
  List.map (String.toNat!) (List.filter (· != "") (List.map String.trim (s.splitOn " ")))

def parseInput (s : String) : List Race :=
  let lines := s.split (· == '\n')
  let times := lines.head!.splitOn ":" |> List.getLast! |> extractNumbers
  let bests := lines.tail!.head!.splitOn ":" |> List.getLast! |> extractNumbers

  List.zipWith Race.mk times bests

def realRoots (a b c : Float) : Option (Float × Float) :=
  let delta := b*b - 4 * a * c
  if delta < 0 then none
  else ((-b + delta.sqrt)/(2*a), (-b - delta.sqrt)/(2*a))

-- we need to find the amount of solutions over the naturals of:
-- x*(time - x) > best <=> x*(time - x) - best > 0 <=> x*(time - x) - best - 1 >= 0
-- so we find the roots of x*(time - x) - best - 1 = -x^2 + x*time - best - 1
-- a < 0 so we get a parabola with a global maximum
-- the roots (if exist within reals) indicate the range of values
-- that beat the best time
def winsCount (time best : Nat) : Option Nat := match realRoots (-1.0) (Float.ofNat time) (-(Float.ofNat (best + 1))) with
    | some (a, b) => some (b.floor.toUInt64.toNat - a.ceil.toUInt64.toNat + 1)
    | none => none

def part1 : (input : List Race) → Nat
  | race :: t => (winsCount race.time race.best).getD 1 * part1 t
  | [] => 1

def part2 (input : List Race) : Nat :=
  let merged := List.foldl (fun acc race => (acc.fst.append race.time.repr, acc.snd.append race.best.repr)) ("", "") input
  (winsCount merged.fst.toNat! merged.snd.toNat!).getD 0


def main : IO Unit := do
  let file ← IO.FS.readFile "input/day06.txt"
  let input := parseInput file

  println! s!"\tPart 1: {(part1 input)}"
  println! s!"\tPart 2: {(part2 input)}"
