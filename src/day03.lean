structure Number where
  num : Nat
  row : Nat
  span : Nat × Nat
deriving Repr

structure Symbol where
  row : Nat
  column : Nat
  isGear : Bool
deriving Repr

instance : ToString Symbol where
  toString := reprStr
instance : ToString Number where
  toString := reprStr

def extractNumbers (s : String) (row : Nat) : List Number :=
  let numbers := List.foldl (fun acc (i, c) => if c.isDigit then (match acc.getLast? with
    | some l => acc.dropLast ++ (if l.span.snd != i - 1 then
        [l, Number.mk (c.val - '0'.val).toNat row (i, i)]
      else
        [{l with num := l.num * 10 + (c.val - '0'.val).toNat, span := (l.span.fst, i)}])
    | none => [Number.mk (c.val - '0'.val).toNat row (i, i)]
  ) else acc) List.nil s.data.enum
  numbers

def extractSymbols (s : String) (row : Nat) : List Symbol :=
  List.foldl (fun acc (i, c) => if !c.isDigit && c != '.' then acc ++ [Symbol.mk row i (c == '*')] else acc) List.nil s.data.enum

def inSpan (span : Nat × Nat) (n : Nat)  : Bool :=
  n >= span.fst && n <= span.snd

def isAdjacent (n : Number) (s : Symbol) : Bool :=
  let inside := inSpan n.span
  n.row == s.row && (n.span.snd == s.column-1 || n.span.fst == s.column+1) ||
  n.row == s.row-1 && (inside s.column || inside (s.column - 1) || inside (s.column + 1)) ||
  n.row == s.row+1 && (inside s.column || inside (s.column - 1) || inside (s.column + 1))

def flatten (l : List (List α)) : List α := List.foldl List.append List.nil l

def part1 (numbers : List Number) (symbols : List Symbol) : Nat :=
  let digits := List.map (fun e => List.filter (isAdjacent · e) numbers) symbols |> flatten
  List.foldl (· + Number.num ·) 0 digits

def part2 (numbers : List Number) (symbols : List Symbol) : Nat :=
  let gears := List.filter Symbol.isGear symbols
  let digits := List.map (fun e => List.filter (isAdjacent · e) numbers) gears
  let doubles := List.filterMap (fun
    | first :: second :: _ => some (first.num * second.num)
    | _ => none
  ) digits
  List.foldl (· + ·) 0 doubles

def main : IO Unit := do
  let file ← IO.FS.readFile "input/day03.txt"
  let lines := file.split (· == '\n')
  let numbers := List.map (fun (i, e) => extractNumbers e i) lines.enum |> flatten
  let symbols := List.map (fun (i, e) => extractSymbols e i) lines.enum |> flatten

  println! s!"\tPart 1: {(part1 numbers symbols)}"
  println! s!"\tPart 2: {(part2 numbers symbols)}"
