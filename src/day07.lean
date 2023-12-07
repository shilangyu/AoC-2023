set_option tactic.hygienic false


inductive HandType where
  | highCard
  | two
  | twoPairs
  | three
  | fullHouse
  | four
  | five
deriving Repr, Ord, BEq, Inhabited

inductive Card where
  | n2
  | n3
  | n4
  | n5
  | n6
  | n7
  | n8
  | n9
  | T
  | J
  | Q
  | K
  | A
deriving Repr, Ord, BEq

def Card.ofChar? : (s : Char) → Option Card
  | 'A' => A
  | 'K' => K
  | 'Q' => Q
  | 'J' => J
  | 'T' => T
  | '9' => n9
  | '8' => n8
  | '7' => n7
  | '6' => n6
  | '5' => n5
  | '4' => n4
  | '3' => n3
  | '2' => n2
  | _ => none

-- a list carrying a proof of its length
structure FixedArray (α : Type) (n : Nat) where
  data : List α
  length : data.length == n
deriving Repr

def makeFixed (l : List α) (n : Nat) : Option (FixedArray α n) :=
  if h : l.length == n then some (FixedArray.mk l h) else none

abbrev HandCards := FixedArray Card 5

structure Hand where
  hand : HandCards
  handType : HandType
  bid : Nat
deriving Repr

instance [Repr α]: ToString α where
  toString := reprStr


def sort [Ord α] (l : List α) : List α :=
  l.toArray.insertionSort (fun a b => (Ord.compare a b).isLE) |> Array.toList

def uniqueAmounts [BEq α] (l : FixedArray α n) : List Nat := aux l.data []
where
  aux (l : List α) (u : List (α × Nat)) := match l, u with
    | [], u => List.map Prod.snd u
    | t :: h, u => aux h (add t u)
  add := fun
    | e, [] => [(e, 1)]
    | e, (p, c) :: t => if e == p then (p, c+1) :: t else (p,c) :: add e t

theorem zero_nil (l : List α) (_ : l.length == 0) : l = [] := match l with
  | [] => rfl

theorem nil_FixedArray [BEq α] (l : FixedArray α 0) : l.data = [] := by
  rw [zero_nil l.data l.length]

theorem nil_uniqueAmounts [BEq α] (l : FixedArray α 0) : (uniqueAmounts l).length = 0 := by
  rw [uniqueAmounts, nil_FixedArray, uniqueAmounts.aux, List.map, List.length]

theorem length_uniqueAmounts [BEq α] (l : FixedArray α n) : (uniqueAmounts l).length <= n := by
  induction n
  · rw [nil_uniqueAmounts, Nat.zero_eq, Nat.le_zero_eq]
  · sorry

def handType (hand : HandCards) : HandType :=
  match sort (uniqueAmounts hand) with
  | [5] => HandType.five
  | [1, 4] => HandType.four
  | [2, 3] => HandType.fullHouse
  | [1, 1, 3] => HandType.three
  | [1, 2, 2] => HandType.twoPairs
  | [1, 1, 1, 2] => HandType.two
  | [1, 1, 1, 1, 1] => HandType.highCard
  | _ => panic! "bad hand"

instance : Ord Hand where
  compare a b :=
    let rec aux := fun
      | h1 :: t1, h2 :: t2 => let cmp := compare h1 h2; if cmp == Ordering.eq then aux t1 t2 else cmp
      | _, _ => Ordering.eq

    let cmp := compare a.handType b.handType
    if cmp == Ordering.eq then aux a.hand.data b.hand.data else cmp

def parseInput (s : String) : Option Hand :=
  let parts := s.splitOn " "
  let hand := parts.head!.toList |> List.filterMap Card.ofChar? |> (makeFixed · 5)
  let bid := parts.tail!.head!.toNat!

  hand.map (fun e => Hand.mk e (handType e) bid)

def part1 (input : List Hand) : Nat :=
  List.map (fun (i, e) => (i+1) * e.bid) (sort input).enum |> List.foldl (· + ·) 0

def part2 (input : List Hand) : Nat :=
  0

def main : IO Unit := do
  let file ← IO.FS.readFile "input/day07.txt"
  let input := List.filterMap parseInput (file.splitOn "\n")

  println! s!"\tPart 1: {(part1 input)}"
  println! s!"\tPart 2: {(part2 input)}"
