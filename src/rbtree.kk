module rbtree

type color
  Red
  Black


pub type tree<v>
  Node(color : color, lchild : tree<v>, key : string, value : v, rchild : tree<v>)
  Leaf()


fun is-red(t : tree<v>) : bool
  match t
    Node(Red) -> True
    _         -> False


fun balance-left(l :tree<v>, k : string, v : v, r : tree<v>) : tree<v>
  match l
    Node(_, Node(Red, lx, kx, vx, rx), ky, vy, ry)
      -> Node(Red, Node(Black, lx, kx, vx, rx), ky, vy, Node(Black, ry, k, v, r))
    Node(_, ly, ky, vy, Node(Red, lx, kx, vx, rx))
      -> Node(Red, Node(Black, ly, ky, vy, lx), kx, vx, Node(Black, rx, k, v, r))
    Node(_, lx, kx, vx, rx)
      -> Node(Black, Node(Red, lx, kx, vx, rx), k, v, r)
    Leaf -> Leaf


fun balance-right(l : tree<v>, k : string, v : v, r : tree<v>) : tree<v>
  match r
    Node(_, Node(Red, lx, kx, vx, rx), ky, vy, ry)
      -> Node(Red, Node(Black, l, k, v, lx), kx, vx, Node(Black, rx, ky, vy, ry))
    Node(_, lx, kx, vx, Node(Red, ly, ky, vy, ry))
      -> Node(Red, Node(Black, l, k, v, lx), kx, vx, Node(Black, ly, ky, vy, ry))
    Node(_, lx, kx, vx, rx)
      -> Node(Black, l, k, v, Node(Red, lx, kx, vx, rx))
    Leaf -> Leaf


fun ins(t : tree<v>, k : string, v : v) : tree<v>
  match t
    Node(Red, l, kx, vx, r)
      -> if k < kx then Node(Red, ins(l, k, v), kx, vx, r)
         elif k > kx then Node(Red, l, kx, vx, ins(r, k, v))
         else Node(Red, l, k, v, r)
    Node(Black, l, kx, vx, r)
      -> if k < kx then (if is-red(l) then balance-left(ins(l,k,v), kx, vx, r)
                                      else Node(Black, ins(l, k, v), kx, vx, r))
         elif k > kx then (if is-red(r) then balance-right(l, kx, vx, ins(r,k,v))
                                        else Node(Black, l, kx, vx, ins(r, k, v)))
         else Node(Black, l, k, v, r)
    Leaf -> Node(Red, Leaf, k, v, Leaf)


fun set-black(t : tree<v>) : tree<v>
  match t
    Node(_, l, k, v, r) -> Node(Black, l, k, v, r)
    _ -> t


pub fun insert(t : tree<v>, k : string, v : v) : tree<v>
  ins(t, k, v).set-black


pub fun insert-with(t : tree<v>, k : string, v : v, f: (v, v) -> e v) : e tree<v>
  val old = t.get(k)
  match old
      Just(old_v) -> ins(t, k, f(old_v, v)).set-black
      Nothing -> ins(t, k, v).set-black


pub fun fold(t : tree<v>, b : a, f: (a, string, v) -> e a) : e a
  match t
    Node(_, l, k, v, r) -> r.fold( f(l.fold(b, f), k, v), f)
    Leaf                -> b


pub fun get(t: tree<v>, key: string): maybe<v>
    match t
        Node(_, l, k, v, r) ->
            if k == key then
                v.Just
            else
                if k > key then
                    l.get(key)
                else
                    r.get(key)
        Leaf -> Nothing


pub fun rbtree(): tree<v>
    Leaf


pub fun rbtree(k: string, v: v): tree<v>
    rbtree().insert(k, v)


pub fun rbtree(input: list<(string, v)>): tree<v>
    input.foldl(rbtree(), fn(t, (k, v)) t.insert(k, v))


pub fun list(t: tree<v>): list<(string, v)>
    t.fold(Nil, fn(l, k, v) Cons((k, v), l))


pub fun show(t: tree<v>, callback: (v) -> string): string
    "[" ++ t.list.map(fn((k, v)) k ++ ": " ++ v.callback()).join(", ") ++ "]"


pub fun (++)(l: tree<v>, r: tree<v>): tree<v>
    r.fold(l, fn(t, k, v) t.insert(k, v))

pub fun union-with(l: tree<v>, r: tree<v>, f: (v, v) -> e v): e tree<v>
    r.fold(l, fn(t, k, v) t.insert-with(k, v, f))
