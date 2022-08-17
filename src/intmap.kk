module intmap

import std/num/int64


type intmap<a>
    Bin(prefix_: int64, mask_: int64, l: intmap<a>, r: intmap<a>)
/* -- Fields: */
/* --   prefix: The most significant bits shared by all keys in this Bin. */
/* --   mask: The switching bit to determine if a key should follow the left */
/* --         or right subtree of a 'Bin'. */
/* -- Invariant: Nil is never found as a child of Bin. */
/* -- Invariant: The Mask is a power of 2. It is the largest bit position at which */
/* --            two keys of the map differ. */
/* -- Invariant: Prefix is the common high-order bits that all elements share to */
/* --            the left of the Mask bit. */
/* -- Invariant: In (Bin prefix mask left right), left consists of the elements that */
/* --            don't have the mask bit set; right is all the elements that do. */
    Tip(key: int64, value: a)
    MapNil

fun link(p1: int64, t1: intmap<a>, p2: int64, t2: intmap<a>): intmap<a>
    linkWithMask(branchMask(p1, p2), p1, t1, t2)

fun linkWithMask(m: int64, p1: int64, t1: intmap<a>, t2: intmap<a>): intmap<a>
    val p = mask_(p1, m)
    if zero(p1, m) then Bin(p, m, t1, t2) else Bin(p, m, t2, t1)

fun branchMask(p1: int64, p2: int64): int64
    highestBitMask(xor(p1, p2))

pub fun highestBitMask(num: int64): int64
    val r = 1.int64.shl(63 - leading_zeros(num).int)
    trace("bitmask " ++ num.show ++ ":" ++ r.show)
    r

pub fun leading_zeros(num: int64): int64
    unsafe-no-div
        var x := num
        var n := 64.int64
        var c := 32.int64
        while {c != 0.int64}
            val y = x.shr(c.int)
            if y != 0.int64 then
                n := n - c
                x := y
            c := c.shr(1)

        n - x

fun zero(i: int64, m: int64): bool
    and(i, m) == 0.int64

fun mask_(key: int64, m: int64): int64
    and(key, xor(~m, m))

fun nomatch(i: int64, p: int64, m: int64): bool
    mask_(i, m) != p

pub fun insert(t: intmap<a>, k: int64, x: a): intmap<a>
    match t
        Bin(p, m, l, r) ->
            if nomatch(k, p, m) then
                link(k, Tip(k, x), p, t)
            elif zero(k, m) then
                Bin(p, m, insert(l, k, x), r)
            else
                Bin(p, m, l, insert(r, k, x))
        Tip(ky, _) -> if k == ky
            then Tip(k, x)
            else link(k, Tip(k, x), ky, t)
        MapNil -> Tip(k, x)

pub fun get(t: intmap<a>, k: int64): maybe<a>
    trace("get " ++ k.show)
    match t
        Bin(_, m, l, r) ->
            trace("get bin " ++ m.show)
            if zero(k, m) then l.get(k) else r.get(k)
        Tip(kx, x) ->
            if k == kx then x.Just else Nothing
        MapNil -> Nothing

pub fun fold(m: intmap<a>, acc: x, f: (x, int64, a) -> x): x
    fun go(acc', m')
        match m'
            MapNil -> acc'
            Tip(k, v) -> f(acc', k, v)
            Bin(_, _, l, r) -> go(go(acc', l), r)
    match m
        Bin(_, mask_, l, r) ->
            if mask_ < 0.int64 then
                go(go(acc, r), l)
            else
                go(go(acc, l), r)
        _ -> go(acc, m)

pub fun list(t: intmap<v>): list<(int64, v)>
    t.fold(Nil, fn(l, k, v) Cons((k, v), l))

pub fun intmap(): intmap<a>
    MapNil

pub fun intmap(k: int64, v: v): intmap<v>
    Tip(k, v)

pub fun intmap(xs: list<(int64, v)>): intmap<v>
    xs.foldl(intmap(), fn(m, (k, v)) m.insert(k, v))

pub fun show(t: intmap<v>, callback: (v) -> string): string
    "[" ++ t.list.map(fn((k, v)) k.show() ++ ": " ++ v.callback()).join(", ") ++ "]"
