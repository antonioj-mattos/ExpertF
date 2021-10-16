namespace Chapter05

open System


module TypeAbbreviations =

    type Index = int
    type Flags = int64
    type Results = string * TimeSpan * int * int


module Records =

    type Person =
        { Name: string
          DateOfBirth: DateTime }

    let person =
        { Name = "Bill"
          DateOfBirth = DateTime(1962, 9, 2) }

    type PageStats =
        { Site: string
          Time: TimeSpan
          Refs: int
          Words: int
          Length: int }

    type Dot =
        { X: int
          Y: int }

    type Point =
        { X: float
          Y: float }

    let distance p = sqrt (p.X * p.X + p.Y * p.Y)

    type Point3D =
        { X: float
          Y: float
          Z: float }

    let p1 =
        { X = 3.0
          Y = 4.0
          Z = 5.0 }

    let p2 =
        { p1 with
              Y = 0.0
              Z = 0.0 }


module DiscriminatedUnions =

    type Make = string
    type Model = string
    type Route = int

    type Transport =
        | Car of Make * Model
        | Bus of Route
        | Bicycle

    let ian = Car("BMW", "360")

    let averageSpeed (transport: Transport) =
        match transport with
        | Car _ -> 35
        | Bus _ -> 24
        | Bicycle -> 16

    type Proposition =
        | True
        | And of Proposition * Proposition
        | Or of Proposition * Proposition
        | Not of Proposition

    let rec eval (proposition: Proposition) =
        match proposition with
        | True -> true
        | And (p1, p2) -> eval p1 && eval p2
        | Or (p1, p2) -> eval p1 || eval p2
        | Not (p) -> not (eval p)

    type 'T List =
        | op_Nil
        | op_ColonColon of 'T * 'T List

    type Tree<'T> =
        | Leaf of 'T
        | Tree of 'T * Tree<'T> * Tree<'T>

    let rec sizeOfTree tree =
        match tree with
        | Leaf _ -> 1
        | Tree (_, left, right) -> 1 + sizeOfTree left + sizeOfTree right

    let smallTree =
        Tree("1", Tree("2", Leaf "a", Leaf "b"), Leaf "c")

    let size = sizeOfTree smallTree


    type Node =
        { Name: string
          Links: Link list }

    and Link =
        | Dangling
        | Link of Node


module Generics =

    type StringMap<'T> = Map<string, 'T>

    let rec map (f: 'T -> 'U) (l: 'T list) =
        match l with
        | h :: t -> f h :: map f t
        | [] -> []

    let rec hcf a b =
        if a = 0 then b
        elif a < b then hcf a (b - a)
        else hcf (a - b) b

    let hcf' (zero, sub, lessThan) =
        let rec hcf a b =
            if a = zero then b
            elif lessThan a b then hcf a (sub b a)
            else hcf (sub a b) b

        hcf

    type INumeric<'T> =
        abstract Zero : 'T
        abstract Subtract : 'T * 'T -> 'T
        abstract LessThan : 'T * 'T -> bool

    let intOps =
        { new INumeric<int> with
            member _.Zero = 0
            member _.Subtract(x, y) = x - y
            member _.LessThan(x, y) = x < y }

    let hcfGeneric (ops: INumeric<'T>) =
        let rec hcf a b =
            if a = ops.Zero then
                b
            elif ops.LessThan(a, b) then
                hcf a (ops.Subtract(b, a))
            else
                hcf (ops.Subtract(a, b)) b

        hcf

    let inline hcf'' a b =
        hcfGeneric
            { new INumeric<'T> with
                member _.Zero = LanguagePrimitives.GenericZero<'T>
                member _.Subtract(x, y) = x - y
                member _.LessThan(x, y) = x < y }
            a
            b

    let inline convertToFloatAndAdd x y = float x + float y
