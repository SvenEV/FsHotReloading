namespace FsHotReloading

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Linq

module String =
    let lineSeparators = [| "\r\n"; "\r"; "\n" |]
    let join separator (strings: 'a seq) = String.Join (separator, strings)
    let joinn strings = join Environment.NewLine strings
    let lines (s: string) = s.Split (lineSeparators, StringSplitOptions.None)
    let indent i =
        let prefix = String.replicate i " "
        lines >> Array.map ((+) prefix) >> joinn

module KeyValuePair =
    let asTuple (kvp: KeyValuePair<'key, 't>) = kvp.Key, kvp.Value
    let ofTuple (k, v) = KeyValuePair(k, v)
    let key (kvp: KeyValuePair<'key, 't>) = kvp.Key
    let value (kvp: KeyValuePair<'key, 't>) = kvp.Value

module ImmutableDictionary =
    [<GeneralizableValue>]
    let empty<'k, 'v> = ImmutableDictionary<'k, 'v>.Empty

    let ofSeq<'k, 'v> entries =
        // Note: 'ImmutableDictionary.CreateRange' fails when there are duplicate keys
        ImmutableDictionary<'k, 'v>.Empty.SetItems entries

    let ofTupleSeq items = items |> Seq.map KeyValuePair.ofTuple |> ofSeq
    
    let add key value (dict: ImmutableDictionary<'k, 'v>) = dict.SetItem (key, value)
    let remove key (dict: ImmutableDictionary<'k, 'v>) = dict.Remove key

module Dictionary =
    let add key value (dict: IDictionary<'k, 'v>) = dict.[key] <- value
    let remove (key: 'k) (dict: IDictionary<'k, 'v>) = dict.Remove key
    let clear (dict: IDictionary<'k, 'v>) = dict.Clear ()

    let tryFind key (dict: IDictionary<'key, 't>) =
        match dict.TryGetValue key with
        | true, v -> Some v
        | _ -> None
    
    let iter f =
        Seq.iter (fun (kvp: KeyValuePair<'k, 'v>) -> f kvp.Key kvp.Value)

    let equal (a: ICollection<KeyValuePair<'k, 'v>>) (b: ICollection<KeyValuePair<'k, 'v>>) =
        a.Count = b.Count && Enumerable.SequenceEqual(a, b) // note: count check for performance only
    
    let filter predicate (dict: IEnumerable<KeyValuePair<'k, 'v>>) =
        dict
        |> Seq.filter (fun o -> predicate o.Key o.Value)
        |> ImmutableDictionary.ofSeq

    let fold folder state =
        Seq.fold (fun state (o: KeyValuePair<'k, 'v>) -> folder state o.Key o.Value) state

    let partition predicate (dict: IEnumerable<KeyValuePair<'k, 'v>>) =
        let groups =
            dict.GroupBy (fun o -> predicate o.Key o.Value)
            |> Seq.map (fun g -> g.Key, g |> Seq.map (fun o -> o.Key, o.Value))
            |> Map.ofSeq
        let get flag =
            groups
            |> Map.tryFind flag
            |> Option.map ImmutableDictionary.ofTupleSeq
            |> Option.defaultValue ImmutableDictionary.empty
        get true, get false

module Map =
    let ofDictionary (dict: IEnumerable<KeyValuePair<'k, 'v>>) =
        dict
        |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
        |> Map.ofSeq

    let keys (dict: IEnumerable<KeyValuePair<'k, 'v>>) =
        dict |> Seq.map (fun o -> o.Key)

    let values (dict: IEnumerable<KeyValuePair<'k, 'v>>) =
        dict |> Seq.map (fun o -> o.Value)

[<AutoOpen>]
module DictionaryExtensions =
    let (|KeyValuePair|) (kvp: KeyValuePair<'k, 'v>) = kvp.Key, kvp.Value

    type IDictionary<'key, 't> with
        member dict.TryFind key = Dictionary.tryFind key dict

module ImmutableHashSet =
    let ofSeq = ImmutableHashSet.CreateRange
    let union (a: ImmutableHashSet<'a>) b = a.Union b
