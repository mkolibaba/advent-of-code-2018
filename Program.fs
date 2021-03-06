﻿module aoc2018

open System
open System.IO

module Utils =
    let readInput day = (sprintf "resources\\input-%s.txt" day) |> File.ReadLines |> List.ofSeq
    let splitBy (splitter: string) (str: string) = str.Split([|splitter|], StringSplitOptions.None) |> List.ofArray
    let joinBy (joiner: string) (l: 'a list) = String.Join(joiner, l)

module Day1 = 
    let solve freqs (input: string list) = 
        let folder freqs' value = 
            let next = List.last freqs' + value
            if (List.contains next freqs') then printfn "%A" next else next |> ignore
            freqs'@[next]
        input |> List.map Int32.Parse |> List.fold folder freqs
    let solve1 = solve [0] >> List.last 
    let solve2 input =
        let rec solve2' solved =
            let morethanone = solved |> List.countBy id |> List.filter (fun x -> snd x > 1)
            match morethanone |> List.tryHead with
            | Some twice -> twice |> fst
            | None -> solve2' (solve solved input)
        solve2' (solve [0] input)    

    let decide = function | "2" -> solve2 | "1" | _ -> solve1


module Day2 = 
    let solve (input: string list) =
        let mapped = input |> List.map (Seq.countBy id >> Seq.map snd)
        let findtimes times = mapped |> List.sumBy (fun s -> if Seq.contains times s then 1 else 0)
        findtimes 2 * findtimes 3
    let solve1 = solve
    let solve2 = solve
    let decide = function | "2" -> solve2 | "1" | _ -> solve1

[<EntryPoint>]
let main argv =
    let day = argv |> Array.head
    let part = argv |> Array.tail |> Array.head

    let decider day = 
        match day with
        | "1" -> Day1.decide
        | "2" -> Day2.decide
        | _ -> failwith "wrong day"
    
    let solver = part |> (decider day)

    day |> Utils.readInput |> solver |> printfn "%A"

    0