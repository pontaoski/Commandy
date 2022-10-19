namespace Commandy

open TShockAPI
open System
open System.Threading.Channels
open System.Threading.Tasks
open System.Collections.Generic
open System.Linq

[<AttributeUsage(AttributeTargets.Property)>]
type public Param() =
    class
        inherit Attribute()

        member val ValueFromPipeline = false with get, set
        member val ValueFromPipelineByPropertyName = false with get, set
        member val Position: Int32  = -1 with get, set
    end

type public CommandContext(player: TSPlayer, rawMessage: string) =
    class
        member public this.Player = player
        member public this.RawMessage = rawMessage
    end

type public CommandArguments(positional: List<obj>, flags: Dictionary<string, obj>) =
    member public this.Positional = positional
    member public this.Flags = flags

module public ConversionRules =
    let rules = new Dictionary<Type, string -> obj>()

    rules.Add(typeof<Int16>, fun s -> Int16.Parse(s))
    rules.Add(typeof<Int32>, fun s -> Int32.Parse(s))
    rules.Add(typeof<Int64>, fun s -> Int64.Parse(s))
    rules.Add(typeof<UInt16>, fun s -> UInt16.Parse(s))
    rules.Add(typeof<UInt32>, fun s -> UInt32.Parse(s))
    rules.Add(typeof<UInt64>, fun s -> UInt64.Parse(s))
    rules.Add(typeof<TSPlayer>, fun s -> TSPlayer.FindByNameOrID(s).First())

    let public convert(item: obj, target: Type): obj =
        match item with
        | :? string as str ->
            let convertTo =
                match Nullable.GetUnderlyingType(target) with
                | null -> target
                | a -> a

            if rules.ContainsKey(convertTo) then
                rules.[convertTo] str
            else
                item
        | _ -> item

[<AbstractClass>]
type public CommandPlus() =
    class
        abstract member Process: CommandContext -> Task

        member internal this.SafeProcess(ctx: CommandContext): Task<bool> =
            task {
                try
                    do! this.Process(ctx)
                    return true
                with
                | a ->
                    printfn "Exception in pipeline: %A" a
                    return false
            }

        member val internal OutStream: ChannelWriter<obj> = null with get, set
        member internal this.Begin(ctx: CommandContext, args: CommandArguments, inStream: ChannelReader<obj> option): ((unit -> Task) * ChannelReader<obj>) =
            let chan = Channel.CreateUnbounded()
            this.OutStream <- chan.Writer

            this.BindFromArguments(args)
            match inStream with
            | Some stream ->
                task {
                    let go = ref true
                    let! initial = stream.WaitToReadAsync()
                    go.Value <- initial
                    while go.Value do
                        let! item = stream.ReadAsync()
                        this.BindFromPipeline(item)
                        let! ok = this.SafeProcess(ctx)
                        if ok then
                            let! next = stream.WaitToReadAsync()
                            go.Value <- next
                        else
                            go.Value <- false
                    this.OutStream.Complete()
                }
            | None -> task { () }
            |> ignore

            let kickOff() =  
                task {
                    let! ok = this.SafeProcess(ctx)
                    ignore ok
                    this.OutStream.Complete()
                } :> Task

            (kickOff, chan.Reader)
        member internal this.GetParam(p: Reflection.PropertyInfo): Param option =
            let filtered =
                p.GetCustomAttributes(true)
                    .Select(fun p ->
                        match p with
                        | :? Param as param -> Some(param)
                        | _ -> None)
                    .Where(fun p -> p <> None)
                    .ToList()

            if filtered.Count = 0 then
                None
            else
                filtered.First()
        member internal this.BindFromPipeline(item: obj): unit =
            let itemProps = this.GetType().GetProperties(Reflection.BindingFlags.Instance ||| Reflection.BindingFlags.Public)
            for prop in itemProps do
                let attr: Param option = this.GetParam(prop)
                match attr with
                | Some v ->
                    if v.ValueFromPipeline then
                        prop.SetValue(this, item)
                    if v.ValueFromPipelineByPropertyName && itemProps.Any(fun x -> x.Name = prop.Name) then
                        prop.SetValue(this, itemProps.First(fun x -> x.Name = prop.Name).GetValue(item))
                | None -> ()
        member internal this.BindFromArguments(item: CommandArguments): unit =
            let itemProps = this.GetType().GetProperties(Reflection.BindingFlags.Instance ||| Reflection.BindingFlags.Public)
            for prop in itemProps do
                let attr: Param option = this.GetParam(prop)
                match attr with
                | Some v ->
                    if item.Flags.ContainsKey(prop.Name) then
                        prop.SetValue(this, ConversionRules.convert(item.Flags.[prop.Name], prop.PropertyType))
                    if v.Position <> -1 && item.Positional.Count > v.Position then
                        prop.SetValue(this, ConversionRules.convert(item.Positional.[v.Position], prop.PropertyType))
                | None -> ()
        member public this.WriteObject(item: obj): Task<unit> =
            task {
                do! this.OutStream.WriteAsync(item)
            }
    end

module Evaluation =
    type Pipeline(kickoff: unit -> Task, outgoing: ChannelReader<obj>, incoming: ChannelWriter<obj>) =
        class
            member public this.Kickoff = kickoff
            member public this.Outgoing = outgoing
            member public this.Incoming = incoming
        end

    let just value =
        let kickoff = fun k -> Task.CompletedTask
        let chan = Channel.CreateBounded(1)
        chan.Writer.WriteAsync(value) |> ignore
        chan.Writer.Complete()
        Pipeline(kickoff, chan.Reader, chan.Writer)

    type CommandNotFound(name: string) =
        class
            inherit Exception()
            member public this.WhatWasntFound = name
        end

    let rec buildPipeline (ctx: CommandContext, from: Expression, isStart: bool, cmdLookup: string -> (CommandPlus option)): Pipeline Async =
        async {
            match from with
            | ExprCommandApplication (cmd, args) ->
                return!
                    match (cmdLookup cmd) with
                    | None -> raise (CommandNotFound(cmd))
                    | Some cmd -> async {
                        let chan = Channel.CreateUnbounded()
                        let! args = evaluateArguments(ctx, args, cmdLookup)
                        let (kickoff, out) = cmd.Begin(ctx, args, if isStart then None else Some chan.Reader)

                        return Pipeline(kickoff, out, chan.Writer)
                    }
            | ExprPipeline (from, into) ->
                let! src = buildPipeline (ctx, from, isStart, cmdLookup)
                let! dest = buildPipeline (ctx, into, false, cmdLookup)

                task {
                    let go = ref true
                    let! initial = src.Outgoing.WaitToReadAsync()
                    go.Value <- initial
                    while go.Value do
                        let! item = src.Outgoing.ReadAsync()
                        let! next = src.Outgoing.WaitToReadAsync()
                        go.Value <- next
                        do! dest.Incoming.WriteAsync(item)
                    dest.Incoming.Complete()
                } |> ignore

                return Pipeline(src.Kickoff, dest.Outgoing, src.Incoming)
            | ExprString str -> return just str
            | ExprNum num -> return just num
        }
    and evaluateArguments(ctx: CommandContext, from: Argument list, cmdLookup: string -> (CommandPlus option)): CommandArguments Async =
        async {
            let mut = new List<Argument>(from)
            let positional = new List<obj>()
            let flags = new Dictionary<string, obj>()
            while mut.Count > 0 do
                let item = mut.[0]
                mut.RemoveAt(0)

                match item with
                | Arg str when str.StartsWith("-") && mut.Count > 0 ->
                    let name = str.[1..]

                    match mut.[0] with
                    | Arg str when str.StartsWith("-") ->
                        flags.Add(name, true)
                    | Arg str ->
                        mut.RemoveAt(0)
                        flags.Add(name, str)
                    | ArgExpr expr ->
                        mut.RemoveAt(0)
                        let! pipeline = buildPipeline(ctx, expr, true, cmdLookup)
                        let! list = pipeline.Outgoing.ReadAllAsync().ToListAsync().AsTask() |> Async.AwaitTask
                        if list.Count = 1 then
                            flags.Add(name, list.[0])
                        else
                            flags.Add(name, list)
                | Arg str when str.StartsWith("-") ->
                    flags.Add(str.[1..], true)
                | Arg str ->
                    positional.Add(str)
                | ArgExpr expr ->
                    let! pipeline = buildPipeline(ctx, expr, true, cmdLookup)
                    let! list = pipeline.Outgoing.ReadAllAsync().ToListAsync().AsTask() |> Async.AwaitTask
                    if list.Count = 1 then
                        positional.Add(list.[0])
                    else
                        positional.Add(list)

            return CommandArguments(positional, flags)
        }
