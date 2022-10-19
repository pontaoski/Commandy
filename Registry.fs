namespace Commandy

open System
open System.Collections.Generic

type public CommandEntry public(name: string, kind: Type, aliases: List<string>) =
    member public _.Name = name
    member public _.Aliases = aliases
    member public _.Type = kind

[<AbstractClass; Sealed>]
type public Registry private() =
    static member private Commands = new List<CommandEntry>()

    static member public RegisterCommand<'t>(name: string, aliases: List<string>) =
        Registry.Commands.Add (new CommandEntry(name, typeof<'t>, aliases))

    static member public RegisterCommand<'t>(name: string) =
        Registry.RegisterCommand<'t>(name, new List<string>())

    static member public RegisterCommand(name: string, kind: Type, aliases: List<string>) =
        Registry.Commands.Add (new CommandEntry(name, kind, aliases))

    static member public RegisterCommand(name: string, kind: Type) =
        Registry.RegisterCommand(name, kind, new List<string>())
