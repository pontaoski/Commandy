namespace Commandy

open Terraria
open TerrariaApi.Server
open TShockAPI
open System
open System.Threading.Tasks
open System.Collections.Generic
open System.Linq

[<AttributeUsage(AttributeTargets.Property)>]
type public IsCommandPlusCompatible(commandPlusClass: Type) =
    class
        inherit Attribute()

        member public this.CommandPlusClass = commandPlusClass
    end

[<ApiVersion(2, 1)>]
type public CommandyPlugin(game: Main) =
    class inherit TerrariaPlugin(game)
    end

    override this.Name = "Commandy"
    override this.Version = Version("1.0")
    override this.Author = "Janet"
    override this.Description = "Better commands"
    override this.Enabled = true

    override this.Initialize() =
        let cb = this.PlayerCommandD
        Hooks.PlayerHooks.add_PlayerCommand(cb)

    override this.Dispose(disposing: bool) =
        if disposing then
            let cb = this.PlayerCommandD
            Hooks.PlayerHooks.remove_PlayerCommand(cb)
        base.Dispose(disposing)

    member this.PlayerCommandD (item: Hooks.PlayerCommandEventArgs): unit =
        task {
            let parsed = Parser.parseInput(item.CommandText)
            let ctx = CommandContext(item.Player, item.CommandText)
            let cb = this.LookupCommand

            try
                let! pipeline = Evaluation.buildPipeline(ctx, parsed, true, cb)
                item.Handled <- true
                task {
                    do! pipeline.Kickoff() |> Async.AwaitTask
                    let! list = pipeline.Outgoing.ReadAllAsync().ToListAsync().AsTask() |> Async.AwaitTask
                    ignore list
                }
                |> ignore
            with
            | :? Evaluation.CommandNotFound -> item.Handled <- false
        }
        |> Task.WaitAll

    member this.LookupCommand(name: string): (CommandPlus option) =
        let dict = new Dictionary<string, Type>()
        dict.Add("players", typeof<Players>)
        dict.Add("kill", typeof<Kill>)
        if dict.ContainsKey(name.ToLower()) then
            Some(downcast Activator.CreateInstance(dict.[name]))
        else
            None

