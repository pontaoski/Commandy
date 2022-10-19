namespace Commandy

open TShockAPI
open System
open System.Threading.Tasks
open System.Linq

type public Players() =
    inherit CommandPlus()

    [<Param(Position = 0, ValueFromPipeline = true)>]
    member val public Team: int Nullable = Nullable() with get, set

    override this.Process(ctx: CommandContext): Task =
        let players =
            TShock.Players
                .Where(fun p -> p <> null)
                .Where(fun p -> if this.Team.HasValue then p.Team = this.Team.Value else true)

        task {
            for player in players do
                do! this.WriteObject(player)
        }

type public Kill() =
    inherit CommandPlus()

    override this.Process(ctx: CommandContext): Task =
        this.Player.KillPlayer()
        Task.CompletedTask

    [<Param(Position = 0, ValueFromPipeline = true)>]
    member val public Player: TSPlayer = null with get, set
