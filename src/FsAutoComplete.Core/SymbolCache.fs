module SymbolCache

open System
open System.Diagnostics
open FsAutoComplete
open FSharp.Compiler.SourceCodeServices

open System.Net
open System.IO
open Newtonsoft.Json

[<CLIMutable>]
type SymbolUseRange = {
    FileName: string
    StartLine: int
    StartColumn: int
    EndLine: int
    EndColumn: int
    IsFromDefinition: bool
    IsFromAttribute : bool
    IsFromComputationExpression : bool
    IsFromDispatchSlotImplementation : bool
    IsFromPattern : bool
    IsFromType : bool
    SymbolFullName: string
    SymbolDisplayName: string
    SymbolIsLocal: bool
}


module private PersistenCacheImpl =
    open Microsoft.Data.Sqlite
    open Dapper
    open System.Data

    let mutable connection : Lazy<SqliteConnection> option = None

    let insertHelper (connection: SqliteConnection) file (sugs: SymbolUseRange[]) =
        if connection.State <> ConnectionState.Open then connection.Open()
        use tx = connection.BeginTransaction()
        let delCmd = sprintf "DELETE FROM Symbols WHERE FileName=\"%s\"" file
        let inserCmd =
            sprintf "INSERT INTO SYMBOLS(FileName, StartLine, StartColumn, EndLine, EndColumn, IsFromDefinition, IsFromAttribute, IsFromComputationExpression, IsFromDispatchSlotImplementation, IsFromPattern, IsFromType, SymbolFullName, SymbolDisplayName, SymbolIsLocal) VALUES
            (@FileName, @StartLine, @StartColumn, @EndLine, @EndColumn, @IsFromDefinition, @IsFromAttribute, @IsFromComputationExpression, @IsFromDispatchSlotImplementation, @IsFromPattern, @IsFromType, @SymbolFullName, @SymbolDisplayName, @SymbolIsLocal)"
        connection.Execute(delCmd, transaction = tx) |> ignore
        connection.Execute(inserCmd, sugs, transaction = tx) |> ignore
        tx.Commit()

    let insertQueue = MailboxProcessor.Start(fun agent ->
        let rec loop () = async {
            let! (con, file, symbols) = agent.Receive()
            insertHelper con file symbols
            return! loop ()
        }
        loop ()
    )

    let insert = insertQueue.Post


    let loadAll (connection: SqliteConnection) =
        if connection.State <> ConnectionState.Open then connection.Open()
        let q = "SELECT * FROM SYMBOLS"
        let res = connection.Query<SymbolUseRange>(q)
        res

    let loadFile (connection: SqliteConnection) file =
        if connection.State <> ConnectionState.Open then connection.Open()
        let q = sprintf "SELECT * FROM SYMBOLS WHERE FileName=\"%s\"" file
        let res = connection.Query<SymbolUseRange>(q)
        res

    let loadSymbolUses (connection: SqliteConnection) file = async {
        if connection.State <> ConnectionState.Open then connection.Open()
        let q = sprintf "SELECT * FROM SYMBOLS WHERE SymbolFullName=\"%s\" AND SymbolIsLocal=false" file
        let! res = connection.QueryAsync<SymbolUseRange>(q) |> Async.AwaitTask
        return Seq.toArray res
    }

    let loadImplementations (connection: SqliteConnection) file = async {
        if connection.State <> ConnectionState.Open then connection.Open()
        let q = sprintf "SELECT * FROM SYMBOLS WHERE SymbolFullName=\"%s\" AND SymbolIsLocal=false AND (IsFromDispatchSlotImplementation=true OR IsFromType=true)" file
        let! res = connection.QueryAsync<SymbolUseRange>(q) |> Async.AwaitTask
        return Seq.toArray res
    }

    let initializeCache dir =
        let connectionString = sprintf "Data Source=%s/.ionide/symbolCache.db" dir

        let dir = Path.Combine(dir, ".ionide")
        do if not (Directory.Exists dir) then Directory.CreateDirectory dir |> ignore
        let dbPath = Path.Combine(dir, "symbolCache.db")
        let dbExists = File.Exists dbPath
        let conn = new SqliteConnection(connectionString)

        do if not dbExists then
            let fs = File.Create(dbPath)
            fs.Close()
            let cmd = "CREATE TABLE Symbols(
                FileName TEXT,
                StartLine INT,
                StartColumn INT,
                EndLine INT,
                EndColumn INT,
                IsFromDefinition BOOLEAN,
                IsFromAttribute BOOLEAN,
                IsFromComputationExpression BOOLEAN,
                IsFromDispatchSlotImplementation BOOLEAN,
                IsFromPattern BOOLEAN,
                IsFromType BOOLEAN,
                SymbolFullName TEXT,
                SymbolDisplayName TEXT,
                SymbolIsLocal BOOLEAN
            )"

            conn.Execute(cmd)
            |> ignore

        conn

    let initLazyCache dir =
        let lazyConn = Lazy<SqliteConnection>(fun _ -> initializeCache dir)
        connection <- Some lazyConn

let fromSymbolUse (su : FSharpSymbolUse) =
    {   StartLine = su.RangeAlternate.StartLine
        StartColumn = su.RangeAlternate.StartColumn + 1
        EndLine = su.RangeAlternate.EndLine
        EndColumn = su.RangeAlternate.EndColumn + 1
        FileName = su.FileName
        IsFromDefinition = su.IsFromDefinition
        IsFromAttribute = su.IsFromAttribute
        IsFromComputationExpression = su.IsFromComputationExpression
        IsFromDispatchSlotImplementation = su.IsFromDispatchSlotImplementation
        IsFromPattern = su.IsFromPattern
        IsFromType = su.IsFromType
        SymbolFullName = su.Symbol.FullName
        SymbolDisplayName = su.Symbol.DisplayName
        SymbolIsLocal = su.Symbol.IsPrivateToFile  }

let initCache dir =
    PersistenCacheImpl.initLazyCache dir

let updateSymbols fn (symbols: FSharpSymbolUse[]) =
    let sus = symbols |> Array.map(fromSymbolUse)

    PersistenCacheImpl.connection
    |> Option.iter (fun con -> PersistenCacheImpl.insert(con.Value,fn,sus) )

let getSymbols symbolName =
    async {
        match PersistenCacheImpl.connection with
        | Some conn ->
            let! res = PersistenCacheImpl.loadSymbolUses conn.Value symbolName
            return Some res
        | None -> return None
    }

let getImplementation symbolName =
    async {
        match PersistenCacheImpl.connection with
        | Some conn ->
            let! res = PersistenCacheImpl.loadImplementations conn.Value symbolName
            return Some res
        | None -> return None
    }
