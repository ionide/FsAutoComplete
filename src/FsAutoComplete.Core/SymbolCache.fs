module SymbolCache

open System
open FsAutoComplete
open FSharp.Compiler.CodeAnalysis

open System.IO
open FSharp.UMX

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
} with
  member x.Range: FSharp.Compiler.Text.Range =
    FSharp.Compiler.Text.Range.mkRange
      x.FileName
      (FSharp.Compiler.Text.Position.mkPos x.StartLine x.StartColumn)
      (FSharp.Compiler.Text.Position.mkPos x.EndLine x.EndColumn)

module private PersistentCacheImpl =
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

    let loadKnownFiles (connection: SqliteConnection) = async {
        if connection.State <> ConnectionState.Open then connection.Open()
        let q = "SELECT FileName FROM SYMBOLS"
        let! res = connection.QueryAsync<{|FileName: string |}>(q) |> Async.AwaitTask
        return Seq.toArray res
    }

    let deleteFile (connection: SqliteConnection) file = async {
        if connection.State <> ConnectionState.Open then connection.Open()
        let q = sprintf "DELETE FROM SYMBOLS WHERE FileName = \"%s\"" file
        let! res = connection.ExecuteAsync q |> Async.AwaitTask
        return res
    }

    let initializeCache dir =
        let dir = Path.Combine(dir, ".ionide")
        do if not (Directory.Exists dir) then Directory.CreateDirectory dir |> ignore
        let dbPath = Path.Combine(dir, "symbolCache.db")
        let dbExists = File.Exists dbPath
        let conn = new SqliteConnection($"Data Source={dbPath}")

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
    {   StartLine = su.Range.StartLine
        StartColumn = su.Range.StartColumn
        EndLine = su.Range.EndLine
        EndColumn = su.Range.EndColumn
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
    PersistentCacheImpl.initLazyCache dir

let updateSymbols (fn: string<LocalPath>) (symbols: FSharpSymbolUse seq) =
    let sus =
      symbols
      |> Seq.map (fromSymbolUse)
      |> Seq.toArray


    PersistentCacheImpl.connection
    |> Option.iter (fun con -> PersistentCacheImpl.insert(con.Value, UMX.untag fn, sus) )

let getSymbols symbolName =
    async {
        match PersistentCacheImpl.connection with
        | Some conn ->
            let! res = PersistentCacheImpl.loadSymbolUses conn.Value symbolName
            return Some res
        | None -> return None
    }

let getImplementation symbolName =
    async {
        match PersistentCacheImpl.connection with
        | Some conn ->
            let! res = PersistentCacheImpl.loadImplementations conn.Value symbolName
            return Some res
        | None -> return None
    }

let getKnownFiles () =
  async {
        match PersistentCacheImpl.connection with
        | Some conn ->
            let! res = PersistentCacheImpl.loadKnownFiles conn.Value
            return Some res
        | None -> return None
    }

let deleteFile file =
  async {
        match PersistentCacheImpl.connection with
        | Some conn ->
            let! res = PersistentCacheImpl.deleteFile conn.Value file
            return Some res
        | None -> return None
    }
