module Tests

open Xunit

[<Fact>]
let ``My test`` () = Assert.True(true)

[<Theory>]
[<InlineData(1)>]
[<InlineData(2)>]
let ``My theory`` (n: int) = Assert.True(n > 0)

[<Fact>]
let ``Fails`` () = Assert.True(false)

[<Fact(Skip = "Skip me")>]
let ``Skipped`` () = Assert.True(true)

[<Fact>]
let ``Writes to stdout`` () =
  System.Console.WriteLine("Where do I show up in the results")
  Assert.True(true)

[<Fact>]
let ``Waits for cancellation`` () =
  // Only the cancellation regression creates this gate, before execution starts.
  let gate =
    System.IO.Path.Combine(System.IO.Path.GetTempPath(), $"fsac-mtp-cancellation-{System.Environment.ProcessId}")

  if System.IO.File.Exists gate then
    System.IO.File.WriteAllText(gate + ".started", "started")
    let elapsed = System.Diagnostics.Stopwatch.StartNew()

    while System.IO.File.Exists gate && elapsed.Elapsed < System.TimeSpan.FromSeconds 60.0 do
      System.Threading.Thread.Sleep 25

// xunit.v3 hands Console output to the platform only when the assembly asks for it.
[<assembly: CaptureConsole>]
do ()
