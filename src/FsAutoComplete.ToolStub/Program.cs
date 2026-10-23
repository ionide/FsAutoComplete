using System;
using System.IO;
using System.Text;
using System.Text.Json.Nodes;

const string Message =
    "FsAutoComplete needs the .NET 10 SDK. This copy was installed by an older .NET SDK, which only gets this placeholder. "
    + "Install the .NET 10 SDK from https://dotnet.microsoft.com/download/dotnet/10.0 and install FsAutoComplete again, "
    + "for example with `dotnet tool update --global fsautocomplete`. Projects that target older frameworks keep working.";

Console.Error.WriteLine(Message);

// An editor starts the server with redirected input and sends `initialize` first. Answering it with an error lets
// the editor show the message, instead of only reporting that the server exited.
if (Console.IsInputRedirected)
{
    try
    {
        AnswerInitialize();
    }
    catch (Exception)
    {
        // The message is on stderr already.
    }
}

return 1;

static void AnswerInitialize()
{
    using var input = Console.OpenStandardInput();
    var length = 0;

    for (var header = ReadLine(input); header.Length > 0; header = ReadLine(input))
    {
        var separator = header.IndexOf(':');

        if (separator > 0 && header[..separator].Trim().Equals("Content-Length", StringComparison.OrdinalIgnoreCase))
        {
            length = int.Parse(header[(separator + 1)..].Trim());
        }
    }

    var body = new byte[length];
    input.ReadExactly(body);
    var id = JsonNode.Parse(body)?["id"];

    if (id is null)
    {
        return;
    }

    var response = new JsonObject
    {
        ["jsonrpc"] = "2.0",
        ["id"] = id.DeepClone(),
        // InternalError
        ["error"] = new JsonObject { ["code"] = -32603, ["message"] = Message },
    };

    var responseBody = Encoding.UTF8.GetBytes(response.ToJsonString());
    using var output = Console.OpenStandardOutput();
    output.Write(Encoding.ASCII.GetBytes($"Content-Length: {responseBody.Length}\r\n\r\n"));
    output.Write(responseBody);
    output.Flush();
}

static string ReadLine(Stream input)
{
    var line = new StringBuilder();

    for (var b = input.ReadByte(); b != '\n'; b = input.ReadByte())
    {
        if (b == -1)
        {
            throw new EndOfStreamException();
        }

        if (b != '\r')
        {
            line.Append((char)b);
        }
    }

    return line.ToString();
}
