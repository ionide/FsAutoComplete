module FsAutoComplete.Tests.CodeFixTests.Tests

open System
open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix

let private addMissingEqualsToTypeDefinitionTests state =
  serverTestList (nameof AddMissingEqualsToTypeDefinition) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle AddMissingEqualsToTypeDefinition.title

      testCaseAsync "can add = to record def"
      <| CodeFix.check
        server
        """
        type Person $0{ Name : string; Age : int; City : string }
        """
        (Diagnostics.expectCode "10")
        selectCodeFix
        """
        type Person = { Name : string; Age : int; City : string }
        """

      testCaseAsync "can add = to union def"
      <| CodeFix.check
        server
        """
        type Name $0Name of string
        """
        (Diagnostics.expectCode "10")
        selectCodeFix
        """
        type Name = Name of string
        """ ])

let private addMissingFunKeywordTests state =
  serverTestList (nameof AddMissingFunKeyword) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.ofKind "quickfix" >> CodeFix.withTitle AddMissingFunKeyword.title

      testCaseAsync "can generate the fun keyword when error 10 is raised"
      <| CodeFix.check
        server
        """
        let doThing = x $0-> printfn "%s" x
        """
        (Diagnostics.expectCode "10")
        selectCodeFix
        """
        let doThing = fun x -> printfn "%s" x
        """

      testCaseAsync "can generate fun keyword for multiple parameters"
      <| CodeFix.check
        server
        """
        let calculate = x y z $0-> x + y * z
        """
        (Diagnostics.expectCode "10")
        selectCodeFix
        """
        let calculate = fun x y z -> x + y * z
        """

      testCaseAsync "can generate fun keyword in let binding with assignment"
      <| CodeFix.check
        server
        """
        let mapper = item $0-> item.ToString()
        """
        (Diagnostics.expectCode "10")
        selectCodeFix
        """
        let mapper = fun item -> item.ToString()
        """

      testCaseAsync "can generate fun keyword in nested expression"
      <| CodeFix.check
        server
        """
        let result = List.map (x $0-> x * 2) [1; 2; 3]
        """
        (Diagnostics.expectCode "10")
        selectCodeFix
        """
        let result = List.map (fun x -> x * 2) [1; 2; 3]
        """

      testCaseAsync "can generate fun keyword with complex expression body"
      <| CodeFix.check
        server
        """
        let processor = data $0-> 
          let processed = data |> List.filter (fun x -> x > 0)
          processed |> List.sum
        """
        (Diagnostics.expectCode "10")
        selectCodeFix
        """
        let processor = fun data -> 
          let processed = data |> List.filter (fun x -> x > 0)
          processed |> List.sum
        """

      testCaseAsync "can generate fun keyword with tuple parameter"
      <| CodeFix.check
        server
        """
        let addTuple = (a, b) $0-> a + b
        """
        (Diagnostics.expectCode "10")
        selectCodeFix
        """
        let addTuple = fun (a, b) -> a + b
        """

      testCaseAsync "can generate fun keyword in piping context"
      <| CodeFix.check
        server
        """
        let result = 
          [1..10] 
          |> List.filter (n $0-> n % 2 = 0)
          |> List.sum
        """
        (Diagnostics.expectCode "10")
        selectCodeFix
        """
        let result = 
          [1..10] 
          |> List.filter (fun n -> n % 2 = 0)
          |> List.sum
        """ ])

let private addMissingInstanceMemberTests state =
  serverTestList (nameof AddMissingInstanceMember) state defaultConfigDto None (fun server ->
    [ testCaseAsync "can add this member prefix"
      <| CodeFix.check
        server
        """
        type C () =
          member $0Foo() = ()
        """
        (Diagnostics.expectCode "673")
        (CodeFix.ofKind "quickfix" >> CodeFix.withTitle AddMissingInstanceMember.title)
        """
        type C () =
          member x.Foo() = ()
        """ ])

let private addMissingRecKeywordTests state =
  serverTestList (nameof AddMissingRecKeyword) state defaultConfigDto None (fun server ->
    [
      // `rec` in single function is handled in `MakeOuterBindingRecursive`
      testCaseAsync "can add rec to mutual recursive function"
      <| CodeFix.check
        server
        """
        $0let a x = x
        and b x = x
        """
        (Diagnostics.expectCode "576")
        (CodeFix.withTitle (AddMissingRecKeyword.title "a"))
        """
        let rec a x = x
        and b x = x
        """

      testCaseAsync "can add rec to mutually recursive functions with pattern matching"
      <| CodeFix.check
        server
        """
        $0let isEven x = 
          match x with
          | 0 -> true
          | n -> isOdd (n - 1)
        and isOdd x = 
          match x with
          | 0 -> false
          | n -> isEven (n - 1)
        """
        (Diagnostics.expectCode "576")
        (CodeFix.withTitle (AddMissingRecKeyword.title "isEven"))
        """
        let rec isEven x = 
          match x with
          | 0 -> true
          | n -> isOdd (n - 1)
        and isOdd x = 
          match x with
          | 0 -> false
          | n -> isEven (n - 1)
        """

      testCaseAsync "can add rec to mutually recursive functions with complex expressions"
      <| CodeFix.check
        server
        """
        $0let fibonacci n = 
          if n <= 1 then n
          else fibHelper (n - 1) + fibHelper (n - 2)
        and fibHelper n = fibonacci n
        """
        (Diagnostics.expectCode "576")
        (CodeFix.withTitle (AddMissingRecKeyword.title "fibonacci"))
        """
        let rec fibonacci n = 
          if n <= 1 then n
          else fibHelper (n - 1) + fibHelper (n - 2)
        and fibHelper n = fibonacci n
        """

      testCaseAsync "can add rec to first function when second function has complex parameters"
      <| CodeFix.check
        server
        """
        $0let processData items = 
          items |> List.map transformItem
        and transformItem item = 
          { item with Processed = processData item.Children }
        """
        (Diagnostics.expectCode "576")
        (CodeFix.withTitle (AddMissingRecKeyword.title "processData"))
        """
        let rec processData items = 
          items |> List.map transformItem
        and transformItem item = 
          { item with Processed = processData item.Children }
        """

      testCaseAsync "can add rec to functions with type annotations"
      <| CodeFix.check
        server
        """
        $0let parseExpression (tokens: string list) : int = 
          parseTerm tokens + parseExpression (List.tail tokens)
        and parseTerm (tokens: string list) : int = 
          int (List.head tokens)
        """
        (Diagnostics.expectCode "576")
        (CodeFix.withTitle (AddMissingRecKeyword.title "parseExpression"))
        """
        let rec parseExpression (tokens: string list) : int = 
          parseTerm tokens + parseExpression (List.tail tokens)
        and parseTerm (tokens: string list) : int = 
          int (List.head tokens)
        """

      testCaseAsync "can add rec to three mutually recursive functions"
      <| CodeFix.check
        server
        """
        $0let funcA x = funcB x + 1
        and funcB x = funcC x * 2
        and funcC x = funcA x - 1
        """
        (Diagnostics.expectCode "576")
        (CodeFix.withTitle (AddMissingRecKeyword.title "funcA"))
        """
        let rec funcA x = funcB x + 1
        and funcB x = funcC x * 2
        and funcC x = funcA x - 1
        """ ])

let private addNewKeywordToDisposableConstructorInvocationTests state =
  serverTestList (nameof AddNewKeywordToDisposableConstructorInvocation) state defaultConfigDto None (fun server ->
    [ let selectCodeFix =
        CodeFix.withTitle AddNewKeywordToDisposableConstructorInvocation.title

      testCaseAsync "can add new to Disposable"
      <| CodeFix.check
        server
        """
        open System.Threading.Tasks
        let _ = $0Task<int>(fun _ -> 1)
        """
        (Diagnostics.expectCode "760")
        selectCodeFix
        """
        open System.Threading.Tasks
        let _ = new Task<int>(fun _ -> 1)
        """

      testCaseAsync "can add new to Disposable with namespace"
      <| CodeFix.check
        server
        """
        let _ = System.Threading.Tasks.$0Task<int>(fun _ -> 1)
        """
        (Diagnostics.expectCode "760")
        selectCodeFix
        """
        let _ = new System.Threading.Tasks.Task<int>(fun _ -> 1)
        """

      testCaseAsync "doesn't trigger for not Disposable"
      <| CodeFix.checkNotApplicable
        server
        """
        let _ = System.$0String('.', 3)
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "adds parentheses when needed"
      <| CodeFix.check
        server
        """
        let path = "test.txt"
        let _ = System.IO.StreamReader path$0
        """
        (Diagnostics.expectCode "760")
        selectCodeFix
        """
        let path = "test.txt"
        let _ = new System.IO.StreamReader(path)
        """

      testCaseAsync "keeps space"
      <| CodeFix.check
        server
        """
        let stream = System.IO.MemoryStream ()$0
        """
        (Diagnostics.expectCode "760")
        selectCodeFix
        """
        let stream = new System.IO.MemoryStream ()
        """

      testCaseAsync "does not add space"
      <| CodeFix.check
        server
        """
        let stream = System.IO.MemoryStream()$0
        """
        (Diagnostics.expectCode "760")
        selectCodeFix
        """
        let stream = new System.IO.MemoryStream()
        """

      testCaseAsync "adds parentheses when needed and keeps indentation"
      <| CodeFix.check
        server
        """
        let path = "test.txt"
        let sr =
            System.IO.StreamReader
                path$0
        """
        (Diagnostics.expectCode "760")
        selectCodeFix
        """
        let path = "test.txt"
        let sr =
            new System.IO.StreamReader
                (path)
        """ ])

let private addTypeToIndeterminateValueTests state =
  serverTestList (nameof AddTypeToIndeterminateValue) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle AddTypeToIndeterminateValue.title

      testCaseAsync "can add type annotation to error 72 ('Lookup on object of indeterminate type')"
      <| CodeFix.check
        server
        """
        let data = [
          {| Name = "foo"; Value = 42 |}
          {| Name = "bar"; Value = 13 |}
        ]
        let res = List.filter (fun d -> $0d.Value > 20) data
        """
        (Diagnostics.expectCode "72")
        selectCodeFix
        """
        let data = [
          {| Name = "foo"; Value = 42 |}
          {| Name = "bar"; Value = 13 |}
        ]
        let res = List.filter (fun (d: {| Name: string; Value: int |}) -> d.Value > 20) data
        """

      testCaseAsync
        "can add type annotation to error 3245 ('The input to a copy-and-update expression that creates an anonymous record must be either an anonymous record or a record')"
      <| CodeFix.check
        server
        """
        [1..5]
        |> List.fold
            (fun s i ->
              match i % 2 with
              | 0 -> {| $0s with Evens = s.Evens + 1 |}
              | _ -> s
            )
            {| Evens = 0 |}
        """
        (Diagnostics.expectCode "3245")
        selectCodeFix
        """
        [1..5]
        |> List.fold
            (fun (s: {| Evens: int |}) i ->
              match i % 2 with
              | 0 -> {| s with Evens = s.Evens + 1 |}
              | _ -> s
            )
            {| Evens = 0 |}
        """ ])

let private changeDerefBangToValueTests state =
  serverTestList (nameof ChangeDerefBangToValue) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle ChangeDerefBangToValue.title

      testCaseAsync "can replace ! with .Value"
      <| CodeFix.check
        server
        """
        let rv = ref 5
        let v = $0!rv
        """
        (Diagnostics.expectCode "3370")
        selectCodeFix
        """
        let rv = ref 5
        let v = rv.Value
        """

      testCaseAsync "can replace ! with .Value when parens"
      <| CodeFix.check
        server
        """
        let rv = ref 5
        let v = $0!(rv)
        """
        (Diagnostics.expectCode "3370")
        selectCodeFix
        """
        let rv = ref 5
        let v = (rv).Value
        """

      testCaseAsync "can replace ! with .Value when function in parens"
      <| CodeFix.check
        server
        """
        let fr a = ref a
        let v = $0!(fr 5)
        """
        (Diagnostics.expectCode "3370")
        selectCodeFix
        """
        let fr a = ref a
        let v = (fr 5).Value
        """

      testCaseAsync "can replace ! with .Value when space between ! and variable"
      <| CodeFix.check
        server
        """
        let rv = ref 5
        let v = $0! rv
        """
        (Diagnostics.expectCode "3370")
        selectCodeFix
        """
        let rv = ref 5
        let v = rv.Value
        """

      testCaseAsync "can replace ! with .Value when when parens and space between ! and variable"
      <| CodeFix.check
        server
        """
        let rv = ref 5
        let v = $0! (rv)
        """
        (Diagnostics.expectCode "3370")
        selectCodeFix
        """
        let rv = ref 5
        let v = (rv).Value
        """ ])

let private changeDowncastToUpcastTests state =
  serverTestList (nameof ChangeDowncastToUpcast) state defaultConfigDto None (fun server ->
    [ let selectOperatorCodeFix =
        CodeFix.withTitle ChangeDowncastToUpcast.titleUpcastOperator

      let selectFunctionCodeFix =
        CodeFix.withTitle ChangeDowncastToUpcast.titleUpcastFunction

      testCaseAsync "can change :?> to :>"
      <| CodeFix.check
        server
        """
        type I = interface end
        type C() = interface I

        let v: I = C() $0:?> I
        """
        (Diagnostics.expectCode "3198")
        selectOperatorCodeFix
        """
        type I = interface end
        type C() = interface I

        let v: I = C() :> I
        """

      testCaseAsync "can change downcast to upcast"
      <| CodeFix.check
        server
        """
        type I = interface end
        type C() = interface I

        let v: I = $0downcast C()
        """
        (Diagnostics.expectCode "3198")
        selectFunctionCodeFix
        """
        type I = interface end
        type C() = interface I

        let v: I = upcast C()
        """

      () ])

let private changeEqualsInFieldTypeToColonTests state =
  serverTestList (nameof ChangeEqualsInFieldTypeToColon) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle ChangeEqualsInFieldTypeToColon.title

      testCaseAsync "can change = to : in single line"
      <| CodeFix.check
        server
        """
        type A = { Name : string; Key $0= int }
        """
        (Diagnostics.expectCode "10")
        selectCodeFix
        """
        type A = { Name : string; Key : int }
        """

      testCaseAsync "can change = to : in multi line"
      <| CodeFix.check
        server
        """
        type A = {
          Name : string
          Key $0= int
        }
        """
        (Diagnostics.expectCode "10")
        selectCodeFix
        """
        type A = {
          Name : string
          Key : int
        }
        """

      testCaseAsync "can change = to : with complex type annotation"
      <| CodeFix.check
        server
        """
        type Person = { 
          Name : string
          Age : int
          Addresses $0= list<string>
        }
        """
        (Diagnostics.expectCode "10")
        selectCodeFix
        """
        type Person = { 
          Name : string
          Age : int
          Addresses : list<string>
        }
        """

      testCaseAsync "can change = to : with generic type"
      <| CodeFix.check
        server
        """
        type Container<'T> = { 
          Value $0= 'T
          Metadata : string
        }
        """
        (Diagnostics.expectCode "10")
        selectCodeFix
        """
        type Container<'T> = { 
          Value : 'T
          Metadata : string
        }
        """

      testCaseAsync "can change = to : with option type"
      <| CodeFix.check
        server
        """
        type Config = {
          ConnectionString : string
          Timeout $0= int option
        }
        """
        (Diagnostics.expectCode "10")
        selectCodeFix
        """
        type Config = {
          ConnectionString : string
          Timeout : int option
        }
        """

      testCaseAsync "can change = to : with tuple type"
      <| CodeFix.check
        server
        """
        type Point = { 
          Coordinates $0= int * int
          Name : string 
        }
        """
        (Diagnostics.expectCode "10")
        selectCodeFix
        """
        type Point = { 
          Coordinates : int * int
          Name : string 
        }
        """

      testCaseAsync "can change = to : with function type"
      <| CodeFix.check
        server
        """
        type Processor = {
          Transform $0= string -> int
          Name : string
        }
        """
        (Diagnostics.expectCode "10")
        selectCodeFix
        """
        type Processor = {
          Transform : string -> int
          Name : string
        }
        """

      testCaseAsync "can change = to : in nested record with indentation"
      <| CodeFix.check
        server
        """
        module Data =
          type User = {
            Id : int
            Profile $0= {| Name: string; Email: string |}
          }
        """
        (Diagnostics.expectCode "10")
        selectCodeFix
        """
        module Data =
          type User = {
            Id : int
            Profile : {| Name: string; Email: string |}
          }
        """ ])

let private changePrefixNegationToInfixSubtractionTests state =
  serverTestList (nameof ChangePrefixNegationToInfixSubtraction) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = 
        CodeFix.ofKind "quickfix" >> CodeFix.withTitle ChangePrefixNegationToInfixSubtraction.title

      testCaseAsync "converts negation to subtraction"
      <| CodeFix.check
        server
        """
        let getListWithoutFirstAndLastElement list =
          let l = List.length list
          list[ 1 .. $0l -1 ]
        """
        (Diagnostics.expectCode "3")
        selectCodeFix
        """
        let getListWithoutFirstAndLastElement list =
          let l = List.length list
          list[ 1 .. l - 1 ]
        """

      testCaseAsync "converts negation to subtraction in arithmetic expression"
      <| CodeFix.check
        server
        """
        let calculate x y = 
          let result = x + $0y -5
          result
        """
        (Diagnostics.expectCode "3")
        selectCodeFix
        """
        let calculate x y = 
          let result = x + y - 5
          result
        """

      testCaseAsync "converts negation to subtraction in complex arithmetic"
      <| CodeFix.check
        server
        """
        let formula a b c = a * b + $0c -10 / 2
        """
        (Diagnostics.expectCode "3")
        selectCodeFix
        """
        let formula a b c = a * b + c - 10 / 2
        """

      testCaseAsync "converts negation to subtraction in array indexing"
      <| CodeFix.check
        server
        """
        let getElement arr index = 
          let lastIndex = Array.length arr
          arr.[index..$0lastIndex -1]
        """
        (Diagnostics.expectCode "3")
        selectCodeFix
        """
        let getElement arr index = 
          let lastIndex = Array.length arr
          arr.[index..lastIndex - 1]
        """

      testCaseAsync "converts negation to subtraction in function call context"
      <| CodeFix.check
        server
        """
        let processRange start count = 
          List.init count (fun i -> start + i)
          |> List.take ($0count -2)
        """
        (Diagnostics.expectCode "3")
        selectCodeFix
        """
        let processRange start count = 
          List.init count (fun i -> start + i)
          |> List.take (count - 2)
        """

      testCaseAsync "converts negation to subtraction in nested expression"
      <| CodeFix.check
        server
        """
        let nested x = 
          match x with
          | n when n > 0 -> Some(n + $0x -1)
          | _ -> None
        """
        (Diagnostics.expectCode "3")
        selectCodeFix
        """
        let nested x = 
          match x with
          | n when n > 0 -> Some(n + x - 1)
          | _ -> None
        """

      testCaseAsync "converts negation to subtraction with variable names"
      <| CodeFix.check
        server
        """
        let difference first second = 
          let gap = first - second
          gap + $0first -gap
        """
        (Diagnostics.expectCode "3")
        selectCodeFix
        """
        let difference first second = 
          let gap = first - second
          gap + first - gap
        """ ])

let private changeRefCellDerefToNotTests state =
  serverTestList (nameof ChangeRefCellDerefToNot) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle ChangeRefCellDerefToNot.title

      testCaseAsync "can change simple deref to not"
      <| CodeFix.check
        server
        """
        let x = 1
        !$0x
        """
        (Diagnostics.expectCode "1")
        selectCodeFix
        """
        let x = 1
        not x
        """

      testCaseAsync "can change simple deref with parens to not"
      <| CodeFix.check
        server
        """
        let x = 1
        !($0x)
        """
        (Diagnostics.expectCode "1")
        selectCodeFix
        """
        let x = 1
        not (x)
        """

      testCaseAsync "can change deref of binary expr to not"
      <| CodeFix.check
        server
        """
        let x = 1
        !($0x = false)
        """
        (Diagnostics.expectCode "1")
        selectCodeFix
        """
        let x = 1
        not (x = false)
        """ ])

let private changeTypeOfNameToNameOfTests state =
  serverTestList (nameof ChangeTypeOfNameToNameOf) state defaultConfigDto None (fun server ->
    [ testCaseAsync "can suggest fix"
      <| CodeFix.check
        server
        """
        let x = $0typeof<Async<string>>.Name
        """
        (Diagnostics.acceptAll)
        (CodeFix.ofKind "refactor" >> CodeFix.withTitle ChangeTypeOfNameToNameOf.title)
        """
        let x = nameof(Async<string>)
        """ ])

let private convertBangEqualsToInequalityTests state =
  serverTestList (nameof ConvertBangEqualsToInequality) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle ConvertBangEqualsToInequality.title

      testCaseAsync "can change != to <>"
      <| CodeFix.check
        server
        """
        1 $0!= 2
        """
        (Diagnostics.expectCode "43")
        selectCodeFix
        """
        1 <> 2
        """ ])

let private convertCSharpLambdaToFSharpLambdaTests state =
  serverTestList (nameof ConvertCSharpLambdaToFSharpLambda) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle ConvertCSharpLambdaToFSharpLambda.title

      testCaseAsync "can convert csharp lambda in variable assignment with cursor on input"
      <| CodeFix.check
        server
        """
        let x = $0y => 1 + y
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let x = fun y -> 1 + y
        """

      testCaseAsync "can convert csharp lambda in variable assignment with cursor on usage"
      <| CodeFix.check
        server
        """
        let x = y => 1 + $0y
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let x = fun y -> 1 + y
        """
      //ENHANCEMENT: trigger on `=>`
      itestCaseAsync "can convert csharp lambda in variable assignment with cursor on =>"
      <| CodeFix.check
        server
        """
        let x = y $0=> 1 + y
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let x = fun y -> 1 + y
        """

      testCaseAsync "can convert csharp lambda in lambda with parens with cursor on input"
      <| CodeFix.check
        server
        """
        [1..10] |> List.map ($0x => 1 + x)
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        [1..10] |> List.map (fun x -> 1 + x)
        """

      testCaseAsync "can convert csharp lambda in lambda with parens with cursor on usage"
      <| CodeFix.check
        server
        """
        [1..10] |> List.map (x => 1 + $0x)
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        [1..10] |> List.map (fun x -> 1 + x)
        """

      testCaseAsync "keep multi-line lambda intact - cursor on input"
      <| CodeFix.check
        server
        """
        let x =
          $0y =>
            let a = 1 + y
            a
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let x =
          fun y ->
            let a = 1 + y
            a
        """

      testCaseAsync "keep multi-line lambda intact - cursor on usage"
      <| CodeFix.check
        server
        """
        let x =
          y =>
            let a = 1 + $0y
            a
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let x =
          fun y ->
            let a = 1 + y
            a
        """ ])

let private convertDoubleEqualsToSingleEqualsTests state =
  serverTestList (nameof ConvertDoubleEqualsToSingleEquals) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle ConvertDoubleEqualsToSingleEquals.title

      testCaseAsync "can replace == with ="
      <| CodeFix.check
        server
        """
        1 $0== 1
        """
        (Diagnostics.expectCode "43")
        selectCodeFix
        """
        1 = 1
        """

      testCaseAsync "doesn't replace existing operator == with ="
      <| CodeFix.checkNotApplicable
        server
        """
        let (==) a b = a = b
        1 $0== 1
        """
        Diagnostics.acceptAll
        selectCodeFix ])

let private convertInvalidRecordToAnonRecordTests state =
  serverTestList (nameof ConvertInvalidRecordToAnonRecord) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle ConvertInvalidRecordToAnonRecord.title

      testCaseAsync "can convert single-line record with single field"
      <| CodeFix.check
        server
        """
        let v = { $0Name = "foo" }
        """
        (Diagnostics.expectCode "39")
        selectCodeFix
        """
        let v = {| Name = "foo" |}
        """

      testCaseAsync "can convert single-line record with two fields"
      <| CodeFix.check
        server
        """
        let v = { $0Name = "foo"; Value = 42 }
        """
        (Diagnostics.expectCode "39")
        selectCodeFix
        """
        let v = {| Name = "foo"; Value = 42 |}
        """

      testCaseAsync "can convert multi-line record with two fields"
      <| CodeFix.check
        server
        """
        let v = {
          $0Name = "foo"
          Value = 42
        }
        """
        (Diagnostics.expectCode "39")
        selectCodeFix
        """
        let v = {|
          Name = "foo"
          Value = 42
        |}
        """

      testCaseAsync "doesn't trigger for existing record"
      <| CodeFix.checkNotApplicable
        server
        """
        type V = { Name: string; Value: int }
        let v = { $0Name = "foo"; Value = 42 }
        """
        (Diagnostics.acceptAll)
        selectCodeFix

      testCaseAsync "doesn't trigger for anon record"
      <| CodeFix.checkNotApplicable
        server
        """
        let v = {| $0Name = "foo"; Value = 42 |}
        """
        (Diagnostics.acceptAll)
        selectCodeFix ])

let private convertPositionalDUToNamedTests state =
  serverTestList (nameof ConvertPositionalDUToNamed) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle ConvertPositionalDUToNamed.title

      testCaseAsync "in parenthesized let binding"
      <| CodeFix.check
        server
        """
        type A = A of a: int * b: bool

        let (A(a$0, b)) = A(1, true)
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type A = A of a: int * b: bool

        let (A(a = a; b = b;)) = A(1, true)
        """

      testCaseAsync "in simple match"
      <| CodeFix.check
        server
        """
        type A = A of a: int * b: bool

        match A(1, true) with
        | A(_, false) -> ()
        | A(a$0, b) -> ()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type A = A of a: int * b: bool

        match A(1, true) with
        | A(_, false) -> ()
        | A(a = a; b = b;) -> ()
        """

      testCaseAsync "in parenthesized match"
      <| CodeFix.check
        server
        """
        type A = A of a: int * b: bool

        match A(1, true) with
        | (A(_, false)) -> ()
        | (A(a$0, b)) -> ()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type A = A of a: int * b: bool

        match A(1, true) with
        | (A(_, false)) -> ()
        | (A(a = a; b = b;)) -> ()
        """

      testCaseAsync "when there is one new field on the DU"
      <| CodeFix.check
        server
        """
        type ThirdFieldWasJustAdded = ThirdFieldWasJustAdded of a: int * b: bool * c: char

        let (ThirdFieldWasJustAdded($0a, b)) = ThirdFieldWasJustAdded(1, true, 'c')
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type ThirdFieldWasJustAdded = ThirdFieldWasJustAdded of a: int * b: bool * c: char

        let (ThirdFieldWasJustAdded(a = a; b = b; c = _;)) = ThirdFieldWasJustAdded(1, true, 'c')
        """

      testCaseAsync "when there are multiple new fields on the DU"
      <| CodeFix.check
        server
        """
        type U = U of aValue: int * boolean: int * char: char * dec: decimal * element: int
        let (U($0a, b)) = failwith "..."
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type U = U of aValue: int * boolean: int * char: char * dec: decimal * element: int
        let (U(aValue = a; boolean = b; char = _; dec = _; element = _;)) = failwith "..."
        """

      testCaseAsync "when the existing pattern isn't formatted well"
      <| CodeFix.check
        server
        """
        type A = A of a: int * b: bool * c: bool * d: bool
        let (A($0a,b, c,     d)) = A(1, true, false, false)
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type A = A of a: int * b: bool * c: bool * d: bool
        let (A(a = a;b = b; c = c;     d = d;)) = A(1, true, false, false)
        """

      testCaseAsync "when there are multiple SynLongIdent Pats"
      <| CodeFix.check
        server
        """
        type MyDiscUnion = Case1 of field1: int * field2: int

        type MyC() =

          let x = Case1 (1, 2)

          member _.Func2 () =
            match x with
            | Case1(3$0, 4) -> ()
            | _ -> ()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyDiscUnion = Case1 of field1: int * field2: int

        type MyC() =

          let x = Case1 (1, 2)

          member _.Func2 () =
            match x with
            | Case1(field1 = 3; field2 = 4;) -> ()
            | _ -> ()
        """

      testCaseAsync "when surrounding function takes union parameter"
      <| CodeFix.check
        server
        """
        type MyDiscUnion = X of field1: int * field2: int

        let f () =
          let x = X (1, 2)
          match x with
          | X(32$0, 23) -> ()
          | _ -> ()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyDiscUnion = X of field1: int * field2: int

        let f () =
          let x = X (1, 2)
          match x with
          | X(field1 = 32; field2 = 23;) -> ()
          | _ -> ()
        """ ])

let private addPrivateAccessModifierTests state =
  let config =
    { defaultConfigDto with
        AddPrivateAccessModifier = Some true }

  serverTestList (nameof AddPrivateAccessModifier) state config None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle AddPrivateAccessModifier.title

      testCaseAsync "add private works for simple function"
      <| CodeFix.check
        server
        """
        let f$0 x = x * x
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let private f x = x * x
        """

      testCaseAsync "add private works for simple identifier"
      <| CodeFix.check
        server
        """
        let x$0 = 23
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let private x = 23
        """

      testCaseAsync "add private works for simple identifier used in other private function"
      <| CodeFix.check
        server
        """
        module PMod =
          let xx$0x = 10

          module PMod2 =
            let insidePMod2 = 23

          let private a = 23

          let private g z =
            let sF y = y + xxx
            z
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        module PMod =
          let private xxx = 10

          module PMod2 =
            let insidePMod2 = 23

          let private a = 23

          let private g z =
            let sF y = y + xxx
            z
        """

      testCaseAsync "add private is not offered for already private functions"
      <| CodeFix.checkNotApplicable
        server
        """
        let private f$0 x = x * x
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private is not offered for function with reference outside its declaring module"
      <| CodeFix.checkNotApplicable
        server
        """
        module MyModule =

            let helper x = x + 10

            let $0f x = helper x

        MyModule.f 10
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private works for class type definition"
      <| CodeFix.check
        server
        """
        type [<System.Obsolete>] MyCla$0ss() =
          member _.X = 10
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type [<System.Obsolete>] private MyClass() =
          member _.X = 10
        """

      testCaseAsync "add private is not offered for class type definition with reference"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyCla$0ss() =
          member _.X = 10

        let _ = MyClass()
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private is not offered for explicit ctor" // ref finding might not show us usages
      <| CodeFix.checkNotApplicable
        server
        """
        type MyC(x: int) =
          ne$0w() =
            MyC(23)
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private is not offered for member with reference outside its declaring class"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyClass() =
          member _.$0X = 10

        let myInst = MyClass()
        myInst.X |> ignore
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync
        "add private is not offered for member with reference outside its declaring class when caret is on thisValue"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyClass() =
          member _$0.X = 10

        let myInst = MyClass()
        myInst.X |> ignore
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private is not offered for member when caret is in SynTypArDecl"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyC() =
          member _.X<'T$0> a = a
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private is not offered for class member when caret is on parameter"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyClass() =
          member _.Func x$0 = x
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private is not offered for let bindings inside a class"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyClass() =
          let $0f x = x * x
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private works for class member"
      <| CodeFix.check
        server
        """
        type MyClass() =
          member _.$0X = 10
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyClass() =
          member private _.X = 10
        """

      testCaseAsync "add private works for autoproperty"
      <| CodeFix.check
        server
        """
        type MyClass() =
          member val Name$0 = "" with get, set
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyClass() =
          member val private Name = "" with get, set
        """

      testCaseAsync "add private works for autoproperty with get"
      <| CodeFix.check
        server
        """
        type MyClass() =
          member val Name$0 = "" with get
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyClass() =
          member val private Name = "" with get
        """

      testCaseAsync "add private is not offered for autoproperty with references outside its class"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyClass() =
          member val Name$0 = "" with get, set

        let myInst = MyClass()
        myInst.Name |> ignore
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private works for autoproperty getter"
      <| CodeFix.check
        server
        """
        type MyClass() =
          member val Name = "" with $0get, set
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyClass() =
          member val Name = "" with private get, set
        """

      testCaseAsync "add private works for autoproperty setter"
      <| CodeFix.check
        server
        """
        type MyClass() =
          member val Name = "" with get, s$0et
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyClass() =
          member val Name = "" with get, private set
        """

      testCaseAsync "add private is not offered for autoproperty getter if member access is already there"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyClass() =
          member val private Name = "" with g$0et, set
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private is not offered for autoproperty setter if member access is already there"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyClass() =
          member val private Name = "" with get, set$0
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private is not offered for DU type definition" // ref finding might not show us type inferred usages
      <| CodeFix.checkNotApplicable
        server
        """
        type [<System.Obsolete>] MyDi$0scUnion =
        | Case1
        | Case2
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private is not offered for member with reference outside its declaring DU"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyDiscUnion =
        | Case1
        | Case2
        with
          member _.F$0oo x = x

        let x = MyDiscUnion.Case1
        x.Foo 10
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync
        "add private is not offered for member with reference outside its declaring DU when caret is on thisValue"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyDiscUnion =
        | Case1
        | Case2
        with
          member $0_.Foo x = x

        let x = MyDiscUnion.Case1
        x.Foo 10
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private is not offered for DU member when caret is on parameter"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyDiscUnion =
        | Case1
        | Case2
        with
          member _.Foo $0x = x
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private works for DU member"
      <| CodeFix.check
        server
        """
        type MyDiscUnion =
        | Case1
        | Case2
        with
          member _.Fo$0o x = x
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyDiscUnion =
        | Case1
        | Case2
        with
          member private _.Foo x = x
        """

      testCaseAsync "add private is not offered for Record type definition" // ref finding might not show us type inferred usages
      <| CodeFix.checkNotApplicable
        server
        """
        type [<System.Obsolete>] My$0Record =
          { Field1: int
            Field2: string }
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private is not offered for member with reference outside its declaring Record"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyRecord =
          { Field1: int
            Field2: string }
        with
          member _.F$0oo x = x

        let x = { Field1 = 23; Field2 = "bla" }
        x.Foo 10
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync
        "add private is not offered for member with reference outside its declaring Record when caret is on thisValue"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyRecord =
          { Field1: int
            Field2: string }
        with
          member _$0.Foo x = x

        let x = { Field1 = 23; Field2 = "bla" }
        x.Foo 10
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private is not offered for Record member when caret is on parameter"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyRecord =
          { Field1: int
            Field2: string }
        with
          member _.Foo $0x = x
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private works for Record member"
      <| CodeFix.check
        server
        """
        type MyRecord =
          { Field1: int
            Field2: string }
        with
          member _.Fo$0o x = x
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyRecord =
          { Field1: int
            Field2: string }
        with
          member private _.Foo x = x
        """

      testCaseAsync "add private works for top level module"
      <| CodeFix.check
        server
        """
        module [<System.Obsolete>] rec M$0

          module Sub = ()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        module [<System.Obsolete>] private rec M

          module Sub = ()
        """

      testCaseAsync "add private works for module"
      <| CodeFix.check
        server
        """
        module [<System.Obsolete>] rec M$0 =
          ()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        module [<System.Obsolete>] private rec M =
          ()
        """

      testCaseAsync "add private is not offered for module with references outside its declaring module"
      <| CodeFix.checkNotApplicable
        server
        """
        module M =
          module N$0 =
              let foofoo = 10

        M.N.foofoo |> ignore
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "add private works for type abbreviation"
      <| CodeFix.check
        server
        """
        type My$0Int = int
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type private MyInt = int
        """

      testCaseAsync "add private is not offered for type abbreviation with reference outside its declaring module"
      <| CodeFix.checkNotApplicable
        server
        """
        module M =
          type My$0Int = int

        let x: M.MyInt = 23
        """
        Diagnostics.acceptAll
        selectCodeFix ])

let private convertTripleSlashCommentToXmlTaggedDocTests state =
  serverTestList (nameof ConvertTripleSlashCommentToXmlTaggedDoc) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle ConvertTripleSlashCommentToXmlTaggedDoc.title

      testCaseAsync "single line comment over top level function"
      <| CodeFix.check
        server
        """
        /// $0line          1
        let f () = ()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary>line          1</summary>
        let f () = ()
        """

      testCaseAsync "multiline comments over top level function"
      <| CodeFix.check
        server
        """
        /// $0line          1
        /// line  2
        /// line   3
        /// line    4
        let f a b c = a + b
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary>
        /// line          1
        /// line  2
        /// line   3
        /// line    4
        /// </summary>
        let f a b c = a + b
        """

      testCaseAsync "multiline comments over nested function"
      <| CodeFix.check
        server
        """
        /// line          1
        /// line  2
        /// line   3
        let g () =
                /// line          1
                /// line  2
                /// line   $03
                /// line    4
                let f x = x * x
                ()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// line          1
        /// line  2
        /// line   3
        let g () =
                /// <summary>
                /// line          1
                /// line  2
                /// line   3
                /// line    4
                /// </summary>
                let f x = x * x
                ()
        """

      testCaseAsync "single line comment over use"
      <| CodeFix.check
        server
        """
        let f a b _ =
            /// line on use$0
            use r = new System.IO.BinaryReader(null)

            a + b
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let f a b _ =
            /// <summary>line on use</summary>
            use r = new System.IO.BinaryReader(null)

            a + b
        """

      testCaseAsync "multiline comments over record type"
      <| CodeFix.check
        server
        """
        /// line          1
        /// line  2
        /// line   3
        $0/// line    4
        type MyRecord = { Foo: int }
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary>
        /// line          1
        /// line  2
        /// line   3
        /// line    4
        /// </summary>
        type MyRecord = { Foo: int }
        """

      testCaseAsync "multiline comments over discriminated union type"
      <| CodeFix.check
        server
        """
        /// line 1 on DU
        /// $0line 2 on DU
        /// line 3 on DU
        type DiscUnionTest =
          /// line 1 on Field 1
          /// line 2 on Field 1
          | Field1
          /// line 1 on Field 2
          /// line 2 on Field 2
          | Field2
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary>
        /// line 1 on DU
        /// line 2 on DU
        /// line 3 on DU
        /// </summary>
        type DiscUnionTest =
          /// line 1 on Field 1
          /// line 2 on Field 1
          | Field1
          /// line 1 on Field 2
          /// line 2 on Field 2
          | Field2
        """

      testCaseAsync "multiline comments over discriminated union field"
      <| CodeFix.check
        server
        """
        /// line 1 on DU
        /// line 2 on DU
        /// line 3 on DU
        type DiscUnionTest =
          /// line 1 on Field 1
          /// line 2 on Field 1
          | Field1
          /// line 1 $0on Field 2
          /// line 2 on Field 2
          | Field2
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// line 1 on DU
        /// line 2 on DU
        /// line 3 on DU
        type DiscUnionTest =
          /// line 1 on Field 1
          /// line 2 on Field 1
          | Field1
          /// <summary>
          /// line 1 on Field 2
          /// line 2 on Field 2
          /// </summary>
          | Field2
        """

      testCaseAsync "multiline comments over enum"
      <| CodeFix.check
        server
        """
        $0/// line 1 on enum
        /// line 2 on enum
        type myEnum =
        | value1 = 1
        | value2 = 2
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary>
        /// line 1 on enum
        /// line 2 on enum
        /// </summary>
        type myEnum =
        | value1 = 1
        | value2 = 2
        """

      testCaseAsync "multiline comment over class"
      <| CodeFix.check
        server
        """
        //$0/ On Class 1
        /// On Class 2
        type MyClass() =
          /// On member 1
          /// On member 2
          member val Name = "" with get, set
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary>
        /// On Class 1
        /// On Class 2
        /// </summary>
        type MyClass() =
          /// On member 1
          /// On member 2
          member val Name = "" with get, set
        """

      testCaseAsync "multiline comment over member"
      <| CodeFix.check
        server
        """
        type MyClass() =
          /// on new 1
          $0/// on new 2
          new() = MyClass()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyClass() =
          /// <summary>
          /// on new 1
          /// on new 2
          /// </summary>
          new() = MyClass()
        """

      testCaseAsync "multiline comment over autoproperty"
      <| CodeFix.check
        server
        """
        type MyClass() =
          /// line 1 on autoproperty
          /// li$0ne 2 on autoproperty
          member val Name = "" with get, set
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyClass() =
          /// <summary>
          /// line 1 on autoproperty
          /// line 2 on autoproperty
          /// </summary>
          member val Name = "" with get, set
        """

      testCaseAsync "multiline comment over named module"
      <| CodeFix.check
        server
        """
        $0/// On named module 1
        /// On named module 2
        module M
          let f x = x
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary>
        /// On named module 1
        /// On named module 2
        /// </summary>
        module M
          let f x = x
        """

      testCaseAsync "multiline comment over nested module"
      <| CodeFix.check
        server
        """
        module M
          module MyNestedModule =
              /// Line 1 on$0 MyNestedNestedModule
              /// Line 2 on MyNestedNestedModule
              module MyNestedNestedModule =
                let x = 3
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        module M
          module MyNestedModule =
              /// <summary>
              /// Line 1 on MyNestedNestedModule
              /// Line 2 on MyNestedNestedModule
              /// </summary>
              module MyNestedNestedModule =
                let x = 3
        """

      testCaseAsync "is not applicable to existing xml tag comment"
      <| CodeFix.checkNotApplicable
        server
        """
        /// <summary>
        /// foo$0
        /// ...
        """
        Diagnostics.acceptAll
        selectCodeFix ])

let private generateXmlDocumentationTests state =
  serverTestList (nameof GenerateXmlDocumentation) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle GenerateXmlDocumentation.title

      testCaseAsync "documentation for function with two parameters"
      <| CodeFix.check
        server
        """
        let $0f x y = x + y
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary></summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        let f x y = x + y
        """

      testCaseAsync "documentation for function with attribute"
      <| CodeFix.check
        server
        """
        [<System.Obsolete("foo")>]
        let $0f x y = x + y
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary></summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        [<System.Obsolete("foo")>]
        let f x y = x + y
        """

      testCaseAsync "documentation for use"
      <| CodeFix.check
        server
        """
        let f a b _ =
          use $0r = new System.IO.BinaryReader(null)

          a + b
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let f a b _ =
          /// <summary></summary>
          /// <returns></returns>
          use r = new System.IO.BinaryReader(null)

          a + b
        """

      testCaseAsync "documentation for record type"
      <| CodeFix.check
        server
        """
        type MyRec$0ord = { Foo: int }
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary></summary>
        type MyRecord = { Foo: int }
        """

      testCaseAsync "documentation for record type with multiple attribute lists"
      <| CodeFix.check
        server
        """
        [<System.Obsolete("foo")>]
        [<System.Serializable>]
        type MyRec$0ord = { Foo: int }
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary></summary>
        [<System.Obsolete("foo")>]
        [<System.Serializable>]
        type MyRecord = { Foo: int }
        """

      testCaseAsync "documentation for discriminated union type"
      <| CodeFix.check
        server
        """
        type Dis$0cUnionTest =
          | Field1
          | Field2
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary></summary>
        type DiscUnionTest =
          | Field1
          | Field2
        """

      testCaseAsync "documentation for discriminated union case"
      <| CodeFix.check
        server
        """
        type DiscUnionTest =
          | C$0ase1
          | Case2
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type DiscUnionTest =
          /// <summary></summary>
          | Case1
          | Case2
        """

      testCaseAsync "documentation for enum type"
      <| CodeFix.check
        server
        """
        type myE$0num =
        | value1 = 1
        | value2 = 2
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary></summary>
        type myEnum =
        | value1 = 1
        | value2 = 2
        """

      testCaseAsync "documentation for class type"
      <| CodeFix.check
        server
        """
        type MyC$0lass() =
          member val Name = "" with get, set
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary></summary>
        type MyClass() =
          member val Name = "" with get, set
        """

      testCaseAsync "documentation for member"
      <| CodeFix.check
        server
        """
        type MyClass() =
          n$0ew(x: int) = MyClass()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyClass() =
          /// <summary></summary>
          /// <param name="x"></param>
          /// <returns></returns>
          new(x: int) = MyClass()
        """

      testCaseAsync "documentation for autoproperty"
      <| CodeFix.check
        server
        """
        type MyClass() =
          member val Na$0me = "" with get, set
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyClass() =
          /// <summary></summary>
          /// <returns></returns>
          member val Name = "" with get, set
        """

      testCaseAsync "not applicable for explicit getter and setter"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyClass() =
          let mutable someField = ""
          member _.Name
            with $0get () = "foo"
            and set (x: string) = someField <- x
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "documentation on property with explicit getter and setter"
      <| CodeFix.check
        server
        """
        type MyClass() =
          let mutable someField = ""
          member _.$0Name
            with get () = "foo"
            and set (x: string) = someField <- x
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyClass() =
          let mutable someField = ""
          /// <summary></summary>
          /// <returns></returns>
          member _.Name
            with get () = "foo"
            and set (x: string) = someField <- x
        """

      testCaseAsync "documentation on property with explicit getter"
      <| CodeFix.check
        server
        """
        type MyClass() =
          let mutable someField = ""
          member _.$0Name
            with get () = "foo"
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyClass() =
          let mutable someField = ""
          /// <summary></summary>
          /// <returns></returns>
          member _.Name
            with get () = "foo"
        """

      testCaseAsync "documentation on property with explicit setter"
      <| CodeFix.check
        server
        """
        type MyClass() =
          let mutable someField = ""
          member _.$0Name
            with set (x: string) = someField <- x
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyClass() =
          let mutable someField = ""
          /// <summary></summary>
          /// <returns></returns>
          member _.Name
            with set (x: string) = someField <- x
        """

      testCaseAsync "not applicable for explicit getter"

      <| CodeFix.checkNotApplicable
        server
        """
        type MyClass() =
          member _.Name
            with $0get () = "foo"
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "not applicable for explicit setter"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyClass() =
          let mutable someField = ""
          member _.Name
            with s$0et (x: string) = someField <- x
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "documentation for named module"
      <| CodeFix.check
        server
        """
        module $0M
          let f x = x
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary></summary>
        module M
          let f x = x
        """

      testCaseAsync "documentation for nested module"
      <| CodeFix.check
        server
        """
        module M
          module MyNestedMo$0dule =
            let x = 3
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        module M
          /// <summary></summary>
          module MyNestedModule =
            let x = 3
        """

      testCaseAsync "not applicable for namespace"
      <| CodeFix.checkNotApplicable
        server
        """
        namespace N$0
          module MyNestedModule =
            let x = 3
        """
        Diagnostics.acceptAll
        selectCodeFix ])

let private addMissingXmlDocumentationTests state =
  serverTestList (nameof AddMissingXmlDocumentation) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle AddMissingXmlDocumentation.title

      testCaseAsync "missing params and returns for function with two parameters"
      <| CodeFix.check
        server
        """
        /// <summary>some comment$0</summary>
        let f x y = x + y
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary>some comment</summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        let f x y = x + y
        """

      testCaseAsync "missing params and returns for nested function with two parameters"
      <| CodeFix.check
        server
        """
        let f x y =
          /// <summary>some comment$0</summary>
          let g a b =
            a + b
          x + y
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let f x y =
          /// <summary>some comment</summary>
          /// <param name="a"></param>
          /// <param name="b"></param>
          /// <returns></returns>
          let g a b =
            a + b
          x + y
        """

      testCaseAsync "missing single parameter for function with two parameters"
      <| CodeFix.check
        server
        """
        /// <summary>some comment$0</summary>
        /// <param name="x"></param>
        /// <returns></returns>
        let f x y = x + y
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary>some comment</summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        let f x y = x + y
        """

      testCaseAsync "missing type parameter for function"
      <| CodeFix.check
        server
        """
        /// <summary>some comment$0</summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        let f x y _ = x + y
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary>some comment</summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <param name=""></param>
        /// <typeparam name="'a"></typeparam>
        /// <returns></returns>
        let f x y _ = x + y
        """

      testCaseAsync "wraps single line non-xml comment"
      <| CodeFix.check
        server
        """
        /// some comment$0
        let f x y = x + y
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary>some comment</summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        let f x y = x + y
        """

      testCaseAsync "wraps multi line non-xml comment"
      <| CodeFix.check
        server
        """
        /// some comment here
        /// some comment there$0
        let f x y = x + y
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        /// <summary>
        /// some comment here
        /// some comment there
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        let f x y = x + y
        """

      testCaseAsync "missing returns for use"
      <| CodeFix.check
        server
        """
        let f a b _ =
          /// <summary>$0</summary>
          use r = new System.IO.BinaryReader(null)

          a + b
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let f a b _ =
          /// <summary></summary>
          /// <returns></returns>
          use r = new System.IO.BinaryReader(null)

          a + b
        """

      testCaseAsync "not applicable for type with summary"
      <| CodeFix.checkNotApplicable
        server
        """
        /// <summary>some comment$0</summary>
        type MyClass() =
          member val Name = "" with get, set
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "missing returns for member"
      <| CodeFix.check
        server
        """
        type MyClass() =
          /// <sum$0mary>some comment</summary>
          new(x: int) = MyClass()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyClass() =
          /// <summary>some comment</summary>
          /// <param name="x"></param>
          /// <returns></returns>
          new(x: int) = MyClass()
        """

      testCaseAsync "missing returns for autoproperty"
      <| CodeFix.check
        server
        """
        type MyClass() =
          /// <summary>$0</summary>
          member val Name = "" with get, set
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type MyClass() =
          /// <summary></summary>
          /// <returns></returns>
          member val Name = "" with get, set
        """

      testCaseAsync "not applicable for autoproperty with summary and returns"
      <| CodeFix.checkNotApplicable
        server
        """
        type MyClass() =
          /// <summary>$0</summary>
          /// <returns></returns>
          member val Name = "" with get, set
        """
        Diagnostics.acceptAll
        selectCodeFix ])

let private generateRecordStubTests state =
  let config =
    { defaultConfigDto with
        RecordStubGeneration = Some true
        RecordStubGenerationBody = Some "failwith \"---\"" }

  serverTestList (nameof GenerateRecordStub) state config None (fun server ->
    [ CodeFix.testAllPositions
        "can generate record stubs for every pos in the record as soon as one field is known"
        server
        """
      type R = { a: string; b: int }

      let a = $0{  $0a = $0"";$0  }$0
      """
        (Diagnostics.expectCode "764")
        (CodeFix.withTitle GenerateRecordStub.title)
        """
      type R = { a: string; b: int }

      let a = {  a = "";
                 b = failwith "---"  }
      """ ])

let private generateUnionCasesTests state =
  let config =
    { defaultConfigDto with
        UnionCaseStubGeneration = Some true
        UnionCaseStubGenerationBody = Some "failwith \"---\"" }

  serverTestList (nameof GenerateUnionCases) state config None (fun server ->
    [ let _selectCodeFix = CodeFix.withTitle GenerateUnionCases.title

      testCaseAsync "can generate match cases for a simple DU with one case"
      <| CodeFix.check
        server
        """
        type Letter = A | B | C

        let char = A

        match $0char with
        | A -> ()
        """
        (Diagnostics.expectCode "25")
        (CodeFix.withTitle GenerateUnionCases.title)
        """
        type Letter = A | B | C

        let char = A

        match char with
        | A -> ()
        | B -> failwith "---"
        | C -> failwith "---"
        """

      testCaseAsync "can generate match cases for a simple DU without cases"
      <| CodeFix.check
        server
        """
        type Letter = A | B | C

        let char = A

        match $0char with
        """
        (Diagnostics.expectCode "25")
        (CodeFix.withTitle GenerateUnionCases.title)
        """
        type Letter = A | B | C

        let char = A

        match char with
        | A -> failwith "---"
        | B -> failwith "---"
        | C -> failwith "---"
        """ ])

let private makeDeclarationMutableTests state =
  serverTestList (nameof MakeDeclarationMutable) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle MakeDeclarationMutable.title

      testCaseAsync "can make decl mutable in top level assignment"
      <| CodeFix.check
        server
        """
        let x = 0
        x $0<- 1
        """
        (Diagnostics.expectCode "27")
        selectCodeFix
        """
        let mutable x = 0
        x <- 1
        """

      testCaseAsync "can make decl mutable in nested assignment"
      <| CodeFix.check
        server
        """
        let x = 0
        let _ =
          x $0<- 1
          ()
        """
        (Diagnostics.expectCode "27")
        selectCodeFix
        """
        let mutable x = 0
        let _ =
          x <- 1
          ()
        """

      testCaseAsync "can make decl mutable in function"
      <| CodeFix.check
        server
        """
        let count xs =
          let counter = 0
          for x in xs do
            counter $0<- counter + 1
          counter
        """
        (Diagnostics.expectCode "27")
        selectCodeFix
        """
        let count xs =
          let mutable counter = 0
          for x in xs do
            counter <- counter + 1
          counter
        """

      testCaseAsync "doesn't trigger for already mutable variable"
      <| CodeFix.checkNotApplicable
        server
        """
        let mutable x = 0
        x $0<- 1
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "doesn't trigger for immutable parameter"
      <| CodeFix.checkNotApplicable
        server
        """
        let f (v: int) =
          v $0<- 1
          v
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "doesn't trigger for immutable member parameter"
      <| CodeFix.checkNotApplicable
        server
        """
        type C() =
          member _.M(v: int)
            v $0<- 1
        """
        Diagnostics.acceptAll
        selectCodeFix ])

let private makeOuterBindingRecursiveTests state =
  serverTestList (nameof MakeOuterBindingRecursive) state defaultConfigDto None (fun server ->
    [ testCaseAsync "can make the outer binding recursive when self-referential"
      <| CodeFix.check
        server
        """
        let mySum xs acc =
            match xs with
            | [] -> acc
            | _ :: tail ->
                $0mySum tail (acc + 1)
        """
        (Diagnostics.expectCode "39")
        (CodeFix.ofKind "quickfix" >> CodeFix.withTitle MakeOuterBindingRecursive.title)
        """
        let rec mySum xs acc =
            match xs with
            | [] -> acc
            | _ :: tail ->
                mySum tail (acc + 1)
        """ ])

let private removeRedundantQualifierTests state =
  let config =
    { defaultConfigDto with
        SimplifyNameAnalyzer = Some true }

  serverTestList (nameof RemoveRedundantQualifier) state config None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle RemoveRedundantQualifier.title

      testCaseAsync "can remove redundant namespace"
      <| CodeFix.check
        server
        """
        open System
        let _ = $0System.String.IsNullOrWhiteSpace "foo"
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        open System
        let _ = String.IsNullOrWhiteSpace "foo"
        """

      testCaseAsync "doesn't remove necessary namespace"
      <| CodeFix.checkNotApplicable
        server
        """
        let _ = $0System.String.IsNullOrWhiteSpace "foo"
        """
        Diagnostics.acceptAll
        selectCodeFix ])

let private removeUnnecessaryReturnOrYieldTests state =
  serverTestList (nameof RemoveUnnecessaryReturnOrYield) state defaultConfigDto None (fun server ->
    [ testCaseAsync "can remove return"
      <| CodeFix.check
        server
        """
        let f x =
          $0return x
        """
        (Diagnostics.expectCode "748")
        (CodeFix.withTitle (RemoveUnnecessaryReturnOrYield.title "return"))
        """
        let f x =
          x
        """
      testCaseAsync "can remove return!"
      <| CodeFix.check
        server
        """
        let f x =
          $0return! x
        """
        (Diagnostics.expectCode "748")
        (CodeFix.withTitle (RemoveUnnecessaryReturnOrYield.title "return!"))
        """
        let f x =
          x
        """
      testCaseAsync "can remove yield"
      <| CodeFix.check
        server
        """
        let f x =
          $0yield x
        """
        (Diagnostics.expectCode "747")
        (CodeFix.withTitle (RemoveUnnecessaryReturnOrYield.title "yield"))
        """
        let f x =
          x
        """
      testCaseAsync "can remove yield!"
      <| CodeFix.check
        server
        """
        let f x =
          $0yield! x
        """
        (Diagnostics.expectCode "747")
        (CodeFix.withTitle (RemoveUnnecessaryReturnOrYield.title "yield!"))
        """
        let f x =
          x
        """
      testCaseAsync "doesn't trigger in seq"
      <| CodeFix.checkNotApplicable
        server
        """
        let f x = seq {
          $0yield x
        }
        """
        (Diagnostics.acceptAll)
        (CodeFix.withTitle (RemoveUnnecessaryReturnOrYield.title "yield")) ])

let private removeUnusedBindingTests state =
  let config =
    { defaultConfigDto with
        FSIExtraSharedParameters = Some [| "--warnon:1182" |] }

  serverTestList (nameof RemoveUnusedBinding) state config None (fun server ->
    [ let selectRemoveUnusedBinding = CodeFix.withTitle RemoveUnusedBinding.titleBinding

      let selectRemoveUnusedParameter =
        CodeFix.withTitle RemoveUnusedBinding.titleParameter

      let validateDiags = Diagnostics.expectCode "1182"

      testCaseAsync "can remove unused single character function parameter"
      <| CodeFix.check
        server
        """
        let incr $0i x = 2
        """
        validateDiags
        selectRemoveUnusedParameter
        """
        let incr x = 2
        """

      testCaseAsync "can remove unused single character function parameter in parens"
      <| CodeFix.check
        server
        """
        let incr ($0i) x = 2
        """
        validateDiags
        selectRemoveUnusedParameter
        """
        let incr x = 2
        """

      testCaseAsync "can remove unused binding inside top level"
      <|
      //ENHANCEMENT: remove empty line
      CodeFix.check
        server
        """
        let container () =
          let $0incr x = 2 // dummy comment to keep spacing
          ()"""
        validateDiags
        selectRemoveUnusedBinding
        """
        let container () =
           // dummy comment to keep spacing
          ()""" ])

let private removeUnusedOpensTests state =
  let config =
    { defaultConfigDto with
        UnusedOpensAnalyzer = Some true }

  serverTestList (nameof RemoveUnusedOpens) state config None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle RemoveUnusedOpens.title

      testCaseAsync "can remove single unused open"
      <| CodeFix.check
        server
        """
        open $0System
        """
        Diagnostics.acceptAll
        selectCodeFix
        ""

      testCaseAsync "removes just current unused open"
      <|
      // unlike VS, `RemoveUnusedOpens` removes just current open (with cursor) and not all unused opens
      CodeFix.check
        server
        """
        open $0System
        open System.Text
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        open System.Text
        """

      testCaseAsync "removes just current unused open 2"
      <| CodeFix.check
        server
        """
        open System
        open $0System.Text
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        open System
        """

      testCaseAsync "doesn't remove used open"
      <| CodeFix.checkNotApplicable
        server
        """
        open $0System

        let _ = String.IsNullOrWhiteSpace ""
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "can remove open in nested module"
      <| CodeFix.check
        server
        """
        module A =
          module B =
            open $0System
            ()
          ()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        module A =
          module B =
            ()
          ()
        """

      testCaseAsync "can remove used open in nested module when outer scope opens same open"
      <| CodeFix.check
        server
        """
        open System
        module A =
          module B =
            open $0System
            let x = String.IsNullOrWhiteSpace ""
            ()
          ()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        open System
        module A =
          module B =
            let x = String.IsNullOrWhiteSpace ""
            ()
          ()
        """
      //ENHANCEMENT: detect open in outer scope as unused too
      itestCaseAsync "can remove used open in outer scope when usage in nested scope has own open"
      <| CodeFix.check
        server
        """
        open $0System
        module A =
          module B =
            open System
            let x = String.IsNullOrWhiteSpace ""
            ()
          ()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        module A =
          module B =
            open System
            let x = String.IsNullOrWhiteSpace ""
            ()
          ()
        """

      testCaseAsync "doesn't trigger for used open"
      <| CodeFix.checkNotApplicable
        server
        """
        open $0System
        let x = String.IsNullOrWhiteSpace ""
        """
        Diagnostics.acceptAll
        selectCodeFix ])

let private renameUnusedValue state =
  let config =
    { defaultConfigDto with
        UnusedDeclarationsAnalyzer = Some true }

  serverTestList (nameof RenameUnusedValue) state config None (fun server ->
    [ let selectReplace =
        CodeFix.ofKind "refactor" >> CodeFix.withTitle RenameUnusedValue.titleReplace

      let selectPrefix =
        CodeFix.ofKind "refactor" >> CodeFix.withTitle RenameUnusedValue.titlePrefix

      testCaseAsync "can replace unused self-reference"
      <| CodeFix.check
        server
        """
        type MyClass() =
          member $0this.DoAThing() = ()
        """
        (Diagnostics.acceptAll)
        selectReplace
        """
        type MyClass() =
          member _.DoAThing() = ()
        """

      testCaseAsync "can replace unused binding"
      <| CodeFix.check
        server
        """
        let $0six = 6
        """
        (Diagnostics.acceptAll)
        selectReplace
        """
        let _ = 6
        """

      testCaseAsync "can prefix unused binding"
      <| CodeFix.check
        server
        """
        let $0six = 6
        """
        (Diagnostics.acceptAll)
        selectPrefix
        """
        let _six = 6
        """

      testCaseAsync "can replace unused parameter"
      <| CodeFix.check
        server
        """
        let add one two $0three = one + two
        """
        (Diagnostics.acceptAll)
        selectReplace
        """
        let add one two _ = one + two
        """

      testCaseAsync "can prefix unused parameter"
      <| CodeFix.check
        server
        """
        let add one two $0three = one + two
        """
        (Diagnostics.acceptAll)
        selectPrefix
        """
        let add one two _three = one + two
        """

      testCaseAsync "doesn't replace function with _"
      <| CodeFix.checkNotApplicable
        server
        """
        let $0f _ = ()
        """
        (Diagnostics.acceptAll)
        selectReplace

      testCaseAsync "doesn't prefix function with _"
      <| CodeFix.checkNotApplicable
        server
        """
        let $0f _ = ()
        """
        (Diagnostics.acceptAll)
        selectPrefix

      testCaseAsync "replacing private variable with _ replaces private too"
      <| CodeFix.check
        server
        """
        let private $0value = 42
        """
        (Diagnostics.acceptAll)
        selectReplace
        """
        let _ = 42
        """

      testCaseAsync "private variable can be replaces with _"
      <| CodeFix.checkApplicable
        server
        """
        let private $0value = 42
        """
        (Diagnostics.acceptAll)
        selectReplace

      testCaseAsync "prefixing private variable with _ keeps private"
      <| CodeFix.check
        server
        """
        let private $0value = 42
        """
        (Diagnostics.acceptAll)
        selectPrefix
        """
        let private _value = 42
        """

      testCaseAsync "can replace backticks with _"
      <| CodeFix.check
        server
        """
        let $0``hello world`` = 42
        """
        (Diagnostics.acceptAll)
        selectReplace
        """
        let _ = 42
        """

      testCaseAsync "cannot prefix backticks with _"
      <| CodeFix.checkNotApplicable
        server
        """
        let $0``hello world`` = 42
        """
        (Diagnostics.acceptAll)
        selectPrefix

      testCaseAsync "replace doesn't trigger for function"
      <| CodeFix.checkNotApplicable
        server
        """
        let $0f _ = ()
        """
        (Diagnostics.acceptAll)
        selectReplace

      testCaseAsync "prefix doesn't trigger for function"
      <| CodeFix.checkNotApplicable
        server
        """
        let $0f _ = ()
        """
        (Diagnostics.acceptAll)
        selectPrefix

      testCaseAsync "replace doesn't trigger for member"
      <| CodeFix.checkNotApplicable
        server
        """
        type T() =
          member _.$0F () = ()
        """
        (Diagnostics.acceptAll)
        selectReplace

      testCaseAsync "prefix doesn't trigger for member"
      <| CodeFix.checkNotApplicable
        server
        """
        type T() =
          member _.$0F () = ()
        """
        (Diagnostics.acceptAll)
        selectPrefix

      testCaseAsync "prefix doesn't trigger for _"
      <| CodeFix.checkNotApplicable
        server
        """
        let $0_ = 6
        """
        (Diagnostics.acceptAll)
        selectPrefix

      testCaseAsync "replace doesn't trigger for _"
      <| CodeFix.checkNotApplicable
        server
        """
        let $0_ = 6
        """
        (Diagnostics.acceptAll)
        selectReplace ])

let private replaceWithSuggestionTests state =
  serverTestList (nameof ReplaceWithSuggestion) state defaultConfigDto None (fun server ->
    [ let selectCodeFix replacement = CodeFix.withTitle (ReplaceWithSuggestion.title replacement)

      let validateDiags (diags: Diagnostic[]) =
        Diagnostics.expectCode "39" diags
        let messages = diags |> Array.map (fun d -> d.Message) |> String.concat "\n"

        Expect.exists
          diags
          (fun (d: Diagnostic) -> d.Message.Contains "Maybe you want one of the following:")
          $"Diagnostic with code 39 should suggest name: Contained {messages}"

      testCaseAsync "can change Min to min"
      <| CodeFix.check
        server
        """
        let x = $0Min(2.0, 1.0)
        """
        validateDiags
        (selectCodeFix "min")
        """
        let x = min(2.0, 1.0)
        """

      testList
        "can get multiple suggestions for flout"
        [ testCaseAsync "can change flout to float"
          <| CodeFix.check
            server
            """
          let x = $0flout 2
          """
            validateDiags
            (selectCodeFix "float")
            """
          let x = float 2
          """
          testCaseAsync "can change flout to float32"
          <| CodeFix.check
            server
            """
          let x = $0flout 2
          """
            validateDiags
            (selectCodeFix "float32")
            """
          let x = float32 2
          """ ]

      testCaseAsync "can change flout to float in var type"
      <| CodeFix.check
        server
        """
        let x: $0flout = 2.0
        """
        validateDiags
        (selectCodeFix "float")
        """
        let x: float = 2.0
        """

      // FCS sometimes doesn't give the correct message so test is flakey
      ptestCaseAsync "can change namespace in open"
      <| CodeFix.check
        server
        """
        open System.Text.$0RegularEcpressions
        """
        validateDiags
        (selectCodeFix "RegularExpressions")
        """
        open System.Text.RegularExpressions
        """

      testCaseAsync "can change type in type constructor"
      <| CodeFix.check
        server
        """
        open System.Text.RegularExpressions
        let x = $0Regec()
        """
        validateDiags
        (selectCodeFix "Regex")
        """
        open System.Text.RegularExpressions
        let x = Regex()
        """

      testCaseAsync "can replace identifier in double-backticks"
      <| CodeFix.check
        server
        """
        let ``hello world`` = 2
        let x = ``$0hello word``
        """
        validateDiags
        (selectCodeFix "``hello world``")
        """
        let ``hello world`` = 2
        let x = ``hello world``
        """

      testCaseAsync "can add double-backticks"
      <| CodeFix.check
        server
        """
        let ``hello world`` = 2
        let x = $0helloword
        """
        validateDiags
        (selectCodeFix "``hello world``")
        """
        let ``hello world`` = 2
        let x = ``hello world``
        """ ])

let private resolveNamespaceTests state =
  let config =
    { defaultConfigDto with
        ResolveNamespaces = Some true }

  serverTestList (nameof ResolveNamespace) state config None (fun server ->
    [ let selectCodeFix =
        CodeFix.matching (fun ca -> ca.Title.StartsWith("open", StringComparison.Ordinal))

      testCaseAsync "doesn't fail when target not in last line"
      <| CodeFix.checkApplicable
        server
        """
        let x = $0Min(2.0, 1.0)
        """ // Note: new line at end!
        (Diagnostics.log >> Diagnostics.acceptAll)
        (CodeFix.log >> selectCodeFix >> Array.take 1)

      testCaseAsync "doesn't fail when target in last line"
      <| CodeFix.checkApplicable
        server
        "let x = $0Min(2.0, 1.0)" // Note: No new line at end!
        (Diagnostics.log >> Diagnostics.acceptAll)
        (CodeFix.log >> selectCodeFix >> Array.take 1)

      testCaseAsync "place open in module correctly when having additional modules"
      <| CodeFix.check
        server
        """
module Foo =
  open Microsoft

  let foo = Date$0Time.Now
        """
        (Diagnostics.log >> Diagnostics.acceptAll)
        selectCodeFix
        """
module Foo =
  open Microsoft
  open System

  let foo = DateTime.Now
        """


      testCaseAsync "place open in module correctly without any modules"
      <| CodeFix.check
        server
        """
module Foo =
  let foo = $0DateTime.Now
        """
        (Diagnostics.log >> Diagnostics.acceptAll)
        selectCodeFix
        """
module Foo =
  open System
  let foo = DateTime.Now
        """



      testCaseAsync "With attribute"
      <| CodeFix.check
        server
        """
[<AutoOpen>]
module Foo =

  let foo = $0DateTime.Now
          """
        (Diagnostics.log >> Diagnostics.acceptAll)
        selectCodeFix
        """
[<AutoOpen>]
module Foo =
  open System

  let foo = DateTime.Now
          """
      //TODO: Implement & unify with `Completion.AutoOpen` (`CompletionTests.fs`)
      // Issues:
      // * Complex because of nesting modules (-> where to open)
      // * Different open locations of CodeFix and AutoOpen
      ])

let private useMutationWhenValueIsMutableTests state =
  serverTestList (nameof UseMutationWhenValueIsMutable) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle UseMutationWhenValueIsMutable.title

      testCaseAsync "can replace = with <- when cursor on ="
      <| CodeFix.check
        server
        """
        let _ =
          let mutable v = 42
          v $0= 5
          v
        """
        (Diagnostics.expectCode "20")
        selectCodeFix
        """
        let _ =
          let mutable v = 42
          v <- 5
          v
        """

      testCaseAsync "can replace = with <- when cursor on variable"
      <| CodeFix.check
        server
        """
        let _ =
          let mutable v = 42
          $0v = 5
          v
        """
        (Diagnostics.expectCode "20")
        selectCodeFix
        """
        let _ =
          let mutable v = 42
          v <- 5
          v
        """

      testCaseAsync "doesn't suggest fix when = is comparison"
      <| CodeFix.checkNotApplicable
        server
        """
        let _ =
          let mutable v = 42
          v $0= 5
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "doesn't suggest fix when variable is not mutable"
      <| CodeFix.checkNotApplicable
        server
        """
        let _ =
          let v = 42
          v $0= 5
          v
        """
        Diagnostics.acceptAll
        selectCodeFix ])

let private useTripleQuotedInterpolationTests state =
  // blocked by FCS 43.7.200 - AST changes
  pserverTestList (nameof UseTripleQuotedInterpolation) state defaultConfigDto None (fun server ->
    [ testCaseAsync "converts erroring single-quoted interpolation to triple-quoted"
      <| CodeFix.check
        server
        """
        let a = $":^) {if true then $0"y" else "n"} d"
        """
        (Diagnostics.expectCode "3373")
        (CodeFix.ofKind "quickfix"
         >> CodeFix.withTitle UseTripleQuotedInterpolation.title)
        // cannot use triple quotes string here: ends with `"""` -> cannot use in string
        @"
        let a = $"""""":^) {if true then ""y"" else ""n""} d""""""
        " ])

let private wrapExpressionInParenthesesTests state =
  serverTestList (nameof WrapExpressionInParentheses) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle WrapExpressionInParentheses.title

      testCaseAsync "can add parenthesize expression"
      <| CodeFix.check
        server
        """
        printfn "%b" System.String.$0IsNullOrWhiteSpace("foo")
        """
        (Diagnostics.expectCode "597")
        selectCodeFix
        """
        printfn "%b" (System.String.IsNullOrWhiteSpace("foo"))
        """

      testCaseAsync "doesn't trigger for expression in parens"
      <| CodeFix.checkNotApplicable
        server
        """
        printfn "%b" (System.String.$0IsNullOrWhiteSpace("foo"))
        """
        Diagnostics.acceptAll
        selectCodeFix ])

let private removeRedundantAttributeSuffixTests state =
  serverTestList (nameof RemoveRedundantAttributeSuffix) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle RemoveRedundantAttributeSuffix.title

      testCaseAsync "redundant attribute suffix in let binding"
      <| CodeFix.check
        server
        """
        open System

        [<ObsoleteAttribute$0("Meh")>]
        let f x y = x + y
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        open System

        [<Obsolete("Meh")>]
        let f x y = x + y
        """

      testCaseAsync "redundant attribute suffix on type definition"
      <| CodeFix.check
        server
        """
        open System

        type FooAttribute() = inherit Attribute()

        [<FooAttribute$0>]
        type A =
            class end
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        open System

        type FooAttribute() = inherit Attribute()

        [<Foo>]
        type A =
            class end
        """

      testCaseAsync "redundant attribute suffix with target and constructor"
      <| CodeFix.check
        server
        """
        open System

        type FooAttribute() = inherit Attribute()

        [<type: FooAttribute$0()>]
        type A =
            class end
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        open System

        type FooAttribute() = inherit Attribute()

        [<type: Foo()>]
        type A =
            class end
        """

      testCaseAsync "redundant attribute suffix in multiple attributes"
      <| CodeFix.check
        server
        """
        open System

        type FooAttribute() = inherit Attribute()
        type BarAttribute() = inherit Attribute()

        [<FooAttribute$0; Bar>]
        type A =
            class end
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        open System

        type FooAttribute() = inherit Attribute()
        type BarAttribute() = inherit Attribute()

        [<Foo; Bar>]
        type A =
            class end
        """ ])

let private removePatternArgumentTests state =
  serverTestList (nameof RemovePatternArgument) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle RemovePatternArgument.title

      testCaseAsync "Literal pattern qualified single parameter"
      <| CodeFix.check
        server
        """
        type E =
            | A = 1
            | B = 2

        let (E.A x$0) = E.A
        """
        (Diagnostics.expectCode "3191")
        selectCodeFix
        """
        type E =
            | A = 1
            | B = 2

        let (E.A) = E.A
        """

      testCaseAsync "Local literal pattern qualified match parens parameter"
      <| CodeFix.check
        server
        """
        type E =
            | A = 1
            | B = 2

        match E.A with
        | (E.A x$0) -> ()
        """
        (Diagnostics.expectCode "3191")
        selectCodeFix
        """
        type E =
            | A = 1
            | B = 2

        match E.A with
        | (E.A) -> ()
        """

      testCaseAsync "Local literal pattern qualified single parameter"
      <| CodeFix.check
        server
        """
        type E =
          | A = 1
          | B = 2

        do
          let (E.A x$0) = E.A
          ()
        """
        (Diagnostics.expectCode "3191")
        selectCodeFix
        """
        type E =
          | A = 1
          | B = 2

        do
          let (E.A) = E.A
          ()
        """

      testCaseAsync "Local literal constant pattern qualified parameter"
      <| CodeFix.check
        server
        """
        let [<Literal>] A = 1

        match 1 with
        | A x$0 -> ()
        """
        (Diagnostics.expectCode "3191")
        selectCodeFix
        """
        let [<Literal>] A = 1

        match 1 with
        | A -> ()
        """

      testCaseAsync "Local literal constant pattern qualified parens parameter"
      <| CodeFix.check
        server
        """
        let [<Literal>] A = 1

        match 1 with
        | (A x$0) -> ()
        """
        (Diagnostics.expectCode "3191")
        selectCodeFix
        """
        let [<Literal>] A = 1

        match 1 with
        | (A) -> ()
        """

      testCaseAsync "Local match qualified single parameter"
      <| CodeFix.check
        server
        """
        match None with
        | Option.None x$0 -> ()
        """
        (Diagnostics.expectCode "725")
        selectCodeFix
        """
        match None with
        | Option.None -> ()
        """

      testCaseAsync "Local match qualified single parens parameter"
      <| CodeFix.check
        server
        """
        match None with
        | (Option.None x$0) -> ()
        """
        (Diagnostics.expectCode "725")
        selectCodeFix
        """
        match None with
        | (Option.None) -> ()
        """

      testCaseAsync "Qualified single parameter"
      <| CodeFix.check
        server
        """
        let (Option.None x$0) = None
        """
        (Diagnostics.expectCode "725")
        selectCodeFix
        """
        let (Option.None) = None
        """

      testCaseAsync "Local single parameter"
      <| CodeFix.check
        server
        """
        do
          let (Option.None x$0) = None
          ()
        """
        (Diagnostics.expectCode "725")
        selectCodeFix
        """
        do
          let (Option.None) = None
          ()
        """

      testCaseAsync "Local two match parameters"
      <| CodeFix.check
        server
        """
        match None with
        | None x$0 y -> ()
        """
        (Diagnostics.expectCode "725")
        selectCodeFix
        """
        match None with
        | None -> ()
        """

      testCaseAsync "Local two match parens parameters"
      <| CodeFix.check
        server
        """
        match None with
        | None (x$0 y) -> ()
        """
        (Diagnostics.expectCode "725")
        selectCodeFix
        """
        match None with
        | None -> ()
        """

      testCaseAsync "Local two parameter"
      <| CodeFix.check
        server
        """
        do
          let (Option.None x$0 y) = None
          ()
        """
        (Diagnostics.expectCode "725")
        selectCodeFix
        """
        do
          let (Option.None) = None
          ()
        """

      testCaseAsync "Single parameter"
      <| CodeFix.check
        server
        """
        let (None x$0) = None
        """
        (Diagnostics.expectCode "725")
        selectCodeFix
        """
        let (None) = None
        """

      testCaseAsync "Two parameters"
      <| CodeFix.check
        server
        """
        let (None x$0 y) = None
        """
        (Diagnostics.expectCode "725")
        selectCodeFix
        """
        let (None) = None
        """ ])

let private removeUnnecessaryParenthesesTests state =
  let config =
    { defaultConfigDto with
        UnnecessaryParenthesesAnalyzer = Some true }

  serverTestList (nameof RemoveUnnecessaryParentheses) state config None (fun server ->
    [ let selector =
        CodeFix.ofKind "quickfix"
        >> CodeFix.withTitle RemoveUnnecessaryParentheses.title

      testCaseAsync "Can remove unnecessary parentheses"
      <| CodeFix.check
        server
        """
        let x = $0(3)
        """
        (Diagnostics.expectCode "FSAC0004")
        selector
        """
        let x = 3
        """

      testCaseAsync "Adds space after"
      <| CodeFix.check
        server
        """
        $0(int)3.14
        """
        (Diagnostics.expectCode "FSAC0004")
        selector
        """
        int 3.14
        """

      testCaseAsync "Adds space before"
      <| CodeFix.check
        server
        """
        int$0(3.14)
        """
        (Diagnostics.expectCode "FSAC0004")
        selector
        """
        int 3.14
        """

      testCaseAsync "Adds space before and after"
      <| CodeFix.check
        server
        """
        [|$0(<@ 1 @>)|]
        """
        (Diagnostics.expectCode "FSAC0004")
        selector
        """
        [| <@ 1 @> |]
        """

      testCaseAsync "Handles multiline expr well"
      <| CodeFix.check
        server
        """
        let _ =
          let x = 3
          (
              let y = 99
              y - x
          )$0
        """
        (Diagnostics.expectCode "FSAC0004")
        selector
        """
        let _ =
          let x = 3
          let y = 99
          y - x
        """

      testCaseAsync "Handles sensitive multiline expr well"
      <| CodeFix.check
        server
        """
        let longVarName1 = 1
        let longVarName2 = 2
        (
          longFunctionName
            longVarName1
            longVarName2
        )$0
        """
        (Diagnostics.expectCode "FSAC0004")
        selector
        """
        let longVarName1 = 1
        let longVarName2 = 2
        longFunctionName
          longVarName1
          longVarName2
        """

      testCaseAsync "Handles outlaw match exprs"
      <| CodeFix.check
        server
        """
        3 > (match x with
            | 1
            | _ -> 3)$0
        """
        (Diagnostics.expectCode "FSAC0004")
        selector
        """
        3 > match x with
            | 1
            | _ -> 3
        """

      testCaseAsync "Handles even more outlaw match exprs"
      <| CodeFix.check
        server
        """
        3 > ( match x with
            | 1
            | _ -> 3)$0
        """
        (Diagnostics.expectCode "FSAC0004")
        selector
        """
        3 > match x with
            | 1
            | _ -> 3
        """

      testCaseAsync "Handles single-line comments"
      <| CodeFix.check
        server
        """
        3 > (match x with
             // Lol.
            | 1
            | _ -> 3)$0
        """
        (Diagnostics.expectCode "FSAC0004")
        selector
        """
        3 > match x with
             // Lol.
            | 1
            | _ -> 3
        """

      testCaseAsync "Keep parens when removal would cause reparse of infix as prefix"
      <| CodeFix.checkNotApplicable
        server
        """
        ""+(Unchecked.defaultof<string>)$0+""
        """
        (Diagnostics.expectCode "FSAC0004")
        selector

      ])

let tests textFactory state =
  testSequenced
  <| testList
    "CodeFix-tests"
    [ HelpersTests.tests textFactory
      AddExplicitTypeAnnotationTests.tests state
      AdjustConstantTests.tests state
      ToInterpolatedStringTests.tests state
      ToInterpolatedStringTests.unavailableTests state
      addMissingEqualsToTypeDefinitionTests state
      addMissingFunKeywordTests state
      addMissingInstanceMemberTests state
      addMissingRecKeywordTests state
      addMissingXmlDocumentationTests state
      addNewKeywordToDisposableConstructorInvocationTests state
      addTypeToIndeterminateValueTests state
      changeDerefBangToValueTests state
      changeDowncastToUpcastTests state
      changeEqualsInFieldTypeToColonTests state
      changePrefixNegationToInfixSubtractionTests state
      changeRefCellDerefToNotTests state
      changeTypeOfNameToNameOfTests state
      convertBangEqualsToInequalityTests state
      convertCSharpLambdaToFSharpLambdaTests state
      convertDoubleEqualsToSingleEqualsTests state
      convertInvalidRecordToAnonRecordTests state
      convertPositionalDUToNamedTests state
      convertTripleSlashCommentToXmlTaggedDocTests state
      addPrivateAccessModifierTests state
      GenerateAbstractClassStubTests.tests state
      generateRecordStubTests state
      generateUnionCasesTests state
      generateXmlDocumentationTests state
      ImplementInterfaceTests.tests state
      makeDeclarationMutableTests state
      makeOuterBindingRecursiveTests state
      removeRedundantQualifierTests state
      removeUnnecessaryReturnOrYieldTests state
      removeUnusedBindingTests state
      removeUnusedOpensTests state
      RenameParamToMatchSignatureTests.tests state
      renameUnusedValue state
      replaceWithSuggestionTests state
      resolveNamespaceTests state
      useMutationWhenValueIsMutableTests state
      useTripleQuotedInterpolationTests state
      wrapExpressionInParenthesesTests state
      removeRedundantAttributeSuffixTests state
      removePatternArgumentTests state
      UpdateValueInSignatureFileTests.tests state
      removeUnnecessaryParenthesesTests state
      AddTypeAliasToSignatureFileTests.tests state
      UpdateTypeAbbreviationInSignatureFileTests.tests state
      AddBindingToSignatureFileTests.tests state
      ReplaceLambdaWithDotLambdaTests.tests state
      IgnoreExpressionTests.tests state
      ExprTypeMismatchTests.tests state ]
