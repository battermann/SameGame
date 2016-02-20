namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("SameGame.Suave")>]
[<assembly: AssemblyProductAttribute("SameGame")>]
[<assembly: AssemblyDescriptionAttribute("Same Game")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
