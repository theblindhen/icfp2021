# Tools installation

## F# environment

Install Visual Studio Code and [.NET Core
SDK](https://dotnet.microsoft.com/download) on your system.
- Arch Linux: `pacman -S code dotnet-sdk`.

At this point you can build our source code:

```
cd Playground
dotnet run
```

Launch VS Code, open the Extensions pane (Ctrl+Shift+X), and install the
[Ionide-fsharp
extension](https://marketplace.visualstudio.com/items?itemName=Ionide.Ionide-fsharp) from the marketplace.
- Arch Linux: as of 2021-07-05, to get the debugger working, build and install
  [the Open VSX C#
  extension](https://github.com/muhammadsammy/free-omnisharp-vscode) from
  `master`.
