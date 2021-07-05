# Tools installation

## F# environment

Install Visual Studio Code and [.NET Core
SDK](https://dotnet.microsoft.com/download) on your system.
- Arch Linux: `sudo pacman -S dotnet-sdk`.

At this point you can build our source code:

```
cd Playground
dotnet run
```

## Visual Studio Code

Install Visual Studio Code, version 1.57 or higher.
- Arch Linux: `yay -S visual-studio-code-bin gnome-keyring`. Don't use the
  `code` package: it's outdated at the time of writing this, and the Open VSX
  version of the C# extension is out of date and needs to be rebuilt from
  `master`.

Launch VS Code, open the Extensions pane (Ctrl+Shift+X), and install the
[Ionide-fsharp
extension](https://marketplace.visualstudio.com/items?itemName=Ionide.Ionide-fsharp)
from the marketplace.

Now you may also install the GitHub Copilot extension. If you installed Copilot,
now restart VSCode and sign in. Activate Copilot by clicking its red icon in the
lower right corner of the VSCode window.

Use File -> Open Folder and open the folder containing this readme.
