# Tools installation

## Visual Studio Code

Install Visual Studio Code, version 1.57 or higher.
- Arch Linux: `yay -S visual-studio-code-bin gnome-keyring`. Don't use the
  `code` package: it's outdated at the time of writing this, and the Open VSX
  version of the C# extension is out of date and needs to be rebuilt from
  `master`.
- Windows: [https://code.visualstudio.com].

## Git

Download and install Git for your system.
- Windows: [https://git-scm.com/downloads]. In the install wizard:
    * Deselect Explorer integration (We'll use Git inside VSCode only).
    * Select "Use Visual Studio Code as default editor"
    * Select "Override the default branch name for new repositories", keeping
      the suggested `main` (for compatibility with GitHub).
    * Keep other defaults, as you like.

## Checking out the repository

Open Visual Studio Code. A panel should be available where you can click `Clone
Repository`. Click "Clone on GitHub": You'll be relegated to the browser where
you can authenticate.

After a restart, you just need to use `File -> Open Folder` and select
`icfp2021` (or select from Recently Used).

## F# environment

Install [.NET Core SDK](https://dotnet.microsoft.com/download) on your system.
- Arch Linux: `sudo pacman -S dotnet-sdk`.
- Mac OS X: `brew install dotnet-sdk` (or download via above link)
- Windows: Use [.NET Core SDK](https://dotnet.microsoft.com/download), select
  version 5.0 for Visual Studio 2019. 

At this point you can build our source code.

On Linux/macOS:
```
cd Playground
dotnet run
```

## Visual Studio Code + F\#

Launch VS Code, open the Extensions pane (Ctrl+Shift+X), and install the
following extensions:
- C# (called "powered by Microsoft")
- [Ionide-fsharp
extension](https://marketplace.visualstudio.com/items?itemName=Ionide.Ionide-fsharp)
from the marketplace.

To test that debugging works, place a break point somewhere in `Program.fs:main`
by clicking the left-hand margin. Click `F#` button on the palette to the
extreme left of the VSCode window. Click the `Debug` button to the right of the
`Playground` project. The project should run and execution stop at the
breakpoint. By hovering over variables, you'll see their current values, and a
small control panel will have appeared that allows you to step forward etc.

### Optional: Solutions explorer

To get more convenient Run and Debugging via the usual shortcut keys F5 and
Shift+F5, you may need to install the extension `vscode-solution-explorer`.

### Optional: GitHub Copilot

Now you may also install the GitHub Copilot extension. After you do this,
restart VSCode and sign in to GitHub. Activate Copilot by clicking its red icon
in the lower right corner of the VSCode window.

# F\# Tips

Be sure to read [docs/fsharp.md](docs/fsharp.md)!
