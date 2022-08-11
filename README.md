# The Blind Hen submission

We solved problems using three approaches:

## Simulated Annealing with a selection of neighbor configuration generators

This is guided by a penalty score that describes how bad a figure is from
satisfying the rules, and on an order-of-magnitude less, its dislike score.

Our neighbor generators included:
- Randomly moving a single vertex a single move
- Moving a single vertex N moves according to a mini-simulated annealing
- Reflection / rotation / translation of the whole figure
- Local edge-and-integer-point preserving transformations, e.g. rotation
  around articulation points, sub-graph reflection over vertical lines, etc.

## A DFS that places figure vertices on hole vertices.

We filter out impossible configurations early by looking at local edge lengths
between other placed vertices.

We then run Simulated Annealing on the remaining unplaced figure vertices,
using a conservative neighbor generation that only moves single vertices.

This was built in a mad scramble during the last 6 hours of the contest.
  
## GUI tool solving by hand with computed assistance

The GUI tool allows moving vertices by hand, individually or by selecting
multiple. It also allows running the simulated annealing for N steps at a time.

The GUI tool was indispensable for understanding the behaviour of our automatic solvers,
and also made it more fun to interact with the problems.

# Tools installation

## Visual Studio Code

Install Visual Studio Code, version 1.57 or higher.
- Arch Linux: `yay -S visual-studio-code-bin gnome-keyring`. Don't use the
  `code` package as you may end up with outdated versions of the F# extensions.
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
- Ubuntu: [Follow these instructions](https://docs.microsoft.com/en-us/dotnet/core/install/linux-ubuntu), then `sudo apt-get install -y libfontconfig1` to make Skia work.

### Building and running from the command line

To build and run in debug mode:
```
dotnet run -p TheBlindHen
```

To build and then run in release mode:
```
dotnet build -c Release
dotnet exec TheBlindHen/bin/Release/net6.0/TheBlindHen.dll
```

## Visual Studio Code + F\#

Launch VS Code, open the Extensions pane (Ctrl+Shift+X), and install the
following extensions:
- C# ("C# for Visual Studio Code (powered by OmniSharp)")
- [Ionide for F#](https://marketplace.visualstudio.com/items?itemName=Ionide.Ionide-fsharp)
from the marketplace.

To test that debugging works, place a break point somewhere in `Program.fs:main`
by clicking the left-hand margin. Click `F#` button on the palette to the
extreme left of the VSCode window. Click the Play button at the top.
The project should run and execution stop at the
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
