# FsLexYacc

The lex specification files are calles `.fsl`.
The  parser specification files are called `.fsy`.

## Setting up project build

1. Add FsLexYacc as a dependency to the project
2. To the `fsproj` file, add the following under `PropertyGroup`:

    ```xml
    <FsLexToolExe>fslex.dll</FsLexToolExe>
    <FsYaccToolExe>fsyacc.dll</FsYaccToolExe>
    ```

3. To the `fsproj` file, add the following under `ItemGroup`, after the module that declares the type of the language to be parsed, but before all dependant modules:

    ```xml
    <FsYacc Include="WhileParser.fsy">
      <OtherFlags>--module WhileParser</OtherFlags>
    </FsYacc>
    <FsLex Include="WhileLexer.fsl">
      <OtherFlags>--module WhileLexer --unicode</OtherFlags>
    </FsLex>
    <Compile Include="WhileParser.fsi" />
    <Compile Include="WhileParser.fs" />
    <Compile Include="WhileLexer.fs" />
    ```

This should be sufficient to run Lex and Yacc on the `.fsl` and `.fsy` files to generate a lexer and parser module respectively (the last two `.fs` files mentioned above).

For debugging, it can be useful to run `lex` or `yacc` from the terminal. The command can be found in VSCode's build terminal output, and it looks something like so:

```
> dotnet "/Users/johanrosenkilde/.nuget/packages/fslexyacc/10.2.0/build/fsyacc/netcoreapp3.1/fsyacc.dll"  -o "WhileParser.fs" --module Parser WhileParser.fsy
```

WARNING: due to VSCode's incremental build, you may need to remove the generated `.fsi` and `.fs` files sometimes during debugging.

4. To invoke the parser on a string `s`, run the following lines:

    ```fsharp
    let lexbuf = LexBuffer<char>.FromString source
    WhileParser.start WhileLexer.token lexbuf
    ```

    Parse errors result in thrown exceptions which may carry position information, but I did not get to examine that.