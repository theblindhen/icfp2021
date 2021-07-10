module Program =
    let [<EntryPoint>] main _ =
        let test = UnitTests.GeometryDecomposeTestClass ()
        test.TestSegmentDecomposeOverlap2 ()
        0

