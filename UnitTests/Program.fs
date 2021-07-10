module Program =
    let [<EntryPoint>] main _ =
        let test = UnitTests.SegmentOutsideHoleTestClass ()
        test.TestSegmentOutsideHole7 ()
        0

