crossScalaVersions := Seq("2.13.8", "2.12.16")

coverageEnabled := true

wartremoverErrors += Wart.NonUnitStatements
wartremoverErrors += Wart.PublicInference
