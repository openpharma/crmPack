# Test that package loads without messages for
# default patient IDs or cohort indices.
tmp <- tempfile()
sink(file(tmp, open = "wt"), type = "message")
library(crmPack)
sink(type = "message")
msg <- readLines(tmp)
checkmate::expect_disjunct("Used default patient IDs!", msg)
checkmate::expect_disjunct("Used best guess cohort indices!", msg)
