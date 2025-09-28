alllines <- paste(readLines("varnames.txt"), collapse = "\n")
alllines
splitlines <- strsplit(alllines, split = "visible", fixed = TRUE)

gsub(
  pattern = ".*'(.+)'.*",
  replacement = "\\1",
  x = splitlines[[1]][2],
  perl = TRUE
)
