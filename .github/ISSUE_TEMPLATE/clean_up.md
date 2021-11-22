To do:

- [ ] Revise documentation
  - [ ] RStudio outline section dividers (use `# function or class name ----`)
  - [ ] Add lifecycle badge (as a default take "stable" for existing functions or classes)
  - [ ] Replace `##'` by `#'`
  - [ ] Add to `pkgdown.yml`
  - [ ] remove file history and the timestamp in comments on top of file
- [ ] Add unit tests
  - [ ] for classes: 
    - [ ] test default constructor
    - [ ] for reference classes: test methods
  - [ ] for functions: as usual
- [ ] Refactor as needed
  - [ ] Code styling (indentation etc.) - install package `styler` and use corresponding add-in
  - [ ] keep externally used names so that we don't break downstream code
  - [ ] consider renaming internal variables etc. if needed
  - [ ] if a function is too long, break up into pieces
  - [ ] for classes: move function definitions outside if possible (e.g. move validation function as `validity_subjects()` to `Data-validity.R` and have corresponding test file `test-Data-validity.R`)
  - [ ] in `setClass` function, just use the `slots` instead of outdated `representation`.
