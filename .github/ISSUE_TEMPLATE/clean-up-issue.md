---
name: Clean up issue
about: General clean up issue template for `crmPack`.
title: "... clean up "
labels: ''
assignees: ''

---

Please edit appropriately below if needed.

To do:

- [ ] Revise documentation
  - [ ] RStudio outline section dividers (use `# function or class name ----`)
  - [ ] Add lifecycle badge (as a default take "stable" for existing functions or classes)
  - [ ] Replace `##'` by `#'`
  - [ ] Add to `pkgdown.yml`
  - [ ] Remove file history and the timestamp in comments on top of file
- [ ] Add unit tests
  - [ ] For classes:
    - [ ] Test default constructor
    - [ ] For reference classes: test methods
  - [ ] For functions: as usual
- [ ] Refactor as needed
  - [ ] Code styling (indentation etc.) - install package `styler` and use corresponding add-in
  - [ ] Keep externally used names so that we don't break downstream code
  - [ ] Remove `keywords` tags
  - [ ] For functions or methods: 
    - [ ] Add assertions for arguments using `checkmate` functions
    - [ ] If it is too long, break up into pieces (rule of thumb: maximum 50 lines of code)
    - [ ] Consider renaming internal variables etc. if needed
    - [ ] Avoid explicit use of `return`
  - [ ] For classes: 
    - [ ] move function definitions outside if possible (e.g. move validation function as `validity_subjects()` to `Data-validity.R` and have corresponding test file `test-Data-validity.R`)
    - [ ] In `setClass` function, just use the `slots` instead of outdated `representation`
    - [ ] Remove things like `validObject(.Data())`
