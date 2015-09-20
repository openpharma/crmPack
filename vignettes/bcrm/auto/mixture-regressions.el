(TeX-add-style-hook "mixture-regressions"
 (lambda ()
    (LaTeX-add-bibliographies
     "flexmix")
    (LaTeX-add-labels
     "fig:artificialData"
     "fig:beta"
     "fig:mehta"
     "fig:tribolium"
     "fig:trypanosome"
     "fig:fabric"
     "fig:patent"
     "fig:seizure"
     "fig:almes")
    (TeX-run-style-hooks
     "amssymb"
     "amsmath"
     "bm"
     "amsfonts"
     ""
     "latex2e"
     "jss10"
     "jss"
     "nojss")))

