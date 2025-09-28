#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: crmPack
##
## Time-stamp: <[reporters.R] by DSB Mon 03/03/2014 11:33>
##
## Description:
## Test ReporteRs package to create Powerpoint presentation
##
## History:
## 03/03/2014   file creation
#####################################################################################

library(ReporteRs)

## Here we define powerpoint document filename to write
pptx.file <- "presentation.pptx"

## Creation of doc, a pptx object (default template)
doc <- pptx()

## check my layout names:
slide.layouts(doc)


## First slide:
doc <- addSlide(doc, "Two Content")

## add into doc first 10 lines of iris
doc <- addTitle(doc, "First 10 lines of iris")
doc <- addTable(doc, iris[1:10, ])

## add text with stylename "Normal" into doc (and an empty line just before)
doc <- addParagraph(doc, value = c("", "Hello World!"), stylename = "Normal")

## Second slide:
doc <- addSlide(doc, "Title and Content")

## add a plot into doc
doc <- addPlot(
  doc,
  function() plot(rnorm(10), rnorm(10))
)

## write the doc
writeDoc(doc, pptx.file)

## --------------------------------------------------

## Adding new slide in an existing Powerpoint document

## Create a new document
doc <- pptx(title = "test", template = "test2.pptx")

# replace slide 3 of MyExistingFileThatNeedAGraphToBeUpdated.pptx
# by a new slide with layout "Title and Content" then add content
doc <- addSlide(doc, slide.layout = "Title and Content", bookmark = 2)
doc <- addTitle(doc, "my new graph")

## myplot <- qplot(Sepal.Length, Petal.Length, data = iris, color = Species, size = Petal.Width, alpha = I(0.7))
## doc <- addPlot( doc, print, x = myplot )

# Write the object in file "~/presentation.pptx"
writeDoc(doc, "test3.pptx")
