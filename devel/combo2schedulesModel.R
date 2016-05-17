

##' @param doses a list (elements "low" and "high" for the two schedules) of 
##' dose matrices, with each row corresponding to a different combination 
##' (nCombis x 2, might be different number of rows for each schedule)
##' The "high" schedule only is valid for the second drug
##' @param intercepts the intercept parameters (alpha1, beta1)
##' @param slopes the slope parameters (alpha2, beta2)
##' @param gamma the interaction parameter
##' @param rho the schedule factor parameter
##' @param refDose vector of reference doses (length 2)
##' @return list (elements "low" and "high" for the two schedules) of 
##' the DLT probabilities for each of the drug combinations 
##' (vectors with length combis, might be different length for each schedule)
##'
##' @keywords internal
##' @author Daniel Sabanes Bove \email{sabanesd@@roche.com}
probCombo2SchedulesLogistic <- function(doses,
                                        intercepts,
                                        slopes,
                                        gamma,
                                        rho,
                                        refDose)
{
    ## start return list
    ret <- list()
    
    ## standardize the doses:
    standDoses <- lapply(doses, FUN=sweep, MARGIN=2, "/", STATS=refDose)
    
    ## for each of the two schedules calculate the prob vector:
    for(schedule in names(doses))
    {
      nCombis <- nrow(doses[[schedule]])
      ret[[schedule]] <- numeric(nCombis)
    
    ## this we will update below:
    singleOdds <- matrix(nrow=nCombis, ncol=2)
    
    ## calculate single agent odds
    for(j in 1:2)
    {
      thisLinpred <- intercepts[j] + slopes[j] *
        log(standDoses[[schedule]][, j])
      
      ## only if high schedule and this is the second drug (Venetoclax)
      if(schedule == "high" && j == 2)
      {
        thisLinpred <- thisLinpred + slopes[j] * log(rho)
      }
      
      ## step function here to avoid numeric problems:
      ## if linpred < -20, it will be set to -20,
      ## if linpred > 20, it will be set to 20, else it
      ## stays.
      thisLin <- ifelse(thisLinpred < -20, -20,
                        ifelse(thisLinpred > 20,
                               20,
                               thisLinpred))
      
      singleOdds[, j] <-  exp(thisLin)
    }
    
    odds0 <- rowSums(singleOdds) + apply(singleOdds, 1L, prod)
    
    ## todo: here one can try out the simpler vs. more complicate
    ## forms
    
    ## odds <- odds0 * exp(gamma * apply(standDoses[[schedule]], 1L, prod))
    odds <- odds0 * exp(gamma)
    
    ## so the final prob vector is:
    ret[[schedule]] <- odds / (1 + odds)
    }
    
    return(ret)
}


dosegrid <- expand.grid(Idasanutlin=seq(from=10, to=100, by=10),
                        Venetoclax=seq(from=200, to=1000, by=100))
doses <- list(low=dosegrid,
              high=dosegrid)
str(doses,1)

intercepts <- c(-2, -2)
slopes <- c(0.25, 0.25)
gamma <- 1
rho <- 2
refDose <- c(50, 300)

probs <- 
probCombo2SchedulesLogistic(doses=doses,
                            intercepts=intercepts,
                            slopes=slopes,
                            gamma=gamma,
                            rho=rho,
                            refDose=refDose)

## now make plots of this:
library(ggplot2)
plotData <- plots <- list()

for(schedule in names(doses))
{
  drugNames <- names(doses[[1]])
  ## format the data
  plotData[[schedule]] <- cbind(doses[[schedule]], probs[[schedule]])
  names(plotData[[schedule]])[1:3] <- c("x1", "x2", "toxicity")

  ## make the plot
  ret <- ggplot(plotData[[schedule]][, c("x1", "x2", "toxicity")],
                aes(x1,x2,toxicity)) +
    geom_tile(aes(fill=toxicity))

  breaks <- seq(from=0, to=1, by=0.1)
  ## from RColorBrewer package:
  ## colorRampPalette(c("green", "yellow", "orange", "red", "red"))(n = 10)
  colors <- c("#00FF00", "#38FF00", "#71FF00", "#AAFF00", "#E2FF00",
              "#FFE200",  "#FFAA00", "#FF7100", "#FF3800",
              "#FF0000")
  ret <- ret +
    stat_contour(aes(x1,x2,z=toxicity),
                 colour="black", size=0.3,
                 breaks=breaks) +
    xlab(drugNames[1]) + ylab(drugNames[2]) +
    scale_fill_gradientn(colours=colors,
                         breaks=breaks,
                         labels=format(breaks),
                         limits=c(0, 1))
  
  ## save the plot
  plots[[schedule]] <- ret
}

library(gridExtra)
jointPlot <- arrangeGrob(plots[["low"]], plots[["high"]], ncol=2)
grid::grid.draw(jointPlot)
