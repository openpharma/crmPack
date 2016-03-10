#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[crmPack-package.R] by DSB Son 08/03/2015 11:52>
##
## Description:
## Package description.
##
## History:
## 29/01/2014   file creation
#####################################################################################

##' Object-oriented implementation of CRM designs
##'
##' @name crmPack-package
##' @aliases crmPack
##' @docType package
##' @title Object-oriented implementation of CRM designs
##' @author Daniel Sabanes Bove \email{sabanesd@@roche.com}
## @useDynLib crmPack
##  cpp_glmBayesMfp cpp_bfgs cpp_optimize cpp_sampleGlm cpp_evalZdensity
##  cpp_coxfit
##' @importFrom graphics plot hist
##' @importFrom methods setClass setOldClass setGeneric setMethod representation
##' signature prototype initialize new is
##' @keywords package
{}

##' @keywords internal
.onAttach <- function(libname, pkgname)
{
    packageStartupMessage(
        ifelse(getOption("crmPackUsesCpp"),
               "crmPack will use few C++ functions for faster computations\n",
               "crmPack cannot use C++ functions, some computations might be slow\n"),
        "Type crmPackHelp() to open help browser\n",
        "Type crmPackExample() to open example\n",
        "Type crmPackUpgrade() to upgrade crmPack to latest version\n",
        "Please visit https://roche.jiveon.com/projects/crmpack for more\n")
}

##' @keywords internal
.onLoad <- function(libname, pkgname)
{
    ## by default, we are not using C++.
    options(crmPackUsesCpp = FALSE)

    ## try loading Rcpp package
    RcppLoaded <- suppressWarnings(require("Rcpp", quietly=TRUE))

    ## if it is loaded,
    if(RcppLoaded)
    {
        ## try to compile dummy function first and then required stuff
        compileResult <-
            try({ ## start try

                ## first compile dummy function to fail quickly
                cppFunction( "int test(int i){ return i ; }" ,
                            showOutput=FALSE, verbose=FALSE)

                ## then use real functions: assign them in
                ## global environment .GlobalEnv
                ## (not sure what the package environment would be)

                ## [1] helper for prob function for class ComboLogistic:
                ## crmPackCppProbComboLogistic
                ## // dose is a matrix, with each row corresponding to a
#               // different combination (combis x drugs)
#               //
#                 // alpha0 and alpha1 are matrices (samples x drugs)
#               //
#                 // and eta is a vector (length samples)
#               //
#                 // refDose is vector of reference doses.
#               //
#                 // function shall return matrix of sample probabilities
#               // for each of the drug combinations (samples x combis).
#               //
                cppFunction("
NumericVector crmPackCppProbComboLogistic( NumericMatrix dose,
                                 NumericMatrix alpha0,
                                 NumericMatrix alpha1,
                                 NumericVector eta,
				 NumericVector refDose)
{
    // first get dimensions
    int nSamples = alpha0.nrow();
    int nDrugs = alpha0.ncol();
    int nCombis = dose.nrow();

    // allocate return matrix
    NumericMatrix ret(nSamples, nCombis);

    // for each drug combination
    for(int iCombi = 0; iCombi < nCombis; ++iCombi)
    {
        // calculate standardized dose vector for this combination
        NumericVector standDose = dose(iCombi, _) / refDose;

        // this we will update below:
            NumericVector singleOddsSums(nSamples, 0.0); // vector of zeroes
        NumericVector singleOddsProds(nSamples, 1.0); // vector of ones
        double standDoseProd = 1.0;

        // now go through all drugs:
            for(int iDrug = 0; iDrug < nDrugs; ++iDrug)
            {
                // calculate singleOdds and update sums and prods
                // just initialized above

                double thisStandDose = standDose[iDrug];
                standDoseProd *= thisStandDose;
                double thisLogStandDose = ::log(thisStandDose);

                // go through all samples
                for(int iSample = 0; iSample < nSamples; ++iSample)
                {
                    double thisLinpred = alpha0(iSample, iDrug) + alpha1(iSample, iDrug) * thisLogStandDose;
                    if(thisLinpred < -20)
                    {
                        thisLinpred = -20;
                    } else if(thisLinpred > 20)
                      {
                          thisLinpred = 20;
                      }

                    double thisSingleOdds = exp(thisLinpred);
                    singleOddsSums[iSample] += thisSingleOdds;
                    singleOddsProds[iSample] *= thisSingleOdds;
                }
            }

        // now finish calculating the probabilities for this combi
        NumericVector thisRes = singleOddsSums + singleOddsProds;
        NumericVector scaledEta = eta * standDoseProd;
        thisRes = thisRes * exp(scaledEta);
        ret(_, iCombi) = thisRes / (1.0 + thisRes);
    }

    return ret;
}
", showOutput=FALSE, verbose=FALSE, env=.GlobalEnv)
                ## end crmPackCppProbComboLogistic

                ## [2] etc.

            }, ## end try
                silent=TRUE)

        ## if all compilations went successful (no errors):
        if(! inherits(compileResult, "try-error"))
        {
            ## set the option to later use C++ functions
            options(crmPackUsesCpp = TRUE)
        }
    }

    ## in the package then use this to conditionally use C++ functions:
    ## if(getOption("crmPackUsesCpp"))
    ## {
    ## ## use C++ functions
    ## }
}

## bla <- function(x)
## {
## cppFunction( "int testbla(int i){ return i ; }" ,
##             showOutput=FALSE, verbose=FALSE, env=.GlobalEnv)
## }
## bla(5)
## testbla(5)
