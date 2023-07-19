/*
 * test.cpp
 *
 *  Created on: 07.03.2015
 *      Author: sabanesd
 */

#include <rcppExport.h>

using namespace Rcpp;

// dose is a matrix, with each row corresponding to a
// different combination. (combis x drugs)
// alpha0 and alpha1 are matrices (samples x drugs)
// and eta is a vector (length samples)
// refDose is vector of reference doses.
//
// function shall return matrix of sample probabilities
// for each of the drug combinations (samples x combis).
NumericVector probComboLogistic( NumericMatrix dose,
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

//    ## calculate standardized dose
//    standDose <- dose / modelspecs$refDose
//    singleOdds <- matrix(nrow=nrow(alpha0),
//                         ncol=ncol(alpha0))
//
//    ## calculate single agent odds
//                           for(j in 1:nDrugs)
//                           {
//                               thisLinpred <- alpha0[, j] + alpha1[, j] *
//                                   log(standDose[j])
//
//                               ## step function here to avoid numeric problems:
//                               ## if linpred < -20, it will be set to -20,
//                               ## if linpred > 20, it will be set to 20, else it
//                               ## stays.
//                               thisLin <- ifelse(thisLinpred < -20, -20,
//                                                 ifelse(thisLinpred > 20,
//                                                        20,
//                                                        thisLinpred))
//
//                               singleOdds[, j] <-  exp(thisLin)
//                           }
//
//                           ## note: these appear to be generic to more than 2 drugs,
//                           ## but are not so easily generalized!!
//                           ## todo: look carefully at the formulas for more than 2 drugs
//                           ## later...
//                           odds0 <- rowSums(singleOdds) + apply(singleOdds, 1L, prod)
//                           odds <- odds0 * exp(eta * prod(standDose))
//
//                           ## so the final prob vector is:
//                           ret <- odds / (1 + odds)
//
//                           return(ret)
