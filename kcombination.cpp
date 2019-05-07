/**
 * k-combination 
 * This is a collection of function alloying the generation of k-combination of n object in R.
 * "More formally, a k-combination of a set S is a subset of k distinct elements of S. 
 * If the set has n elements, the number of k-combinations is equal to the binomial coefficient"
 * -http://en.wikipedia.org/wiki/Combination
 * e.g. k2combination(c(1,2,3)) will return:
 * [[1]] 
 * [1] 1 2
 * [[2]]
 * [1] 1 3
 * [[3]]
 * [1] 2 3
 *
 * Usage (in R):
 *
 * library(Rcpp)
 * sourceCpp("kcombination.cpp")
 * k2combination(c(1,2,3))
 * #Selected only the combinations containing 4 (84 elements vs 210):
 * k4combination(c(1:10),4)
 * Author: Etienne Lord
 * Since : Mai 2015
 */
#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
#include <algorithm>
#include <map>
#include <stdlib.h>     /* srand, rand */			  
 
//Return a list of combination for k=6
//In: data    : vector of the element e.g. c(1,2,3,4,5,6,7) or c(1:20)
//    selector: include only the combination including the <selector> element
//Out: a list of combination     
 
// [[Rcpp::export]]
Rcpp::List k6combination(Rcpp::NumericVector data, int selector=0) {
 	std::vector<std::vector<int> > combinations;
	std::vector<int> combination;
	combination.clear();
 	combinations.clear();
 	for (int i=0; i< data.size();i++) {
 		for (int j=i+1;j<data.size();j++) {
 			for (int k=j+1;k<data.size();k++) {
 				for (int l=k+1;l<data.size();l++) {
 					for (int m=l+1;m<data.size();m++) {
 						for (int n=m+1;n<data.size();n++) {
							if (i==selector||j==selector||k==selector||l==selector||m==selector||n==selector||selector==0) {
								combination.clear();
								combination.push_back(data[i]);
								combination.push_back(data[j]);
								combination.push_back(data[k]);
								combination.push_back(data[l]);
								combination.push_back(data[m]);
								combination.push_back(data[n]);
								combinations.push_back(combination);
							}
						}
					}			
				}
 			}  
 		}
 	} 	   
 	return Rcpp::wrap( combinations );
 }
 
//Return a list of combination for k=5
//In: data    : vector of the element e.g. c(1,2,3,4,5,6,7) or c(1:20)
//    selector: include only the combination including the <selector> element
//Out: a list of combination     
  
  // [[Rcpp::export]]
 Rcpp::List k5combination(Rcpp::NumericVector data, int selector=0) {
 	std::vector<std::vector<int> > combinations;
	std::vector<int> combination;
	combination.clear();
 	combinations.clear(); 	
 	for (int i=0; i< data.size();i++) {
 		for (int j=i+1;j<data.size();j++) {
 			for (int k=j+1;k<data.size();k++) {
 				for (int l=k+1;l<data.size();l++) {
 					for (int m=l+1;m<data.size();m++) {
						if (i==selector||j==selector||k==selector||l==selector||m==selector||selector==0) {
							combination.clear();
							combination.push_back(data[i]);
							combination.push_back(data[j]);
							combination.push_back(data[k]);
							combination.push_back(data[l]);
							combination.push_back(data[m]);
							combinations.push_back(combination);
						}
					}			
				}
 			}  
 		}
 	} 	
 	return Rcpp::wrap( combinations );
 }
 
//Return a list of combination for k=4
//In: data    : vector of the element e.g. c(1,2,3,4,5,6,7) or c(1:20)
//    selector: include only the combination including the <selector> element
//Out: a list of combination     
  
  // [[Rcpp::export]]
Rcpp::List k4combination(Rcpp::NumericVector data, int selector=0) {
 	std::vector<std::vector<int> > combinations;
	std::vector<int> combination;
	combination.clear();
 	combinations.clear(); 	
 	for (int i=0; i< data.size();i++) {
 		for (int j=i+1;j<data.size();j++) {
 			for (int k=j+1;k<data.size();k++) {
 				for (int l=k+1;l<data.size();l++) {
 					if (i==selector||j==selector||k==selector||l==selector||selector==0) {
						combination.clear();
						combination.push_back(data[i]);
						combination.push_back(data[j]);
						combination.push_back(data[k]);
						combination.push_back(data[l]);
						combinations.push_back(combination);
					}
				}
 			}  
 		}
 	} 	
 	return Rcpp::wrap( combinations );
 }
 
//Return a list of combination for k=3
//In: data    : vector of the element e.g. c(1,2,3,4,5,6,7) or c(1:20)
//    selector: include only the combination including the <selector> element
//Out: a list of combination     
  
  // [[Rcpp::export]]
 Rcpp::List k3combination(Rcpp::NumericVector data, int selector=0) {
 	std::vector<std::vector<int> > combinations;
	std::vector<int> combination;
	combination.clear();
 	combinations.clear();
 	for (int i=0; i< data.size();i++) {
 		for (int j=i+1;j<data.size();j++) {
 			for (int k=j+1;k<data.size();k++) {
 				if (i==selector||j==selector||k==selector||selector==0) {
 					combination.clear();
 					combination.push_back(data[i]);
 					combination.push_back(data[j]);
 					combination.push_back(data[k]);
 					combinations.push_back(combination);
 				}
 			}  
 		}
 	} 	
 	return Rcpp::wrap( combinations );
 }
 
//Return a list of combination for k=2
//In: data    : vector of the element e.g. c(1,2,3,4,5,6,7) or c(1:20)
//    selector: include only the combination including the <selector> element
//Out: a list of combination     
 
 // [[Rcpp::export]]
 Rcpp::List k2combination(Rcpp::NumericVector data, int selector=0) {	
 	std::vector<std::vector<int> > combinations;
	std::vector<int> combination;
	combination.clear();
 	combinations.clear(); 	
 	for (int i=0; i< data.size();i++) {
 		for (int j=i+1;j<data.size();j++) {
 			if (i==selector||j==selector||selector==0) {
 				combination.clear();
 				combination.push_back(data[i]);
 				combination.push_back(data[j]);
 			 	combinations.push_back(combination); 
 			}
 		}
 	} 	
 	return Rcpp::wrap( combinations );
 }
 
