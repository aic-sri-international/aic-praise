package com.sri.ai.praise.empiricalevaluation.core;

// Based on http://www.hlt.utdallas.edu/~vgogate/uai14-competition/information.html
public enum ProblemType {
	PR,   // Computing the the partition function and probability of evidence
	MAR,  // Computing the marginal probability distribution over variable(s) given evidence
	MAP,  // Computing the most likely assignment to all variables given evidence (also known as MPE, Most Probable Explanation)
	MMAP, // Computing the most likely assignment to a subset of variables given evidence (Marginal MAP)
}