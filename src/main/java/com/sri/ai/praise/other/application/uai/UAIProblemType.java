package com.sri.ai.praise.other.application.uai;

public enum UAIProblemType {
	PR,   // Computing the partition function and probability of evidence (PR inference)
	MAP,  // Computing the most likely assignment to all variables given evidence (MAP Inference)
	MAR,  // Computing the marginal probability distribution over a variable given evidence (MAR inference)
	MMAP  // Computing the most likely assignment to a subset of variables given evidence (MarginalProblem MAP inference) 
}
