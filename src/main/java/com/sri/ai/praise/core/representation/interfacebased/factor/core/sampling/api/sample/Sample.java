package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample;

/** A sample of a potential function. */
public interface Sample {
	
	Sample copy();
	
	void copy(Sample another);
	
	Assignment getAssignment();
	
	Importance getImportance();
	
	Potential getPotential();
	
	void updatePotential(Potential potentialFactor);
	
	default int size() {
		return getAssignment().size();
	}
	
}
