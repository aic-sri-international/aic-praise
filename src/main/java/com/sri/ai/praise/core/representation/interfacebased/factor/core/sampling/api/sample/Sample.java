package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

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
	
	default boolean instantiates(Variable variable) {
		return getAssignment().get(variable) != null;
	}
	
}
