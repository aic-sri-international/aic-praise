package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample;

import java.util.Collection;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

/** A sample of a potential function. */
public interface Sample {
	
	Sample copy();
	
	void copy(Sample another);
	
	Assignment getAssignment();
	
	Importance getImportance();
	
	Potential getPotential();
	
	void setPotential(Potential potential);
	
	void updatePotential(Potential potentialFactor);
	
	default int size() {
		return getAssignment().size();
	}
	
	default boolean instantiates(Variable variable) {
		return getAssignment().get(variable) != null;
	}

	default void set(Variable variable, Object value) {
		getAssignment().set(variable, value);
	}

	default Object get(Variable variable) {
		return getAssignment().get(variable);
	}

	default void remove(Variable variable) {
		getAssignment().mapValue().remove(variable);
	}
	
	default void removePotential(Potential potential) {
		setPotential(getPotential().divide(potential));
//		updatePotential(potential.inverse());
	}
	
	default Collection<? extends Variable> getVariables() {
		return getAssignment().getVariables();
	}

	void copyToSameInstance(Sample anotherSample);
	
	void setAssignment(Assignment assignment);
}
