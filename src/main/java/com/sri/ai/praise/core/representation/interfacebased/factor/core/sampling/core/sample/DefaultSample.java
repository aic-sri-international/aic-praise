package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample;

import static com.sri.ai.util.Util.assertType;
import static com.sri.ai.util.Util.myAssert;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Assignment;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Importance;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.ImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Potential;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;

public class DefaultSample implements Sample {
	
	private Assignment assignment;
	
	private Importance importance;
	
	private Potential potential;
	
	public DefaultSample(ImportanceFactory importanceFactory, PotentialFactory potentialFactory) {
		this(new DefaultAssignment(), importanceFactory.make(1.0), potentialFactory.make(1.0));
	}

	public DefaultSample(Assignment assignment, Importance importance, Potential potential) {
		this.assignment = assignment;
		this.importance = importance;
		this.potential = potential;
	}

	@Override
	public void copyToSameInstance(Sample anotherSample) {
		DefaultSample anotherDefaultSample = assertType(anotherSample, DefaultSample.class, this);
		this.assignment = anotherDefaultSample.assignment;
		this.importance = anotherDefaultSample.importance;
		this.potential = anotherDefaultSample.potential;
	}

	@Override
	public Assignment getAssignment() {
		return assignment;
	}

	@Override
	public Importance getImportance() {
		return importance;
	}
	
	@Override
	public void setPotential(Potential potential) {
		this.potential = potential;
	}

	@Override
	public Potential getPotential() {
		return potential;
	}

	@Override
	public void updatePotential(Potential potentialFactor) {
		potential = potential.multiply(potentialFactor);
	}

	@Override
	public String toString() {
		return "(" + assignment + ", " + importance + ", " + potential + ")";
	}

	@Override
	public Sample copy() {
		return new DefaultSample(getAssignment().copy(), getImportance(), getPotential());
	}

	@Override
	public void copy(Sample another) {
		myAssert(another instanceof DefaultSample, () -> getClass() + " can only copy from another " + getClass() + ", but got " + another + " of class " + another.getClass());
		DefaultSample anotherDefault = (DefaultSample) another;
		assignment = anotherDefault.getAssignment().copy();
		importance = another.getImportance();
		potential = another.getPotential();
	}

	public static DefaultSample makeFreshSample() {
		return new DefaultSample(new DoubleImportanceFactory(), new DoublePotentialFactory());
		// TODO: change this to use samplingFactor's factories
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((assignment == null) ? 0 : assignment.hashCode());
		result = prime * result + ((importance == null) ? 0 : importance.hashCode());
		result = prime * result + ((potential == null) ? 0 : potential.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		DefaultSample other = (DefaultSample) obj;
		if (assignment == null) {
			if (other.assignment != null)
				return false;
		} else if (!assignment.equals(other.assignment))
			return false;
		if (importance == null) {
			if (other.importance != null)
				return false;
		} else if (!importance.equals(other.importance))
			return false;
		if (potential == null) {
			if (other.potential != null)
				return false;
		} else if (!potential.equals(other.potential))
			return false;
		return true;
	}

}
