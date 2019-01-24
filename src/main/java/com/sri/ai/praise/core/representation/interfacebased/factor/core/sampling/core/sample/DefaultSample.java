package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample;

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
	public Assignment getAssignment() {
		return assignment;
	}

	@Override
	public Importance getImportance() {
		return importance;
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

}
