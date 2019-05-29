package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Assignment;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Importance;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Potential;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.util.Enclosing;

public class UnmodifiableSample implements Sample {
	
	private Sample base;
	
	public UnmodifiableSample(Sample baseSample) {
		this.base = baseSample;
	}

	@Override
	public void copyToSameInstance(Sample anotherSample) {
		throw new Error((new Enclosing(){}).methodName() + " cannot modify " + this.getClass());
	}

	@Override
	public Assignment getAssignment() {
		return new UnmodifiableAssignment(base.getAssignment());
	}

	@Override
	public Importance getImportance() {
		return base.getImportance();
	}
	
	@Override
	public void setPotential(Potential potential) {
		throw new Error((new Enclosing(){}).methodName() + " cannot modify " + this.getClass());
	}

	@Override
	public Potential getPotential() {
		return base.getPotential();
	}

	@Override
	public void updatePotential(Potential potentialFactor) {
		throw new Error((new Enclosing(){}).methodName() + " cannot modify " + this.getClass());
	}

	@Override
	public void setAssignment(Assignment assignment) {
		throw new Error((new Enclosing(){}).methodName() + " cannot modify " + this.getClass());
	}

	@Override
	public String toString() {
		return base.toString();
	}

	@Override
	public Sample copy() {
		return new DefaultSample(getAssignment().copy(), getImportance(), getPotential());
	}

	@Override
	public void copy(Sample another) {
		throw new Error((new Enclosing(){}).methodName() + " cannot modify " + this.getClass());
	}

	@Override
	public Sample copyWithNewAssignment(Assignment newAssignment) {
		return new UnmodifiableSample(base.copyWithNewAssignment(newAssignment));
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((base == null) ? 0 : base.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		UnmodifiableSample other = (UnmodifiableSample) obj;
		if (base == null) {
			if (other.base != null) {
				return false;
			}
		} else if (!base.equals(other.base)) {
			return false;
		}
		return true;
	}

}
