package com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.equality.FactorsEqualityCheck;

public abstract class AbstractFactorsEqualityCheck<F> implements FactorsEqualityCheck<F> {

	private F first;
	private F second;
	
	public AbstractFactorsEqualityCheck(F first, F second) {
		this.first = first;
		this.second = second;
	}

	@Override
	public F getFirst() {
		return first;
	}

	@Override
	public F getSecond() {
		return second;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((first == null) ? 0 : first.hashCode());
		result = prime * result + ((second == null) ? 0 : second.hashCode());
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
		AbstractFactorsEqualityCheck other = (AbstractFactorsEqualityCheck) obj;
		if (first == null) {
			if (other.first != null) {
				return false;
			}
		} else if (!first.equals(other.first)) {
			return false;
		}
		if (second == null) {
			if (other.second != null) {
				return false;
			}
		} else if (!second.equals(other.second)) {
			return false;
		}
		return true;
	}

}
