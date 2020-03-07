package com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality;

import java.util.Collection;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.equality.FactorsHaveDifferentValues;

public class DefaultFactorsHaveDifferentValues<F extends Factor, V> extends AbstractFactorsAreDifferent<F> implements FactorsHaveDifferentValues<F, V> {

	public static <F extends Factor, V> DefaultFactorsHaveDifferentValues<F, V> factorsHaveDifferentValues(
			F first, 
			F second, 
			List<? extends V> violatingAssignment,
			double valueOfFirst,
			double valueOfSecond) {
		return new DefaultFactorsHaveDifferentValues<F,V>(first, second, violatingAssignment, valueOfFirst, valueOfSecond);
	}
	
	private List<? extends V> violatingAssignment;
	private double valueOfFirst;
	private double valueOfSecond;
	
	public DefaultFactorsHaveDifferentValues(
			F first, 
			F second, 
			List<? extends V> violatingAssignment,
			double valueOfFirst,
			double valueOfSecond) {
		
		super(first, second);
		this.violatingAssignment = violatingAssignment;
		this.valueOfFirst = valueOfFirst;
		this.valueOfSecond = valueOfSecond;
	}

	@Override
	public Collection<? extends V> getViolatingAssignment() {
		return violatingAssignment;
	}

	@Override
	public double getValueOfFirst() {
		return valueOfFirst;
	}

	@Override
	public double getValueOfSecond() {
		return valueOfSecond;
	}
	
	@Override
	public String toString() {
		return 
				"Factors have different values at " + 
				getFirst().getVariables() + " = " + getViolatingAssignment() + 
				", with first being " + getValueOfFirst() + 
				" and second " + getValueOfSecond();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		long temp;
		temp = Double.doubleToLongBits(valueOfFirst);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(valueOfSecond);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		result = prime * result + ((violatingAssignment == null) ? 0 : violatingAssignment.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!super.equals(obj)) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		DefaultFactorsHaveDifferentValues other = (DefaultFactorsHaveDifferentValues) obj;
		if (Double.doubleToLongBits(valueOfFirst) != Double.doubleToLongBits(other.valueOfFirst)) {
			return false;
		}
		if (Double.doubleToLongBits(valueOfSecond) != Double.doubleToLongBits(other.valueOfSecond)) {
			return false;
		}
		if (violatingAssignment == null) {
			if (other.violatingAssignment != null) {
				return false;
			}
		} else if (!violatingAssignment.equals(other.violatingAssignment)) {
			return false;
		}
		return true;
	}


}
