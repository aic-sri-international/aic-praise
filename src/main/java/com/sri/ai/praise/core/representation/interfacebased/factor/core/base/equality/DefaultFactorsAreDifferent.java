package com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;

/**
 * A generic factor equality check implementation for different factors with a message.

 * @author braz
 *
 * @param <F>
 * @param <V>
 */
public class DefaultFactorsAreDifferent<F extends Factor> extends AbstractFactorsAreDifferent<F> {

	private String message;
	
	public DefaultFactorsAreDifferent(F first, F second, String message) {
		super(first, second);
		this.message = message;
	}
	
	@Override
	public String toString() {
		return "Factors " + getFirst() + " and " + getSecond() + " are different because " + message;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((message == null) ? 0 : message.hashCode());
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
		DefaultFactorsAreDifferent other = (DefaultFactorsAreDifferent) obj;
		if (message == null) {
			if (other.message != null) {
				return false;
			}
		} else if (!message.equals(other.message)) {
			return false;
		}
		return true;
	}
}
