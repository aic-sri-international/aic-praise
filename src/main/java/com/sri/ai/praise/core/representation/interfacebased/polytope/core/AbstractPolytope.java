package com.sri.ai.praise.core.representation.interfacebased.polytope.core;

import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;

public abstract class AbstractPolytope implements Polytope {

	/**
	 * Checks equality against another polytope, considering it equal only
	 * if it is another {@link Polytope} which is equal
	 * according to {@link #checkEquality(Polytope)}.
	 */
	@Override
	public boolean equals(Object another) {
		return
				another instanceof Polytope
				&&
				checkEquality((Polytope) another).areEqual();
	}
	
	/**
	 * Throws an exception to indicate concrete class has not re-implemented
	 * a hashCode compatible with {@link #equals(Object)}, which
	 * agrees with {@link #checkEquality(Polytope)}.
	 */
	@Override
	public int hashCode() {
		throw new Error("hashCode must be implemented in agreement with equals(Object) and checkEquality(Polytope), but " + getClass() + " does not override it");
	}
	
}
