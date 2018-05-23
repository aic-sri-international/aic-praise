package com.sri.ai.praise.inference.generic.representation.core;

import static com.sri.ai.util.Util.list;
import static java.util.Collections.unmodifiableList;

import java.util.List;
import java.util.Map;

import com.sri.ai.praise.inference.generic.representation.api.Factor;
import com.sri.ai.praise.inference.generic.representation.api.Variable;

/**
 * A class representing an {@link Factor} that is a 0 factor.
 * This can be defined independently of the {@link Representation} used.
 * <p>
 * 
 * @author redouane
 *
 */
public class ZeroFactor implements Factor {
	
	public final static ZeroFactor ZERO_FACTOR = new ZeroFactor();
	
	private ZeroFactor() {
	}

	@Override
	public boolean contains(Variable variable) {
		return false;
	}

	@Override
	public List<? extends Variable> getVariables() {
		return unmodifiableList(list());
	}

	@Override
	public Factor multiply(Factor another) {
		return ZERO_FACTOR;
	}

	@Override
	public Factor sumOut(List<? extends Variable> variablesToSumOut) {
		return this;
	}
	
	@Override
	public boolean isIdentity() {
		return false;
	}

	@Override
	public String toString() {
		return "0";
	}

	@Override
	public Double getEntryFor(Map<? extends Variable, ? extends Object> variableInstantiations) {
		return 0.;
	}

	@Override
	public Factor normalize() {
		return this;
	}

	@Override
	public Factor add(Factor another) {
		return another;
	}

	@Override
	public Factor multiplyByConstant(Number constant) {
		return this;
	}

	@Override
	public boolean isZero() {
		return true;
	}

	@Override
	public Factor invert() {
		// TODO Unsure how to handle this
		return null;
	}
}
