package com.sri.ai.praise.core.representation.interfacebased.factor.core.base;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;


/**
 * A class representing an {@link Factor} that is a 0 factor.
 * This can be defined independently of the {@link Representation} used.
 * <p>
 * 
 * @author redouane
 *
 */
public class ZeroFactor extends ConstantFactor {
	
	public final static ZeroFactor ZERO_FACTOR = new ZeroFactor();
	
	private ZeroFactor() {
		super(0.);
	}

	@Override
	public Factor multiply(Factor another) {
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
	public Factor add(Factor another) {
		return another;
	}

	@Override
	public boolean isZero() {
		return true;
	}

	@Override
	public Factor invert() {
		throw new Error("Zero factor cannot be inverted");
	}
	
}
