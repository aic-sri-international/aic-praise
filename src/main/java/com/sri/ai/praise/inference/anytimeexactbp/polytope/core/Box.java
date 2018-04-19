package com.sri.ai.praise.inference.anytimeexactbp.polytope.core;

import java.util.Collection;

import com.sri.ai.praise.inference.anytimeexactbp.polytope.api.AtomicPolytope;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;

public class Box extends AbstractAtomicPolytope{
	private Factor phiMax;
	private Factor phiMin;
	
	public Box(Factor phiMin, Factor phiMax) {
		this.phiMax = phiMax;
		this.phiMin = phiMin;
	}

	@Override
	public AtomicPolytope getProductIfItIsANonIdentityAtomicPolytopeOrNullOtherwise(
			AtomicPolytope nonIdentityAtomicAnother) {
		if(nonIdentityAtomicAnother instanceof Box) {			
			Box result = new Box(this.phiMin.multiply(((Box)nonIdentityAtomicAnother).getPhiMin()),
								 this.phiMax.multiply(((Box)nonIdentityAtomicAnother).getPhiMax()));
			return result;
		}
		return null;
	}

	@Override
	public Collection<? extends Variable> getFreeVariables() {
		return phiMax.getVariables();
	}

	@Override
	public boolean isIdentity() {
		if(phiMax.isIdentity() && 
				phiMin.isIdentity()) {
			return true;
		}
		return false;
	}
	
	public Factor getPhiMax() {
		return this.phiMax;
	}
	
	public Factor getPhiMin() {
		return this.phiMin;
	}
	
}
