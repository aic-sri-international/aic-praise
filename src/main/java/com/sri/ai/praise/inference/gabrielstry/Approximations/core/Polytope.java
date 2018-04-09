package com.sri.ai.praise.inference.gabrielstry.Approximations.core;

import java.util.List;

import com.sri.ai.praise.inference.gabrielstry.Approximations.api.Approximation;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;

public class Polytope implements Approximation{

	@Override
	public boolean contains(Variable variable) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public List<? extends Variable> getVariables() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Factor multiply(Factor another) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Factor sumOut(List<? extends Variable> variablesToSumOut) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isIdentity() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Approximation totalIgnoreance(Variable variable, Class<? extends Factor> typeOfFactor) {
		// TODO Auto-generated method stub
		return null;
	}

}
