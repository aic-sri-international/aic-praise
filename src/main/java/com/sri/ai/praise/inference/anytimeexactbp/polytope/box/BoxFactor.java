package com.sri.ai.praise.inference.anytimeexactbp.polytope.box;

import java.util.List;
import java.util.Map;

import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;

public class BoxFactor<T extends Factor> implements Factor{

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
	public Double getEntryFor(Map<? extends Variable, ? extends Object> variableInstantiations) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Factor normalize() {
		// TODO Auto-generated method stub
		return null;
	}

}
