package com.sri.ai.praise.inference.gabrielstry.Approximations.core;

import java.util.List;

import com.sri.ai.praise.inference.gabrielstry.Approximations.api.Approximation;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;

public class boxApproximation implements Approximation{
	Factor minFactor;
	Factor maxFactor;
	
	public boxApproximation(Factor minFactor,Factor maxFactor) {
		//They must be on the same variables! but checking that takes linear time...
		this.minFactor = minFactor;
		this.maxFactor = maxFactor;
	}
	
	@Override
	public boolean contains(Variable variable) {
		return this.maxFactor.contains(variable);
	}

	@Override
	public List<? extends Variable> getVariables() {
		return this.maxFactor.getVariables();
	}

	@Override
	public Factor multiply(Factor another) {
		// TODO Auto-generated method stub
		//multiply 
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
		
		//reed the paper, but it is gonna be a zero factor for min and ones for max.
		
		//ou o simplex...
		//dont know
		return null;
	}

}
