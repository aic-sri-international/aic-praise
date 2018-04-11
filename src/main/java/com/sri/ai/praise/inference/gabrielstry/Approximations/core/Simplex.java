package com.sri.ai.praise.inference.gabrielstry.Approximations.core;

import static com.sri.ai.praise.inference.representation.core.IdentityFactor.IDENTITY_FACTOR;
import static com.sri.ai.util.Util.list;

import java.util.List;

import com.sri.ai.praise.inference.gabrielstry.Approximations.api.Approximation;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;

public class Simplex implements Approximation{
	Variable variable;
	
	public Simplex(Variable variable) {
		this.variable = variable;
		
		//p cada instanciacao de v, criar um fator indicatriz...
		//como fazer: adicionar "get values" pras variaveis ou um get instantiations (olhar como faz no UAIModel...);
		//criar indicator function no factor...
		//fazer um for aqui
	}

	@Override
	public boolean contains(Variable variable) {
		return this.variable.equals(variable);
	}

	@Override
	public List<? extends Variable> getVariables() {
		List<Variable> result = list(this.variable);
		return result;
	}

	@Override
	public Factor multiply(Factor another) {
		
		return null;
	}

	@Override
	public Factor sumOut(List<? extends Variable> variablesToSumOut) {
		if(variablesToSumOut.contains(this.variable)) {
			return IDENTITY_FACTOR;
		}
		return this;
	}

	@Override
	public boolean isIdentity() {
		return false;
	}
}
