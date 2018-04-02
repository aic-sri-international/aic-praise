package com.sri.ai.praise.model.v1.imports.uai.nodes;

import java.util.List;

import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.praise.lang.grounded.markov.FactorTable;
import com.sri.ai.util.collect.ManyToManyRelation;

/**
 * This is rather a wrapper for Factor table.
 * It already defines most of the functionality we need to a table factor to have.
 * 
 * But it works with different "terminology" : variable indexes instead of the variables themselves
 * 											and does not implement functions of Factors, but similar functions under different names
 * @author gabriel
 *
 */

public class UAIFactor implements Factor{
	ManyToManyRelation<Variable, Integer> mapBetweenVariableAndVariableIndex;
	FactorTable table;
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
	
}
