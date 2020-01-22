package com.sri.ai.praise.core.representation.interfacebased.factor.core.ndarray;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.explanation.tree.ExplanationTree;

public class NDArrayFactor implements Factor {

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
	public boolean isZero() {
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

	@Override
	public Factor add(Factor another) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Factor invert() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Factor max(Collection<? extends Variable> variablesToMaximize) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Factor argmax(Collection<? extends Variable> variablesToMaximize) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Factor min(Collection<? extends Variable> variablesToMinimize) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Factor argmin(Collection<? extends Variable> variablesToMinimize) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ExplanationTree getExplanation() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setExplanation(ExplanationTree explanation) {
		// TODO Auto-generated method stub

	}

}
