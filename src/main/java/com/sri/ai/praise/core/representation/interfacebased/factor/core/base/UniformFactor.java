package com.sri.ai.praise.core.representation.interfacebased.factor.core.base;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ZeroFactor.ZERO_FACTOR;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.subtract;
import static com.sri.ai.util.Util.unionArrayList;

import java.util.ArrayList;
import java.util.Collection;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.Enclosing;

/** A uniform factor. */
public class UniformFactor extends AbstractFactorWithVariables {

	public UniformFactor(ArrayList<? extends Variable> variables) {
		super(variables);
	}
	
	public UniformFactor() {
		super(arrayList());
	}
	
	@Override
	public int summationCost() {
		return 0; // costs constant time to sum over
	}

	@Override
	public Factor multiply(Factor another) {
		Factor result;
		
		if (another instanceof UniformFactor) {
			var variablesUnion = unionArrayList(list(getVariables(), another.getVariables()));
			result = new UniformFactor(variablesUnion);
		}
		else if (another instanceof ConstantFactor) {
			result = this;
		}
		else {
			throw new Error("Multiplication of " + getClass() + " by " + another.getClass() + " is not implemented.");
		}
		
		return result;
	}

	@Override
	public Factor sumOut(Collection<? extends Variable> variablesToSumOut) {
		var remaining = subtract(getVariables(), variablesToSumOut, arrayList());
		return new UniformFactor(remaining);
	}

	@Override
	public boolean isIdentity() {
		return true;
	}

	@Override
	public boolean isZero() {
		return false;
	}

	@Override
	public Factor normalize() {
		return this;
	}

	@Override
	public Factor max(Collection<? extends Variable> variablesToMaximize) {
		throw new Error((new Enclosing(){}).methodName() + " not implemented for " + getClass());
	}

	@Override
	public Factor min(Collection<? extends Variable> variablesToMinimize) {
		throw new Error((new Enclosing(){}).methodName() + " not implemented for " + getClass());
	}
	@Override
	public Factor potentialRange(Collection<? extends Variable> variablesToEliminate) {
		return ZERO_FACTOR;
	}

	@Override
	public Factor normalize(Collection<? extends Variable> variablesToNormalize) {
		var remaining = subtract(getVariables(), variablesToNormalize, arrayList());
		return new UniformFactor(remaining);
	}

}
