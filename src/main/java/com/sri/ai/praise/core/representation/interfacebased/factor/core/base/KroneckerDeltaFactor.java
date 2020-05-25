package com.sri.ai.praise.core.representation.interfacebased.factor.core.base;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.base.IdentityFactor.IDENTITY_FACTOR;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.subtract;

import java.util.Collection;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.Enclosing;

/** A factor on two variables representing their Kronecker delta. */
public class KroneckerDeltaFactor extends AbstractFactorWithVariables {

	public KroneckerDeltaFactor(Variable variable1, Variable variable2) {
		super(arrayList(variable1, variable2));
	}
	
	@Override
	public int summationCost() {
		return 0; // costs constant time to sum over
	}

	@Override
	public Factor multiply(Factor another) {
		Factor result = null;
		
		if (another instanceof KroneckerDeltaFactor) {
			throw new Error("Product of two " + getClass() + " instances is not implemented; users must transform one of them into a different factor representation that can be multiplied by a Kronecker. We anticipate a new product representation of factors that would simply keep the multiplied factors, and which would allow this implementation, but for the time being it is not available.");
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
		switch (remaining.size()) {
		case 0: return IDENTITY_FACTOR;
		case 1: return new UniformFactor(remaining);
		case 2: return this;
		default: throw new Error("Remaining variables in " + getClass() + "." + (new Enclosing() {}).methodName() + " but this should never happen because the factor should have at most two variables. Current variables are " + getVariables());
		}
	}

	@Override
	public boolean isIdentity() {
		return false;
	}

	@Override
	public boolean isZero() {
		return false;
	}

	@Override
	public Factor normalize(Collection<? extends Variable> variablesToNormalize) {
		return sumOut(variablesToNormalize); // turns out to be the same as summing out
	}
}
