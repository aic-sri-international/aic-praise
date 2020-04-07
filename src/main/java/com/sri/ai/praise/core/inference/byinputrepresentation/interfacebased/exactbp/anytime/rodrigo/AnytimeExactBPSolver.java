package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo;

import java.util.Iterator;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.computation.anytime.api.Approximation;

public class AnytimeExactBPSolver implements BinaryFunction<Variable, FactorNetwork, Iterator<Approximation<Factor>>> {

	@Override
	public Iterator<Approximation<Factor>> apply(Variable query, FactorNetwork factorNetwork) {
		return new AnytimeExactBP<>(new ExactBP(query, factorNetwork));
	}

}
