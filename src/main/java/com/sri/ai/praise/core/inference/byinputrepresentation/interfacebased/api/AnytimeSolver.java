package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.api;

import java.util.Iterator;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.computation.anytime.api.Approximation;

public interface AnytimeSolver extends BinaryFunction<Variable, FactorNetwork, Iterator<Approximation<Factor>>> {

}
