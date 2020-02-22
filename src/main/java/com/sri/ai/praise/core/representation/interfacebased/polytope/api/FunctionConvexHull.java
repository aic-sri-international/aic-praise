package com.sri.ai.praise.core.representation.interfacebased.polytope.api;

import java.util.Collection;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public interface FunctionConvexHull extends AtomicPolytope {

	Collection<? extends Variable> getIndices();
	
	Factor getFactor();
	
	FunctionConvexHull dynamicMultiplyIntoSingleFunctionConvexHullWithoutSimplification(Collection<? extends FunctionConvexHull> functionConvexHulls);

	FunctionConvexHull newInstance(Collection<? extends Variable> finalIndices, Factor factor);
	
	@Override
	FunctionConvexHull sumOut(Collection<? extends Variable> eliminated);
	
	/**
	 * Makes a new instance with given new indices added to the existing indices
	 * (if they overall with existing indices, a single occurrence will be kept).
	 * @param newIndices
	 * @return
	 */
	FunctionConvexHull addIndices(Collection<? extends Variable> newIndices);

	Polytope simplify();

	FunctionConvexHull normalize(Collection<? extends Variable> variables);

}