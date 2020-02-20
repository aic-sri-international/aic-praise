package com.sri.ai.praise.core.representation.interfacebased.polytope.core;

import java.util.Collection;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;

public interface FunctionConvexHull extends AtomicPolytope {

	Collection<? extends Variable> getIndices();
	
	Factor getFactor();
	
	FunctionConvexHull multiplyIntoSingleFunctionConvexHull(Collection<? extends FunctionConvexHull> defaultFunctionConvexHulls);

	FunctionConvexHull newInstance(Collection<? extends Variable> finalIndices, Factor factor);

}