package com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality;

import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.equality.FactorsEqualityCheck;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality.DefaultFirstPolytopeHasFunctionConvexHullWithoutMatchInSecond;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality.DefaultFunctionConvexHullsHaveDifferentFactors;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality.DefaultFunctionConvexHullsHaveDifferentIndices;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality.DefaultPolytopesAreEqual;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality.DefaultPolytopesAreOfIncomparableClasses;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality.DefaultPolytopesAreSimplicesOnDifferentVariables;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality.DefaultPolytopesHaveADifferentNumberOfFunctionConvexHulls;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality.DefaultPolytopesHaveDifferentSimplices;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality.DefaultThereAreFunctionConvexHullsInSecondPolytopeWithoutMatches;

public interface PolytopesEqualityCheck {

	Polytope getFirst();
	
	Polytope getSecond();
	
	boolean areEqual();

	/////////////// STATIC BUILDERS
	
	static PolytopesAreEqual polytopesAreEqual(Polytope first, Polytope second) {
		return new DefaultPolytopesAreEqual(first, second);
	}
	
	static PolytopesAreOfIncomparableClasses polytopesAreOfIncomparableClasses(Polytope first, Polytope second) {
		return new DefaultPolytopesAreOfIncomparableClasses(first, second);
	}
	
	static PolytopesHaveDifferentSimplices polytopesHaveDifferentSimplices(
			Polytope first, 
			Polytope second, 
			Set<? extends Variable> simplexVariablesInFirstButNotInSecond,
			Set<? extends Variable> simplexVariablesInSecondButNotInFirst) {
		
		return new DefaultPolytopesHaveDifferentSimplices(first, second, simplexVariablesInFirstButNotInSecond, simplexVariablesInSecondButNotInFirst);
	}
	
	static FunctionConvexHullsHaveDifferentIndices functionConvexHullsHaveDifferentIndices(
			Polytope first, 
			Polytope second, 
			Set<? extends Variable> indicesInFirstButNotInSecond,
			Set<? extends Variable> indicesInSecondButNotInFirst) {
		
		return new DefaultFunctionConvexHullsHaveDifferentIndices(first, second, indicesInFirstButNotInSecond, indicesInSecondButNotInFirst);
	}
	
	static FunctionConvexHullsHaveDifferentFactors functionConvexHullsHaveDifferentFactors(
			Polytope first, 
			Polytope second, 
			FactorsEqualityCheck factorsEqualityCheck) {
		
		return new DefaultFunctionConvexHullsHaveDifferentFactors(first, second, factorsEqualityCheck);
	}

	static PolytopesAreSimplicesOnDifferentVariables polytopesAreSimplicesOnDifferentVariables(Polytope first, Polytope second) {
		return new DefaultPolytopesAreSimplicesOnDifferentVariables(first, second);
	}

	static FirstPolytopeHasFunctionConvexHullWithoutMatchInSecond firstPolytopeHasFunctionConvexHullWithoutMatchInSecond(
			Polytope first,
			Polytope second,
			FunctionConvexHull functionConvexHullInFirst,
			Set<? extends PolytopesEqualityCheck> equalityChecksAgainstRemainingFunctionConvexHullsInSecond) {
		
		return new DefaultFirstPolytopeHasFunctionConvexHullWithoutMatchInSecond(first, second, functionConvexHullInFirst, equalityChecksAgainstRemainingFunctionConvexHullsInSecond);
	}
	
	static ThereAreFunctionConvexHullsInSecondPolytopeWithoutMatches thereAreFunctionConvexHullsInSecondPolytopeWithoutMatches(
			Polytope first,
			Polytope second,
			Set<? extends FunctionConvexHull> leftOvers) {
		
		return new DefaultThereAreFunctionConvexHullsInSecondPolytopeWithoutMatches(first, second, leftOvers);
	}

	static PolytopesHaveADifferentNumberOfFunctionConvexHulls polytopesHaveADifferentNumberOfFunctionConvexHulls(Polytope first, Polytope second) {
		return new DefaultPolytopesHaveADifferentNumberOfFunctionConvexHulls(first, second);
	}
	
}
