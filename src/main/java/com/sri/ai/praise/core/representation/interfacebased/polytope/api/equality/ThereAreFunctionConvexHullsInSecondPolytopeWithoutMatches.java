package com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality;

import java.util.Collection;

import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;

/**
 * As of February 2020 this interface is never used because the current Polytopes equality check verifies the number of convex hulls in both compared polytopes is the same,
 * so there are lever left-over function convex hulls. We're leaving it anyway in case future modifications make it necessary.
 * 
 * @author braz
 *
 */
public interface ThereAreFunctionConvexHullsInSecondPolytopeWithoutMatches extends PolytopesAreDifferent {

	Collection<? extends FunctionConvexHull> getLeftOvers();

}