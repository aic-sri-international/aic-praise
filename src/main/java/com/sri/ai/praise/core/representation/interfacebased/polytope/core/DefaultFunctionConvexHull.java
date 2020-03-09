/*
 * Copyright (c) 2015, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-praise nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.praise.core.representation.interfacebased.polytope.core;

import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck.functionConvexHullsHaveDifferentFactors;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck.functionConvexHullsHaveDifferentIndices;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck.polytopesAreEqual;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck.polytopesAreOfIncomparableClasses;
import static com.sri.ai.util.Util.setFrom;
import static com.sri.ai.util.Util.subtract;
import static com.sri.ai.util.Util.unorderedEquals;

import java.util.Collection;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck;

/**
 * A default implementation of {@link AbstractFunctionConvexHull} that does not perform any simplification.
 * 
 * @author braz
 *
 */
final public class DefaultFunctionConvexHull extends AbstractFunctionConvexHull {
	
	public DefaultFunctionConvexHull(Collection<? extends Variable> indices, Factor factor) {
		super(indices, factor);
	}
	
	@Override
	public DefaultFunctionConvexHull newInstance(Collection<? extends Variable> indices, Factor factor) {
		return new DefaultFunctionConvexHull(indices, factor);
	}
	
	@Override
	public FunctionConvexHull simplify() {
		return this;
	}

	@Override
	public boolean equalsModuloPermutations(Object another) {
		if (another instanceof FunctionConvexHull) {
			return mathematicallyEqualsToAnotherFunctionConvexHull((FunctionConvexHull) another);
		}
		else {
			return false;
		}
	}

	private boolean mathematicallyEqualsToAnotherFunctionConvexHull(FunctionConvexHull another) {
		Collection<? extends Variable> c1 = getIndices();
		Collection<? extends Variable> c2 = another.getIndices();
		return
				unorderedEquals(c1, c2) 
				&&
				getFactor().mathematicallyEquals(another.getFactor());
	}

	@Override
	public PolytopesEqualityCheck checkEquality(Polytope another) {
		if (another instanceof FunctionConvexHull) {
			var anotherFunctionConvexHull = (FunctionConvexHull) another;
			if ( ! unorderedEquals(getIndices(), anotherFunctionConvexHull.getIndices())) {
				return makeEqualityCheckOfFunctionConvexHullsWithDifferentIndices(anotherFunctionConvexHull);
			}
			else {
				return checkEqualityOfFunctionConvexHullsWithSameIndices(anotherFunctionConvexHull);
			}
		}
		else {
			return polytopesAreOfIncomparableClasses(this, another);
		}
	}

	public PolytopesEqualityCheck makeEqualityCheckOfFunctionConvexHullsWithDifferentIndices(FunctionConvexHull anotherFunctionConvexHull) {
		var indicesInFirstButNotInSecond = setFrom(subtract(getIndices(), anotherFunctionConvexHull.getIndices()));
		var indicesInSecondButNotInFirst = setFrom(subtract(anotherFunctionConvexHull.getIndices(), getIndices()));
		return functionConvexHullsHaveDifferentIndices(this, anotherFunctionConvexHull, indicesInFirstButNotInSecond, indicesInSecondButNotInFirst);
	}

	public PolytopesEqualityCheck checkEqualityOfFunctionConvexHullsWithSameIndices(FunctionConvexHull anotherFunctionConvexHull) {
		var factorsEqualityCheck = getFactor().checkEquality(anotherFunctionConvexHull.getFactor());
		if (factorsEqualityCheck.areEqual()) {
			return polytopesAreEqual(this, anotherFunctionConvexHull);
		}
		else {
			return functionConvexHullsHaveDifferentFactors(this, anotherFunctionConvexHull, factorsEqualityCheck);
		}
	}
}