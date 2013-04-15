/*
 * Copyright (c) 2013, SRI International
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
package com.sri.ai.praise.lbp.core;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.helper.concurrent.RewriteOnBranch;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPRewriter;

/**
 * Default implementation of {@link LBPRewriter#R_DifferenceOfExtensionalAndExtensionalSet}.
 * 
 * @author oreilly
 * 
 */
@Beta
public class DifferenceOfExtensionalAndExtensionalSet extends AbstractLBPHierarchicalRewriter implements LBPRewriter {

	private static Expression _emptySet = ExtensionalSet.makeEmptySetExpression();
	
	public DifferenceOfExtensionalAndExtensionalSet() {
	}
	
	@Override
	public String getName() {
		return R_DifferenceOfExtensionalAndExtensionalSet;
	}
	
	/**
	 * @see LBPRewriter#R_DifferenceOfExtensionalAndExtensionalSet
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		// Assertions on input arguments
		// a tuple of the form: ({a_1,...,a_n} \ {b_1,...,b_m}, i, j).
		if (!Tuple.isTuple(expression) || Tuple.size(expression) != 3) {
			throw new IllegalArgumentException("Invalid input argument expression:"+expression);
		}
		Expression setDiff = Tuple.get(expression, 0);
		if (!setDiff.hasFunctor(FunctorConstants.SET_DIFFERENCE)) {
			throw new IllegalArgumentException("First tuple argument is not a set difference:"+setDiff);
		}
		Expression extensionalSetA = setDiff.get(0);
		Expression extensionalSetB = setDiff.get(1);
		
		if (!Sets.isExtensionalSet(extensionalSetA)) {
			throw new IllegalArgumentException(
					"Extensional argument A is not an extensional Set expression:"
							+ extensionalSetA);
		}

		if (!Sets.isExtensionalSet(extensionalSetB)) {
			throw new IllegalArgumentException(
					"Extensional argument B is not an extensional Set expression:"
							+ extensionalSetB);
		}
	
		final int i = Tuple.get(expression, 1).intValue(); 
		final int j = Tuple.get(expression, 2).intValue();

		Expression result = null;
		int n = ExtensionalSet.cardinality(extensionalSetA);
		int m = ExtensionalSet.cardinality(extensionalSetB);

		if (i >= n) {
			Trace.log("if i > n");
			Trace.log("    return {a_1,...,a_n}");
			result = extensionalSetA;
		} 
		else if (j >= m) {
			Trace.log("if j > m");
			Trace.log("    return R_DifferenceOfExtensionalAndExtensionalSet({a_1,...,a_i,...,a_n}, {b_1,...,b_m}, i+1, 1)");
			result = process.rewrite(R_DifferenceOfExtensionalAndExtensionalSet, 
						LPIUtil.argForDifferenceOfExtensionalAndExtensionalSetRewriteCall(extensionalSetA, extensionalSetB, i + 1, 0));
		} 
		else {
			Trace.log("condition <- R_formula_simplification(a_i = b_j)");

			Expression ai = ExtensionalSet.getElements(extensionalSetA).get(i);
			Expression bj = ExtensionalSet.getElements(extensionalSetB).get(j);

			Expression aEqualB   = Equality.make(ai, bj);
			Expression condition = process.rewrite(R_formula_simplification, aEqualB);

			Trace.log("// condition={}, a_i={}, b_j={}", condition, ai, bj);

			// Avoid recursing into unnecessary branches if the truth or
			// falsehood of the condition is known in advance.
			if (Expressions.TRUE.equals(condition)) {
				Trace.log("if true = condition");
				Trace.log("    return R_DifferenceOfExtensionalAndExtensionalSet({a_1,...,a_i-1,a_i+1,...,a_n}, {b_1,...,b_j-1,b_j+1,...,b_m}, i, 1)");
				
				// This handles multi-sets by subtracting the matching element from B.
				Expression extensionalSetBMinusJ = ExtensionalSet.removeNonDestructively(extensionalSetB, j);
				
				result = rewriteThenBranch(extensionalSetA, extensionalSetBMinusJ, i, process);
			} 
			else if (condition.equals(Expressions.FALSE)) {
				Trace.log("if condition is 'false'");
				Trace.log("    return R_DifferenceOfExtensionalAndExtensionalSet({a_1,...,a_i,...,a_n}, {b_1,...,b_m}, i, j+1)");

				result = rewriteElseBranch(extensionalSetA, extensionalSetB, i, j, process);
			} 
			else {
				Trace.log("return R_basic(if condition");
				Trace.log("              then R_DifferenceOfExtensionalAndExtensionalSet({a_1,...,a_i-1,a_i+1,...,a_n}, {b_1,...,b_m}, i, 1)");
				Trace.log("              else R_DifferenceOfExtensionalAndExtensionalSet({a_1,...,a_i,...,a_n}, {b_1,...,b_m}, i, j+1))");

				RewriteOnBranch callDifferenceOfExtensionalAndExtensionalSetRewriteOnThenBranch = new RewriteOnBranch() {
					@Override
					public Expression rewrite(Expression[] expressions, RewritingProcess process) {

						return rewriteThenBranch(expressions[0], expressions[1], i, process);
					}
				};
				RewriteOnBranch callDifferenceOfExtensionalAndExtensionalSetRewriteOnElseBranch = new RewriteOnBranch() {
					@Override
					public Expression rewrite(Expression[] expressions, RewritingProcess process) {

						return rewriteElseBranch(expressions[0], expressions[1], i, j, process);
					}
				};
				// truth or falsehood of condition is not known in advance.
				result = GrinderUtil
						.branchAndMergeOnACondition(
								condition,
								callDifferenceOfExtensionalAndExtensionalSetRewriteOnThenBranch,
								new Expression[] { extensionalSetA, extensionalSetB },
								callDifferenceOfExtensionalAndExtensionalSetRewriteOnElseBranch,
								new Expression[] { extensionalSetA, extensionalSetB },
								R_check_branch_reachable,
								R_basic, process);
			}
		}

		// This ensures empty multisets and unisets are always
		// returned uniformly as empty unisets.
		if (ExtensionalSet.isEmptySet(result)) {
			result = _emptySet;
		}

		return result;
	}

	//
	// PRIVATE METHODS
	//
	
	private Expression rewriteThenBranch(Expression extensionalSetA, Expression extensionalSetB, int i, RewritingProcess process) {
		// then
		// R_DifferenceOfExtensionalAndExtensionalSet({a_1,...,a_i-1,a_i+1,...,a_n}, {b_1,...,b_m}, i, 1)
		Expression extensionalSetAMinusI = ExtensionalSet.removeNonDestructively(extensionalSetA, i);
		// Note: we pass {b_1,...,b_m} instead of {b_1,...,b_j-1,b_j+1,...,b_m} in the case we know
		// the condition is definitely true as at the end it allows simplification on the nested
		// conditional structure to take into account the equality between logical variables.
		// For e.g:
		//
		// { A, B } - { C }
		//
		// will be simplified to this:
		//
		// if A = C then { B } else if B = C then { A } else if A = B then { A } else { A, B }
		//
		// if we remove the b_j element in advance, however, if we leave, as we are currently doing, we get:
		//
		// if A = C then if A = B then { } else { B } else if B = C then { A } else if A = B then { A } else { A, B }
		//
		// which also tests for A = B in order to derive the {}, which is correct.
		// Alternatively this could possibly be done by externalizing conditionals on Set A up front
		// but this appears more natural/straight forward.
		Expression thenBranch = process.rewrite(R_DifferenceOfExtensionalAndExtensionalSet, 
				LPIUtil.argForDifferenceOfExtensionalAndExtensionalSetRewriteCall(extensionalSetAMinusI, extensionalSetB, i, 0));

		return thenBranch;
	}

	private Expression rewriteElseBranch(Expression extensionalSetA, Expression extensionalSetB, int i, int j, RewritingProcess process) {
		// else 
		// R_DifferenceOfExtensionalAndExtensionalSet({a_1,...,a_i,...,a_n}, {b_1,...,b_m}, i, j+1))
		Expression elseBranch = process.rewrite(R_DifferenceOfExtensionalAndExtensionalSet, 
				LPIUtil.argForDifferenceOfExtensionalAndExtensionalSetRewriteCall(extensionalSetA, extensionalSetB, i, j + 1));


		return elseBranch;
	}
}
