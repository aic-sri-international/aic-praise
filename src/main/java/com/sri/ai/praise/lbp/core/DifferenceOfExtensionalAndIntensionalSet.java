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
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.helper.concurrent.RewriteOnBranch;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.StandardizedApartFrom;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPRewriter;

/**
 * Default implementation of {@link LBPRewriter#R_DifferenceOfExtensionalAndIntensionalSet}.
 * 
 * @author rbraz
 * 
 */
@Beta
public class DifferenceOfExtensionalAndIntensionalSet extends AbstractLBPHierarchicalRewriter implements LBPRewriter {

	private static Expression _emptySet = ExtensionalSet.makeEmptySetExpression();
	
	public DifferenceOfExtensionalAndIntensionalSet() {
	}
	
	@Override
	public String getName() {
		return R_DifferenceOfExtensionalAndIntensionalSet;
	}

	/**
	 * @see LBPRewriter#R_DifferenceOfExtensionalAndIntensionalSet
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		
		// Assertions on input arguments
		// a tuple of the form: ({a_1,...,a_n} \ { Alpha | C}_I, i).
		if (!Tuple.isTuple(expression) || Tuple.size(expression) != 2) {
			throw new IllegalArgumentException("Invalid input argument expression:"+expression);
		}
		Expression setDiff = Tuple.get(expression, 0);
		if (!setDiff.hasFunctor(FunctorConstants.SET_DIFFERENCE)) {
			throw new IllegalArgumentException("First tuple argument is not a set difference:"+setDiff);
		}
		
		Expression extensionalSet = setDiff.get(0);
		Expression intensionalSet = setDiff.get(1); 
		
		// Assertions on input arguments
		if (!Sets.isExtensionalSet(extensionalSet)) {
			throw new IllegalArgumentException(
					"Extensional set argument is not an extensional Set expression:"
							+ extensionalSet);
		}
		if (!Sets.isIntensionalSet(intensionalSet)) {
			throw new IllegalArgumentException(
					"Intensional set argument is not an intensional Set expression:"
							+ intensionalSet);
		}
		
		final int i = Tuple.get(expression, 1).intValue();
		
		
		Expression result = null;

		if (i >= ExtensionalSet.cardinality(extensionalSet)) {
			Trace.log("if i > n");
			Trace.log("    return {a1,...,an}");
			result = extensionalSet;
		} 
		else {
			Trace.log("{ Alpha' | C' }_I' <- standardize { Alpha | C }_I apart from {a1,...,an}");
			
			Expression saIntensionalSet = StandardizedApartFrom
					.standardizedApartFrom(
							intensionalSet, extensionalSet, process);
			
			Trace.log("condition <- R_formula_simplification(there exists I' : C' and a_i = Alpha')");
			
			Expression intensionalSetCondition    = ((IntensionalSet) saIntensionalSet).getCondition();
			Expression iThElementOfExtensionalSet = ExtensionalSet.getElements(extensionalSet).get(i);
			Expression intensionalSetHead         = ((IntensionalSet) saIntensionalSet).getHead();
			Expression comparison                 = Equality.make(iThElementOfExtensionalSet, intensionalSetHead);
			
			IndexExpressionsSet intensionalSetIndexExpressions = ((IntensionalSet) saIntensionalSet).getIndexExpressions();
			
			Expression unificationCondition = ThereExists.make(intensionalSetIndexExpressions, CardinalityUtil.makeAnd(intensionalSetCondition, comparison));
			Expression ifThenElseCondition  = process.rewrite(R_formula_simplification, unificationCondition);

			Trace.log("// condition={}, a_i={}", ifThenElseCondition, iThElementOfExtensionalSet);

			Trace.log("R_normalize(");
			Trace.log("    if condition");
			Trace.log("        then R_DifferenceOfExtensionalAndIntensionalSet({a_1,...,a_i-1,a_i+1,...,a_n}, { Alpha' | C' }_I', i)");
			Trace.log("        else R_DifferenceOfExtensionalAndIntensionalSet({a_1,...,a_n}, { Alpha' | C' }_I', i+1))");

			RewriteOnBranch callDifferenceOfExtensionalAndIntensionalSetRewriteOnThenBranch = new RewriteOnBranch() {
				@Override
				public Expression rewrite(Expression[] expressions, RewritingProcess process) {
					return process.rewrite(R_DifferenceOfExtensionalAndIntensionalSet,
								LPIUtil.argForDifferenceOfExtensionalAndIntensionalSetRewriteCall(expressions[0], expressions[1], i));
				}
			};
			RewriteOnBranch callDifferenceOfExtensionalAndIntensionalSetRewriteOnElseBranch = new RewriteOnBranch() {
				@Override
				public Expression rewrite(Expression[] expressions, RewritingProcess process) {
					return process.rewrite(R_DifferenceOfExtensionalAndIntensionalSet,
								LPIUtil.argForDifferenceOfExtensionalAndIntensionalSetRewriteCall(expressions[0], expressions[1], i + 1));
				}
			};

			Expression extensionalSetWithoutIthElement = ExtensionalSet.removeNonDestructively(extensionalSet, i);
			
			result = GrinderUtil
					.branchAndMergeOnACondition(
							ifThenElseCondition,
							callDifferenceOfExtensionalAndIntensionalSetRewriteOnThenBranch,
							new Expression[] { extensionalSetWithoutIthElement, saIntensionalSet },
							callDifferenceOfExtensionalAndIntensionalSetRewriteOnElseBranch,
							new Expression[] { extensionalSet, saIntensionalSet },
							R_check_branch_reachable,
							R_normalize,
							process);
		}

		// This ensures empty multisets and unisets are always
		// returned uniformly as empty unisets.
		if (ExtensionalSet.isEmptySet(result)) {
			result = _emptySet;
		}

		return result;
	}
}
