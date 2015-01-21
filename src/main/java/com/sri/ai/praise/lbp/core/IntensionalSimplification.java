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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.SemanticSubstitute;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPRewriter;

/**
 * Default implementation of {@link LBPRewriter#R_intensional_simplification}.
 * 
 * @author oreilly
 * 
 */
@Beta
public class IntensionalSimplification extends AbstractLBPHierarchicalRewriter implements LBPRewriter {

	private static final Expression _emptySet = ExtensionalSet.makeEmptySetExpression();

	public IntensionalSimplification() {
	}
	
	@Override
	public String getName() {
		return R_intensional_simplification;
	}

	/**
	 * @see LBPRewriter#R_intensional_simplification
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {

		// Assert input arguments
		Expression intensionalSet = expression;
		if (!Sets.isIntensionalSet(intensionalSet)) {
			throw new IllegalArgumentException(
					"Intensional set argument is not an intensional set expression:="
							+ intensionalSet);
		}

		// Assume no simplification occurs by default.
		Expression result = intensionalSet; 

		Expression intSetHead      = ((IntensionalSet) intensionalSet).getHead();
		Expression intSetCondition = ((IntensionalSet) intensionalSet).getCondition();
		
		IndexExpressionsSet indexExpressions = ((IntensionalSet) intensionalSet).getIndexExpressions();
		assert indexExpressions instanceof ExtensionalIndexExpressionsSet : "IntensionalSimplification not implemented for intensional sets with non-extensional index expressions"; 
		List<Expression> intensionalSetIndexExpressions = new ArrayList<Expression>(((ExtensionalIndexExpressionsSet)indexExpressions).getList());
		Object[]         cPrimeAndiEqualsBeta   = new Object[3];
		
		RewritingProcess subProcess = LPIUtil.extendContextualSymbolsWithIntensionalSetIndicesInferringDomainsFromUsageInRandomVariables(intensionalSet, process);
		
		Expression simplifiedIntSetCondition = subProcess.rewrite(R_formula_simplification, intSetCondition);

		if (simplifiedIntSetCondition.equals(Expressions.FALSE)) {
			Trace.log("if C is false");
			Trace.log("    return empty_set");
			result = _emptySet;
		} 
		else if (intensionalSetIndexExpressions.isEmpty()) {
			Trace.log("if I is empty");
			Trace.log("    return if C then { Alpha } else empty_set");

			Expression thenBranch = null;
			if (Sets.isIntensionalUniSet(intensionalSet)) {
				thenBranch = ExtensionalSet.makeUniSetExpression(Arrays.asList(intSetHead));
			} 
			else {
				thenBranch = ExtensionalSet.makeMultiSet(Arrays.asList(intSetHead));
			}
			if (Expressions.TRUE.equals(simplifiedIntSetCondition)) {
				result = thenBranch;
			} 
			else {
				result = IfThenElse.make(intSetCondition, thenBranch, _emptySet);
			}
		} 
		else if (isConditionConjunctForIndex(intSetCondition, intensionalSetIndexExpressions, cPrimeAndiEqualsBeta)) {
			// Note: from Rodrigo 11 Nov 2011 -
			// We are assuming that Beta is in the range of i.
			// This follows the assumption we talked about a
			// bit ago that things being compared are assumed to be of the same
			// type. One day we will have to reckon with this, but for now it
			// serves our purposes quite nicely.
			Trace.log("if C is (C' and i = Beta) for i an index in I // We are assuming that Beta is in the range of i.");
			Trace.log("    return R_intensional_simplifications({ Alpha[i/Beta] | C'[i/Beta] }_{I \\ {i}})");

			Expression cPrime = (Expression) cPrimeAndiEqualsBeta[0];
			int        i      = (Integer)    cPrimeAndiEqualsBeta[1];
			Expression beta   = (Expression) cPrimeAndiEqualsBeta[2];
			Expression index  = IndexExpressions.getIndex(intensionalSetIndexExpressions.get(i));

			Expression substitutedAlpha  = SemanticSubstitute.replace(intSetHead, index, beta, subProcess);
			Expression substitutedCPrime = SemanticSubstitute.replace(cPrime, index, beta, subProcess);
			
			intensionalSetIndexExpressions.remove(i);

			Expression substitutedIntensionalSet = IntensionalSet
					.make(
							Sets.getLabel(intensionalSet),
							new ExtensionalIndexExpressionsSet(intensionalSetIndexExpressions),
							substitutedAlpha,
							substitutedCPrime);

			// Ensure not simplified to an extensional or 
			// conditional on creation
			if (Sets.isIntensionalSet(substitutedIntensionalSet)) {
				result = process.rewrite(R_intensional_simplification, substitutedIntensionalSet);
			} 
			else {
				// Already simplified.
				result = substitutedIntensionalSet;
			}
		}

		return result;
	}

	//
	// PRIVATE METHODS
	//
	private static boolean isConditionConjunctForIndex(
			Expression intSetCondition,
			List<Expression> intSetIndexExpressions,
			Object[] cPrimeAndiEqualsBeta) {
		// if C is (C' and i = Beta) for i an index in I

		// Get the indices on their own first
		List<Expression> indices = new ArrayList<Expression>();
		for (Expression indexExpression : intSetIndexExpressions) {
			indices.add(IndexExpressions.getIndex(indexExpression));
		}
		
		List<Expression> conjuncts = new ArrayList<Expression>(And.getConjuncts(intSetCondition));
		for (int conjunctIndex = 0; conjunctIndex < conjuncts.size(); conjunctIndex++) {
			Expression conjunct = conjuncts.get(conjunctIndex);
			// Restrict to equality on 2 arguments as more than 2
			// may not be legal, e.gs:
			// (on X) ... | X = a = b
			if (isEquality(conjunct) && conjunct.numberOfArguments() == 2) {
				for (int i = 0; i < indices.size(); i++) {
					Expression index = indices.get(i);
					Expression beta  = null;
					for (int b = 0; b < conjunct.numberOfArguments(); b++) {
						Expression eqTerm = conjunct.get(b);
						if (index.equals(eqTerm)) {
							if (b == 0) {
								// is of the form:
								// i = Beta
								// i.e. take Beta to be the second term
								// for equalities between 2 or more arguments
								beta = conjunct.get(1);
							} 
							else {
								// is of the form:
								// ... = Beta = i = ...
								// i.e. take Beta to be immediately left
								// for equalities between more than 2 arguments
								beta = conjunct.get(b - 1);
							}
							break;
						}
					}

					if (beta != null) {
						// Remove this conjunct in order to be able to
						// isolate C'
						conjuncts.remove(conjunctIndex);
						cPrimeAndiEqualsBeta[0] = And.make(conjuncts);
						cPrimeAndiEqualsBeta[1] = i;
						cPrimeAndiEqualsBeta[2] = beta;
						return true;
					}
				}
			}
		}

		return false;
	}

	private static boolean isEquality(Expression expr) {
		if (Expressions.hasFunctor(expr, FunctorConstants.EQUAL)) {
			return true;
		}

		return false;
	}
}
