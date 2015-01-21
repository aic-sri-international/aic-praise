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

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.helper.concurrent.BranchRewriteTask;
import com.sri.ai.grinder.helper.concurrent.RewriteOnBranch;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.StandardizedApartFrom;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.CheapDisequalityModule;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPRewriter;

/**
 * Default implementation of {@link LBPRewriter#R_in}.
 * 
 * @author oreilly
 * 
 */
@Beta
public class In extends AbstractLBPHierarchicalRewriter implements LBPRewriter {
	
	public In() {
	}
	
	@Override
	public String getName() {
		return R_in;
	}
	
	/**
	 * @see LBPRewriter#R_in
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		// Assert input arguments
		// an expression of the form: 'Alpha in Set'.
		if (!expression.hasFunctor(LPIUtil.FUNCTOR_IN) || expression.numberOfArguments() != 2) {
			throw new IllegalArgumentException("Invalid input argument expression:"+expression);
		}
		
		Expression alpha = expression.get(0);
		Expression set   = expression.get(1);
		
		if (Justification.isEnabled()) {
			Justification.log(expression);
		}

		Expression result = null;

		if (IfThenElse.isIfThenElse(alpha)) {
			// Externalizes conditionals
			result = rewriteExternalizeAlpha(alpha, set, process);
		} 
		else if (IfThenElse.isIfThenElse(set)) {
			// Externalizes conditionals
			result = rewriteExternalizeSet(alpha, set, process);
		} 
		else if (isUnion(set)) {
			// if Set is Set_1 union ... union Set_n
			// return R_formula_simplification(Disjunction_i R_in(Alpha, Set_i))
			result = rewriteSetIsUnion(alpha, set, process);
		} 
		else if (ExtensionalSet.isExtensionalSet(set)) {
			// if Set is { a1,..., an } or {{ a1,..., an }}
			// return R_formula_simplification(Disjunction_i Alpha = ai)
			result = rewriteSetIsExtensional(alpha, set, process);
		} 
		else if (Sets.isIntensionalSet(set)) {
			// if Set is { Beta | C }_I or {{ Beta | C }}_I
			// { Beta' | C' }_I' <- standardize { Beta | C }_I apart from Alpha
			// return R_basic(R_formula_simplification(there exists I' : C' and Alpha = Beta'))
			result = rewriteSetIsIntensional(alpha, set, process);
		} 
		else {
			throw new IllegalArgumentException(
					"Set is not a conditional union of sets:" + set);
		}

		return result;
	}
	
	//
	// PRIVATE METHODS
	//
	private Expression rewriteExternalizeAlpha(Expression alpha, Expression set, RewritingProcess process) {
		Trace.log("if alpha is 'if C then alpha_1 else beta_1'");
		Trace.log("    return R_basic(if C then R_in(alpha_1, Set) else R_in(beta_1, Set)");
		
		Expression condition  = IfThenElse.getCondition(alpha);
		Expression thenBranch = IfThenElse.getThenBranch(alpha);
		Expression elseBranch = IfThenElse.getElseBranch(alpha);

		Justification.beginEqualityStep("externalizing condition " + alpha);
		if (Justification.isEnabled()) {
			Expression currentExpression =
				IfThenElse.make(
						condition,
						Expressions.apply("in", thenBranch, set),
						Expressions.apply("in", elseBranch, set));
			Justification.endEqualityStep(currentExpression);
		}
		
		Justification.beginEqualityStep("solving 'in' on then and else branches");
		Expression result = GrinderUtil.branchAndMergeOnACondition(
				condition,
				newRewriteInOnBranch(), new Expression[] { thenBranch, set },
				newRewriteInOnBranch(), new Expression[] { elseBranch, set },
				R_check_branch_reachable, 
				R_basic, process);
		Justification.endEqualityStep(result);

		return result;
	}

	private Expression rewriteExternalizeSet(Expression alpha, Expression set, RewritingProcess process) {
		Trace.log("if Set is 'if C then alpha_Set else beta_Set'");
		Trace.log("    return R_basic(if C then R_in(alpha, alpha_Set) else R_in(alpha, beta_Set)");
		
		Expression condition  = IfThenElse.getCondition(set);
		Expression thenBranch = IfThenElse.getThenBranch(set);
		Expression elseBranch = IfThenElse.getElseBranch(set);

		Justification.beginEqualityStep("externalizing conditional Set");
		if (Justification.isEnabled()) {
			Expression currentExpression =
				IfThenElse.make(
						condition,
						Expressions.apply("in", alpha, thenBranch),
						Expressions.apply("in", alpha, elseBranch));
			Justification.endEqualityStep(currentExpression);
		}
		
		Justification.beginEqualityStep("solving in on then and else branches");
		Expression result = GrinderUtil.branchAndMergeOnACondition(
				condition,
				newRewriteInOnBranch(), new Expression[] { alpha, thenBranch },
				newRewriteInOnBranch(), new Expression[] { alpha, elseBranch },
				R_check_branch_reachable, 
				R_basic, process);
		Justification.endEqualityStep(result);

		return result;
	}
	
	private Expression rewriteSetIsUnion(Expression alpha, Expression set, RewritingProcess process) {
		Trace.log("if Set is Set_1 union ... union Set_n");
		Trace.log("    return R_formula_simplification(Disjunction_i R_in(alpha, Set_i))");
		
		Justification.beginEqualityStep("Set is Set_1 union ... union Set_n");
		
		Expression result = null;
		switch(set.numberOfArguments()) {
		case 0:
			result = Expressions.FALSE;
			break;
		case 1:
			result = process.rewrite(R_in, LPIUtil.argForInRewriteCall(alpha, set.get(0)));
			break;
		default:
			// Disjunction here is a meta, preferably short-circuited, operation.
			BranchRewriteTask[] disjunctRewriters = new BranchRewriteTask[set.numberOfArguments()];
			for (int i = 0; i < set.numberOfArguments(); i++) {
				Expression set_i = set.get(i);
				disjunctRewriters[i] = new BranchRewriteTask(newRewriteInOnBranch(), new Expression[] {alpha, set_i});
			}

			Expression disjunction = GrinderUtil.branchAndMergeOnADisjunction(disjunctRewriters, process);
			
			// This simplification should remove duplicate comparisons
			// that can arise when iterating over a union of sets
			result = process.rewrite(R_formula_simplification, disjunction);
			break;
		}

		Justification.endEqualityStep(result);
		
		return result;
	}
	
	private Expression rewriteSetIsExtensional(Expression alpha, Expression set, RewritingProcess process) {
		Trace.log("if Set is { a1,..., an } or {{ a1,..., an }}");
		Trace.log("    return R_formula_simplification(Disjunction_i Alpha = ai)");
		
		Justification.beginEqualityStep("Set is extensional");
		
		// Disjunction here is a meta, preferably short-circuited, operation.
		BranchRewriteTask[] disjunctRewriters = new BranchRewriteTask[ExtensionalSet.getElements(set).size()];
		List<Expression> elements = ExtensionalSet.getElements(set);
		for (int i = 0; i < elements.size(); i++) {
			Expression a_i = elements.get(i);
			Expression eq  = Equality.make(alpha, a_i);
			if (i >= disjunctRewriters.length) {
				throw new Error("Out of bounds index");
			}
			disjunctRewriters[i] = new BranchRewriteTask(
					new RewriteOnBranch() {
						
						@Override
						public Expression rewrite(Expression[] expressions, RewritingProcess process) {
							Expression result = process.rewrite(R_formula_simplification, expressions[0]);
							return result;
						}
					},
					new Expression[] {eq});
		}

		Expression disjunction = GrinderUtil.branchAndMergeOnADisjunction(disjunctRewriters, process);
		
		// This simplification should remove duplicate comparisons
		// that can arise when iterating over elements in a multiset.
		Expression result = process.rewrite(R_formula_simplification, disjunction);
		
		Justification.endEqualityStep(result);
		
		return result;
	}
	
	private Expression rewriteSetIsIntensional(Expression alpha, Expression set, RewritingProcess process) {
		Expression result = null;
		Trace.log("if Set is { Beta | C }_I or {{ Beta | C }}_I");
		
		Expression beta = ((IntensionalSet) set).getHead();
		
		if (CheapDisequalityModule.isACheapDisequality(alpha, beta, process)) {
			Trace.log("    is guaranteed Alpha != Beta");
			Trace.log("    return \"false\""); 
			
			result = Expressions.FALSE;
		}
		else {
			Trace.log("    { Beta' | C' }_I' <- standardize { Beta | C }_I apart from Alpha");
			Trace.log("    return R_basic(there exists I' : C' and Alpha = Beta')");
			
			Justification.beginEqualityStep("Set is intensional");
	
			Expression setPrime = StandardizedApartFrom.standardizedApartFrom(set, alpha, process);
	
			IndexExpressionsSet indexExpressionsPrime = ((IntensionalSet) setPrime).getIndexExpressions();
			Expression       conditionPrime        = ((IntensionalSet) setPrime).getCondition();
			Expression       headBetaPrime         = ((IntensionalSet) setPrime).getHead();
	
			Expression alphaEqBetaPrime = Equality.make(alpha, headBetaPrime);
			Expression and              = CardinalityUtil.makeAnd(conditionPrime, alphaEqBetaPrime);
	
			Expression thereExists = ThereExists.make(indexExpressionsPrime, and);
			
			result = process.rewrite(R_basic, thereExists);
		
			Justification.endEqualityStep(result);
		}
		
		return result;
	}
	
	private RewriteOnBranch newRewriteInOnBranch() {
		return new RewriteOnBranch() {
			
			@Override
			public Expression rewrite(Expression[] expressions, RewritingProcess process) {
				Expression result = process.rewrite(R_in, LPIUtil.argForInRewriteCall(expressions[0], expressions[1])); 
				return result;
			}
		};
	}
	 
	private static boolean isUnion(Expression expression) {
		if (Expressions.hasFunctor(expression, FunctorConstants.UNION)) {
			return true;
		}

		return false;
	}
}
