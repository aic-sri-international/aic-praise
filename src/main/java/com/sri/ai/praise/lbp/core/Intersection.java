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
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.helper.concurrent.RewriteOnBranch;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.StandardizedApartFrom;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.CheapDisequalityModule;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.util.Util;

/**
 * Default implementation of {@link LBPRewriter#R_intersection}.
 * 
 * @author oreilly
 *
 */
@Beta
public class Intersection extends AbstractLBPHierarchicalRewriter implements LBPRewriter {
	private static final Expression _emptySet = ExtensionalSet.makeEmptySetExpression();
	
	public Intersection() {
	}
	
	@Override
	public String getName() {
		return R_intersection;
	}
	
	/**
	 * @see LBPRewriter#R_intersection
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = null;
		
		if (!Tuple.isTuple(expression) || Tuple.size(expression) != 2) {
			throw new IllegalArgumentException("Invalid input argument expression:"+expression);
		}
		
		Expression set1 = Tuple.get(expression, 0);
		Expression set2 = Tuple.get(expression, 1);
		
		if (IfThenElse.isIfThenElse(set1)) {
			result = rewriteS1Conditional(set1, set2, process);
		}
		else if (IfThenElse.isIfThenElse(set2)) {
			result = rewriteS2Conditional(set1, set2, process);
		}
		else if ((Sets.isIntensionalMultiSet(set1) && Sets.isIntensionalMultiSet(set2))
				||
				(Sets.isIntensionalUniSet(set1) && Sets.isIntensionalUniSet(set2))) {
			Trace.log("Set1 is { (on I1) Alpha1 | C1 } and Set2 is { (on I2) Alpha2 | C2 } (or multiset version)");
			if (CheapDisequalityModule.isACheapDisequality(((IntensionalSet) set1).getHead(), ((IntensionalSet) set2).getHead(), process)) {
				Trace.log("    is guaranteed Alpha1 != Alpha2");
				Trace.log("    return {}"); 
				
				result = _emptySet;
			}
			else {
				Trace.log("    standardize Set1 apart from (I2, Alpha2, C2)");
				List<Expression> i2 = ((ExtensionalIndexExpressionsSet) ((IntensionalSet) set2).getIndexExpressions()).getList();
				Expression alpha2          = ((IntensionalSet) set2).getHead();
				Expression c2              = ((IntensionalSet) set2).getCondition();
				Expression tupleI2Alpha2C2 = Tuple.make(Tuple.make(i2), alpha2, c2);
				
				Expression saSet1           = StandardizedApartFrom.standardizedApartFrom(set1, tupleI2Alpha2C2, process);
				
				RewritingProcess subProcess = GrinderUtil.extendContextualSymbolsWithIntensionalSetIndices(set2, process);
				subProcess                  = GrinderUtil.extendContextualSymbolsWithIntensionalSetIndices(saSet1, subProcess);

				Trace.log("    C <- R_complete_normalize(Alpha1 = Alpha2 and C1 and C2)");
				Expression alpha1 = ((IntensionalSet) saSet1).getHead();
				Expression c1     = ((IntensionalSet) saSet1).getCondition();
				Expression c      = subProcess.rewrite(R_complete_normalize, CardinalityUtil.makeAnd(Equality.make(alpha1, alpha2), CardinalityUtil.makeAnd(c1, c2)));
				if (c.equals(Expressions.FALSE)) {
					Trace.log("    if C is \"false\"") ;
					Trace.log("        return {}");
					result = _emptySet;
				} 
				else {
					Trace.log("    I <- concatenation of I1 and I2");
					Trace.log("    return { (on I) Alpha1 | C } (or multiset version)");
					List<Expression> i = Util.list();
					ExtensionalIndexExpressionsSet saSet1IndexExpressions;
					ExtensionalIndexExpressionsSet set2IndexExpressions;
					try {
						saSet1IndexExpressions = (ExtensionalIndexExpressionsSet) ((IntensionalSet) saSet1).getIndexExpressions();
						set2IndexExpressions = (ExtensionalIndexExpressionsSet) ((IntensionalSet) set2).getIndexExpressions();
					}
					catch (ClassCastException e) {
						throw new Error("Intersection for intensional sets implemented for extensional index expressions case only");
					}
					i.addAll(saSet1IndexExpressions.getList());
					i.addAll(set2IndexExpressions.getList());
					
					Expression unsimplifiedResult = IntensionalSet.make(Sets.getLabel(set1), new ExtensionalIndexExpressionsSet(i), alpha1, c);
					result = process.rewrite(R_simplify, unsimplifiedResult);
//					System.out.println("Unsimplified: " + unsimplifiedResult);	
//					System.out.println("Simplified  : " + result + "\n");	
//					System.out.println("Context: " + process.getContextualConstraint());	
				}
			}
		} 
		else if (Sets.isExtensionalUniSet(set1) && Sets.isExtensionalUniSet(set2)) {
			Trace.log("Set1 is {...} and Set2 is {...}");
			Trace.log("    return R_set_diff(Set1 \\ R_set_diff(Set1 \\ Set2))");
			Expression set1DiffSet2 = process.rewrite(R_set_diff, LPIUtil.argForSetDifferenceRewriteCall(set1, set2));
			result = process.rewrite(R_set_diff, LPIUtil.argForSetDifferenceRewriteCall(set1, set1DiffSet2));
		}
		else {
			Trace.log("Else");
			Trace.log("    \"Not currently supported\"");
			throw new IllegalArgumentException("Not currently supported:"+expression);
		}
		
		return result;
	}
	
	private Expression rewriteS1Conditional(Expression set1, Expression set2, RewritingProcess process) {
		Trace.log("if S1 is 'if C then Alpha else Beta'");
		Trace.log("    return R_basic(if C then R_intersection(Alpha intersection S2) else R_intersection(Beta intersection S2))");

		Expression condition  = IfThenElse.getCondition(set1);
		Expression thenBranch = IfThenElse.getThenBranch(set1);
		Expression elseBranch = IfThenElse.getElseBranch(set1);

		Justification.beginEqualityStep("externalizing conditional");
		if (Justification.isEnabled()) {
			Expression currentExpression =
				IfThenElse.make(
						condition,
						Expressions.apply(FunctorConstants.INTERSECTION, thenBranch, set2),
						Expressions.apply(FunctorConstants.INTERSECTION, elseBranch, set2));
			Justification.endEqualityStep(currentExpression);
		}
		
		Justification.beginEqualityStep("solving intersection in then and else branches");
		Expression result = GrinderUtil.branchAndMergeOnACondition(
				condition,
				newCallIntersectionRewrite(), new Expression[] { thenBranch, set2 },
				newCallIntersectionRewrite(), new Expression[] { elseBranch, set2 },
				R_check_branch_reachable, 
				R_basic, process);
		Justification.endEqualityStep(result);

		return result;
	}
	
	private Expression rewriteS2Conditional(Expression set1, Expression set2, RewritingProcess process) {
		Trace.log("if S2 is 'if C then Alpha else Beta'");
		Trace.log("    return R_basic(if C then R_intersection(S1 intersection Alpha) else R_intersection(S1 intersection Beta))");

		Expression condition  = IfThenElse.getCondition(set2);
		Expression thenBranch = IfThenElse.getThenBranch(set2);
		Expression elseBranch = IfThenElse.getElseBranch(set2);

		Justification.beginEqualityStep("externalizing conditional");
		if (Justification.isEnabled()) {
			Expression currentExpression =
				IfThenElse.make(
						condition,
						Expressions.apply(FunctorConstants.SET_DIFFERENCE, set1, thenBranch),
						Expressions.apply(FunctorConstants.SET_DIFFERENCE, set1, elseBranch));
			Justification.endEqualityStep(currentExpression);
		}
		
		Justification.beginEqualityStep("solving intersection in then and else branches");
		Expression result = GrinderUtil.branchAndMergeOnACondition(
				condition,
				newCallIntersectionRewrite(), new Expression[] { set1, thenBranch },
				newCallIntersectionRewrite(), new Expression[] { set1, elseBranch },
				R_check_branch_reachable, 
				R_basic, process);
		Justification.endEqualityStep(result);

		return result;
	}
	
	private RewriteOnBranch newCallIntersectionRewrite() {
		return new RewriteOnBranch() {
			@Override
			public Expression rewrite(Expression[] expressions, RewritingProcess process) {
				Expression result = process.rewrite(R_intersection, LPIUtil.argForIntersectionRewriteCall(expressions[0], expressions[1]));
				return result;
			}
		};
	}
}
