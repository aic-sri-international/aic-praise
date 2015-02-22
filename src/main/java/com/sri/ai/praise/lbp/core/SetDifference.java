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
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.StandardizedApartFrom;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
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
 * Default implementation of {@link LBPRewriter#R_set_diff}.
 * 
 * @author oreilly
 * 
 */
@Beta
public class SetDifference extends AbstractLBPHierarchicalRewriter implements LBPRewriter {

	private static final Expression _emptySet = ExtensionalSet.makeEmptySetExpression();

	public SetDifference() {
	}
	
	@Override
	public String getName() {
		return R_set_diff;
	}
	
	/**
	 * @see LBPRewriter#R_set_diff
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		
		// Assert input arguments
		// an expression of the form: 'S1 \ S2'.
		if (!expression.hasFunctor(FunctorConstants.SET_DIFFERENCE) || expression.numberOfArguments() != 2) {
			throw new IllegalArgumentException("Invalid input argument expression:"+expression);
		}
		
		Expression set1 = expression.get(0);
		Expression set2 = expression.get(1);
		
		if (Justification.isEnabled()) {
			Justification.log(Expressions.apply(FunctorConstants.SET_DIFFERENCE, set1, set2));
		}
		
		Expression result = null;

		// Cases:
		if (IfThenElse.isIfThenElse(set1)) {
			// Externalizes Conditionals
			// if S1 is 'if C then Alpha else Beta'
			// return R_basic(if C then R_set_diff(Alpha \ S2) else
			// R_set_diff(Beta \ S2))
			result = rewriteS1Conditional(set1, set2, process);
		} 
		else if (IfThenElse.isIfThenElse(set2)) {
			// Externalizes Conditionals
			// if S2 is 'if C then Alpha else Beta'
			// return R_basic(if C then R_set_diff(S1 \ Alpha) else
			// R_set_diff(S1 \ Beta))
			result = rewriteS2Conditional(set1, set2, process);
		} 
		else if (set1.equals(_emptySet) || (isUnion(set1) && set1.numberOfArguments() == 0)) {
			// ALBP-122: ensure logic is short-circuited if S1 is empty.
			result = _emptySet;
		} 
		else if (set2.equals(_emptySet) || (isUnion(set2) && set2.numberOfArguments() == 0)) {
			// if S2 is the empty set
			result = set1;
		} 
		else if (isUnionOfUniSets(set1)
				&& (Sets.isUniSet(set2) || isUnion(set2))) {
			// if S1 is S11 union S1rest, where S1i and S2 are unisets
			// return R_set_diff(S11 \ S2) union R_set_diff(S1rest \ S2)
			result = rewriteS1UnionUniSets(set1, set2, process);
		} 
		else if (isUnion(set2) && Sets.isSet(set1) && isUnionOfSets(set2)) {
			// if S2 is S21 union S2rest, where S1 and S2i are sets
			// return R_set_diff(R_set_diff(S1 \ S21) \ S2rest)
			result = rewriteS2UnionUniSets(set1, set2, process);
		} 
		else if (isUnionOfMultiSetsOrSingletons(set1) && Sets.isSingletonExtensionalSet(set2)) {
			// if S1 is S11 union S1rest, where each S1i is a multiset
			// guaranteed to have unique elements,
			// or a singleton, and S2 is a singleton { b }
			// return
			// R_basic(if R_in(b in S11)
			// then R_set_diff(S11 \ S2) union S1rest
			// else S11 union R_set_diff(S1rest \ S2))
			result = rewriteS1UnionMultiSetsOrSingletons(set1, set2, process);
		} 
		else if (Sets.isExtensionalMultiSet(set1) && Sets.isSingletonExtensionalSet(set2)) {
			// if S1 is {{a1,...,an}} and S2 is { b }
			// return R_DifferenceOfExtensionalAndExtensionalSet({{a1,...,an}},
			// {b}, 1, 1)
			result = rewriteS1ExtensionalMultiSet(set1, set2, process);
		} 
		else if (Sets.isIntensionalMultiSet(set1) && Sets.isSingletonExtensionalSet(set2)) {			
			// if S1 is {{ Alpha | C }}_I and S2 is { b }
			// {{ Alpha' | C' }}_I' <- standardize {{ Alpha | C}}_I apart from {b}
			// C'' <- R_formula_simplification(C' and not Alpha' = b)  with cont. variables extended by I'
			// return R_basic({{ Alpha' | C'' }}_I')
			result = rewriteS1IntensionalMultiSet(set1, set2, process);
		} 
		else if (Sets.isMultiSet(set1) && Sets.isExtensionalUniSet(set2)) {
			// if S1 is a multiset and S2 is {b1, ..., bm}
			// return R_set_diff(R_set_diff(S1, {b1}), {b2,...,bm})
			result = rewriteS1MultiSetS2ExtensionalUniSet(set1, set2, process);
		} 
		else if (Sets.isMultiSet(set1) && isUnion(set2)) {
			// if S1 is a multiset and S2 is S21 union ... union S2m
			// return R_set_diff(R_set_diff(S1, S21), S22 union ... union S2m)
			result = rewriteS1MultiSetS2Union(set1, set2, process);
		} 
		else if (Sets.isExtensionalUniSet(set1)
				&& Sets.isExtensionalUniSet(set2)) {
			// if S1 is {a1,...,an} and S2 is {b1,...,bm}
			// return R_DifferenceOfExtensionalAndExtensionalSet({a1,...,an},
			// {b1,...,bm}, 1, 1)
			result = rewriteS1andS2ExtensionalUniSets(set1, set2, process);
		} 
		else if (Sets.isIntensionalUniSet(set1)
				&& Sets.isExtensionalUniSet(set2)) {
			// if S1 is { Alpha | C }_I and S2 is {b1,...,bm}
			// { Alpha' | C' }_I' <- standardize { Alpha | C }_I apart from {b1,...,bm}
			// C'' <- R_formula_implification(C' and not (Disjunction_i Alpha' = b_i)) with cont. variables extended by I'
			// return R_basic({ Alpha' | C'' }_I')
			result = rewriteS1IntensionalS2ExtensionalUniSets(set1, set2, process);
		} 
		else if (Sets.isIntensionalUniSet(set1)
				&& Sets.isIntensionalUniSet(set2)) {
			// if S1 is { Alpha | C }_I and S2 is { Alpha' | C' }_I'
			// { Alpha' | C' }_I' <- standardize { Alpha' | C' }_I' apart from (Alpha, C)
			// C''<- R_formula_simplification(C and for all I' : C' => Alpha != Alpha') with cont. variables extended by I
			// return R_basic({ Alpha | C'' }_I
			result = rewriteS1andS2IntensionalUniSets(set1, set2, process);
		} 
		else if (Sets.isExtensionalUniSet(set1)
				&& Sets.isIntensionalUniSet(set2)) {
			// if S1 is {a1,...,an} and S2 is { Alpha | C }_I
			// return 
			// R_DifferenceOfExtensionalAndIntensionalSet ({a1,...,an}, { Alpha | C }_I, 1)
			result = rewriteS1ExtensionalS2IntensionalUniSets(set1, set2, process);
		} 
		else {
			throw new IllegalArgumentException("Arguments set1 and set2 do not conform to expected types");
		}

		return result;
	}

	//
	// PRIVATE METHODS
	//
	private static boolean isUnionOfSets(Expression expression) {
		boolean result = false;
		if (isUnion(expression)) {
			result = true;
			for (Expression u_i : expression.getArguments()) {
				if (IfThenElse.isIfThenElse(u_i) || isUnion(u_i)) {
					// Skip these, as they will be handled later on
					// by the recursive calls to R_set_diff
					continue;
				}
				if (!Sets.isSet(u_i)) {
					result = false;
					break;
				}
			}
		}

		return result;
	}
	
	private static boolean isUnionOfUniSets(Expression expression) {
		boolean result = false;
		if (isUnion(expression)) {
			result = true;
			for (Expression u_i : expression.getArguments()) {
				if (IfThenElse.isIfThenElse(u_i) || isUnion(u_i)) {
					// Skip these, as they will be handled later on
					// by the recursive calls to R_set_diff
					continue;
				}
				if (!Sets.isUniSet(u_i)) {
					result = false;
					break;
				}
			}
		}

		return result;
	}

	private static boolean isUnionOfMultiSetsOrSingletons(Expression expression) {
		boolean result = false;
		if (isUnion(expression)) {
			result = true;
			if (expression.numberOfArguments() > 0) {
				boolean multisetExists = false;
				for (Expression u_i : expression.getArguments()) {
					if (IfThenElse.isIfThenElse(u_i) || isUnion(u_i)) {
						// Skip these, as they will be handled later on
						// by the recursive calls to R_set_diff
						continue;
					}
					if (Sets.isMultiSet(u_i)) {
						multisetExists = true;
					} 
					else if (!Sets.isSingletonExtensionalSet(u_i)) {
						result = false;
						break;
					}
				}
				// At least 1 multiset should be present otherwise
				// this would return true if a union of unisets
				// with single elements.
				result = result && multisetExists;
			}
		}

		return result;
	}

	private static boolean isUnion(Expression expression) {
		if (Expressions.hasFunctor(expression, FunctorConstants.UNION)) {
			return true;
		}

		return false;
	}

	private Expression rewriteS1Conditional(Expression set1, Expression set2, RewritingProcess process) {
		Trace.log("if S1 is 'if C then Alpha else Beta'");
		Trace.log("    return R_basic(if C then R_set_diff(Alpha \\ S2) else R_set_diff(Beta \\ S2))");

		Expression condition  = IfThenElse.getCondition(set1);
		Expression thenBranch = IfThenElse.getThenBranch(set1);
		Expression elseBranch = IfThenElse.getElseBranch(set1);

		Justification.beginEqualityStep("externalizing conditional");
		if (Justification.isEnabled()) {
			Expression currentExpression =
				IfThenElse.make(
						condition,
						Expressions.apply(FunctorConstants.SET_DIFFERENCE, thenBranch, set2),
						Expressions.apply(FunctorConstants.SET_DIFFERENCE, elseBranch, set2));
			Justification.endEqualityStep(currentExpression);
		}
		
		Justification.beginEqualityStep("solving set differences in then and else branches");
		Expression result = GrinderUtil.branchAndMergeOnACondition(
				condition,
				newCallSetDifferenceRewrite(), new Expression[] { thenBranch, set2 },
				newCallSetDifferenceRewrite(), new Expression[] { elseBranch, set2 },
				R_check_branch_reachable, 
				R_basic, process);
		Justification.endEqualityStep(result);

		return result;
	}

	private Expression rewriteS2Conditional(Expression set1, Expression set2, RewritingProcess process) {
		Trace.log("if S2 is 'if C then Alpha else Beta'");
		Trace.log("    return R_basic(if C then R_set_diff(S1 \\ Alpha) else R_set_diff(S1 \\ Beta))");

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
		
		Justification.beginEqualityStep("solving set differences in then and else branches");
		Expression result = GrinderUtil.branchAndMergeOnACondition(
				condition,
				newCallSetDifferenceRewrite(), new Expression[] { set1, thenBranch },
				newCallSetDifferenceRewrite(), new Expression[] { set1, elseBranch },
				R_check_branch_reachable, 
				R_basic, process);
		Justification.endEqualityStep(result);

		return result;
	}

	private Expression rewriteS1UnionUniSets(Expression set1, Expression set2, RewritingProcess process) {
		Trace.log("if S1 is S11 union S1rest, where S1i and S2 are unisets");
		Trace.log("    return R_set_diff(S11 \\ S2) union R_set_diff(S1rest \\ S2)");

		Expression set11    = getFirstOfUnion(set1);
		Expression set1rest = getRestOfUnion(set1);
		Trace.log("// S11={}", set11);
		Trace.log("// S1rest={}", set1rest);

		Justification.beginEqualityStep("distributing difference over union");
		if (Justification.isEnabled()) {
			Expression currentExpression =
				Expressions.apply(
						FunctorConstants.UNION,
						Expressions.apply(FunctorConstants.SET_DIFFERENCE, set11, set2),
						Expressions.apply(FunctorConstants.SET_DIFFERENCE, set1rest, set2));
			Justification.endEqualityStep(currentExpression);
		}
		
		Justification.beginEqualityStep("solving set differences inside union");
		Expression d1 = process.rewrite(R_set_diff, LPIUtil.argForSetDifferenceRewriteCall(set11, set2));
		Trace.log("// R_set_diff(S11 \\ S2)={}", d1);
		Expression d2 = process.rewrite(R_set_diff, LPIUtil.argForSetDifferenceRewriteCall(set1rest, set2));
		Trace.log("// R_set_diff(S1rest \\ S2)={}", d2);
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.UNION, d1, d2);
		Justification.endEqualityStep(result);

		return result;
	}

	private Expression rewriteS2UnionUniSets(Expression set1, Expression set2, RewritingProcess process) {
		Trace.log("if S2 is S21 union S2rest, where S1 and S2i are sets");
		Trace.log("    return R_set_diff(R_set_diff(S1 \\ S21) \\ S2rest)");

		Expression set21    = getFirstOfUnion(set2);
		Expression set2rest = getRestOfUnion(set2);
		Trace.log("// S21={}", set21);
		Trace.log("// S2rest={}", set2rest);

		Justification.beginEqualityStep("difference from union is successive differences");
		if (Justification.isEnabled()) {
			Expression currentExpression =
				Expressions.apply(
						FunctorConstants.SET_DIFFERENCE,
						Expressions.apply(FunctorConstants.SET_DIFFERENCE, set1, set21),
						set2rest);
			Justification.endEqualityStep(currentExpression);
		}
		
		Justification.beginEqualityStep("solving first difference");
		Expression d1 = process.rewrite(R_set_diff, LPIUtil.argForSetDifferenceRewriteCall(set1, set21));
		if (Justification.isEnabled()) {
			Expression currentExpression = Expressions.apply(FunctorConstants.SET_DIFFERENCE, d1, set2rest);
			Justification.endEqualityStep(currentExpression);
		}

		Justification.beginEqualityStep("solving remaining difference");
		Trace.log("// R_set_diff(S1 \\ S21)={}", d1);
		Expression result = process.rewrite(R_set_diff, LPIUtil.argForSetDifferenceRewriteCall(d1, set2rest));
		Justification.endEqualityStep(result);

		return result;
	}

	private Expression rewriteS1UnionMultiSetsOrSingletons(Expression set1, Expression set2, RewritingProcess process) {
		Trace.log("if S1 is S11 union S1rest, where each S1i is a multiset guaranteed to have unique elements,");
		Trace.log("                                or a singleton, and S2 is a singleton { b }");
		Trace.log("    return");
		Trace.log("    R_basic(if R_in(b in S11)");
		Trace.log("            then R_set_diff(S11 \\ S2) union S1rest");
		Trace.log("            else S11 union R_set_diff(S1rest \\ S2))");

		final Expression b        = set2.get(0);
		final Expression set11    = getFirstOfUnion(set1);
		final Expression set1Rest = getRestOfUnion(set1);
		Trace.log("// S11={}", set11);
		Trace.log("// S1rest={}", set1Rest);

		Expression result = null;
		
		Justification.beginEqualityStep("if single element in second set is in first set in the union, subtract from it; otherwise, subtract from the remaining sets");
		Expression currentExpression = null;
		if (Justification.isEnabled()) {
			currentExpression =
				IfThenElse.make(
						Expressions.apply("in", b, set11),
						Expressions.apply(
								FunctorConstants.UNION,
								Expressions.apply("-", set11, set2),
								set1Rest
						),
						Expressions.apply(
								FunctorConstants.UNION,
								set11,
								Expressions.apply("-", set1Rest, set2)));
			Justification.endEqualityStep(currentExpression);
		}

		// Create the condition
		// if R_in(b in S11)
		Justification.beginEqualityStep("computing condition");
		Expression condition = process.rewrite(R_in, LPIUtil.argForInRewriteCall(b, set11));
		if (Justification.isEnabled()) {
			currentExpression = currentExpression.set(0, condition);
			Justification.endEqualityStep(currentExpression);
		}

		// Create the then branch computation routine
		RewriteOnBranch callSetDifferenceRewriteOnThenBranch = new RewriteOnBranch() {
			@Override
			public Expression rewrite(Expression[] expressions, RewritingProcess process) {
				// then R_set_diff(S11 \ S2) union S1rest
				Expression set11DiffS2 = process.rewrite(R_set_diff, LPIUtil.argForSetDifferenceRewriteCall(expressions[0], expressions[1]));
				Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.UNION, set11DiffS2, set1Rest);
				return result;
			}
		};

		// Create the else branch computation routine
		RewriteOnBranch callSetDifferenceRewriteOnElseBranch = new RewriteOnBranch() {
			@Override
			public Expression rewrite(Expression[] expressions, RewritingProcess process) {
				// else S11 union R_set_diff(S1rest \ S2)
				Expression set1restDiffS2 = process.rewrite(R_set_diff, LPIUtil.argForSetDifferenceRewriteCall(expressions[0], expressions[1]));
				Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.UNION, set11, set1restDiffS2);
				return result;
			}
		};

		Justification.beginEqualityStep("computing differences at then and else branches");
		result = GrinderUtil.branchAndMergeOnACondition(
				condition,
				callSetDifferenceRewriteOnThenBranch, new Expression[] { set11,    set2 },
				callSetDifferenceRewriteOnElseBranch, new Expression[] { set1Rest, set2 },
				R_check_branch_reachable, 
				R_basic, process);
		Justification.endEqualityStep(result);

		return result;
	}

	private Expression rewriteS1ExtensionalMultiSet(Expression set1, Expression set2, RewritingProcess process) {
		Trace.log("if S1 is {{a1,...,an}} and S2 is { b }");
		Trace.log("   return R_DifferenceOfExtensionalAndExtensionalSet({{a1,...,an}}, {b}, 1, 1)");

		Justification.beginEqualityStep("computing difference of extensional sets");
		Expression result = process.rewrite(R_DifferenceOfExtensionalAndExtensionalSet,
								LPIUtil.argForDifferenceOfExtensionalAndExtensionalSetRewriteCall(set1, set2, 0, 0));
		Justification.endEqualityStep(result);

		return result;
	}

	private Expression rewriteS1IntensionalMultiSet(Expression set1, Expression set2, RewritingProcess process) {
		Expression result = null;
		
		Trace.log("if S1 is {{ Alpha | C }}_I and S2 is { b }");

		Expression b = ExtensionalSet.getElements(set2).get(0);
		
		if (CheapDisequalityModule.isACheapDisequality(((IntensionalSet) set1).getHead(), b, process)) {
			Justification.beginEqualityStep("is guaranteed Alpha != b'");
			
			Trace.log("    is guaranteed Alpha != b'");
			Trace.log("    return S1"); 
			
			result = set1;
			
			Justification.endEqualityStep(result);
		} else {
			Trace.log("    {{ Alpha' | C' }}_I' <- standardize {{ Alpha | C}}_I apart from {b}");
			Justification.beginEqualityStep("difference is intensional set constrained so that its elements are not equal to " + b);
			
			Expression saS1 = StandardizedApartFrom
					.standardizedApartFrom(
							set1, set2, process);
	
			Trace.log("// {{ Alpha' | C' }}_I'={}", saS1);
			
	
			Expression alphaPrime      = ((IntensionalSet) saS1).getHead();
			Expression cPrime          = ((IntensionalSet) saS1).getCondition();
			IndexExpressionsSet iPrime = ((IntensionalSet) saS1).getIndexExpressions();
	
			Expression alphaPrimeEqualb    = Equality.make(alphaPrime, b);
			Expression notAlphaPrimeEqualb = Not.make(alphaPrimeEqualb);
	
			Expression cPrimeAndNotAlphaPrimeEqualb = CardinalityUtil.makeAnd(cPrime, notAlphaPrimeEqualb);
			
			if (Justification.isEnabled()) {
				Expression currentExpression = IntensionalSet.make(Sets.getLabel(saS1), iPrime, alphaPrime, cPrimeAndNotAlphaPrimeEqualb);
				Justification.endEqualityStep(currentExpression);
			}
			
			Trace.log("    C'' <- R_formula_simplification(C' and not Alpha' = b) with cont. variables extended by I'");
			Justification.beginEqualityStep("simplifying condition");
			
			RewritingProcess processIPrime  = LPIUtil.extendContextualSymbolsWithIntensionalSetIndicesInferringDomainsFromUsageInRandomVariables(saS1, process);
			Expression       cPrimePrime    = processIPrime.rewrite(R_formula_simplification, cPrimeAndNotAlphaPrimeEqualb);
			
			result = IntensionalSet.make(Sets.getLabel(saS1), iPrime, alphaPrime, cPrimePrime);
			
			Justification.endEqualityStep(result);
			
			Trace.log("    return R_basic({{ Alpha' | C'' }}_I')");
	
			Justification.beginEqualityStep("simplifying overall expression");
			result = process.rewrite(R_basic, result);
			Justification.endEqualityStep(result);			
		}

		return result;
	}
	
	private Expression rewriteS1MultiSetS2ExtensionalUniSet(Expression set1, Expression set2, RewritingProcess process) {
		Trace.log("if S1 is a multiset and S2 is {b1, ..., bm}");
		Trace.log("    return R_set_diff(R_set_diff(S1, {b1}), {b2,...,bm})");
		
		Justification.beginEqualityStep("computing difference of multiset and extensional uniset");
		
		Expression b1             = ExtensionalSet.makeSingletonOfSameTypeAs(set2, ExtensionalSet.getElements(set2).get(0));
		Expression s1DiffB1Result = process.rewrite(R_set_diff, LPIUtil.argForSetDifferenceRewriteCall(set1, b1));
		Expression b2ToBm         = ExtensionalSet.removeNonDestructively(set2, 0);
		Expression result         = process.rewrite(R_set_diff, LPIUtil.argForSetDifferenceRewriteCall(s1DiffB1Result, b2ToBm));
		
		Justification.endEqualityStep(result);
		
		return result;
	}
	
	private Expression rewriteS1MultiSetS2Union(Expression set1, Expression set2, RewritingProcess process) {
		Trace.log("if S1 is a multiset and S2 is S21 union ... union S2m");
		Trace.log("    return R_set_diff(R_set_diff(S1, S21), S22 union ... union S2m)");
		
		Justification.beginEqualityStep("computing difference of multiset and union");
		
		Expression s21             = set2.get(0);
		Expression s1DiffS21Result = process.rewrite(R_set_diff, LPIUtil.argForSetDifferenceRewriteCall(set1, s21));
		List<Expression> rest      = Util.rest(set2.getArguments());
		Expression s22UnionS2m     = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.UNION, rest);
		Expression result          = process.rewrite(R_set_diff, LPIUtil.argForSetDifferenceRewriteCall(s1DiffS21Result, s22UnionS2m));
		
		Justification.endEqualityStep(result);
		
		return result;
	}


	private Expression rewriteS1andS2ExtensionalUniSets(Expression set1, Expression set2, RewritingProcess process) {
		Trace.log("if S1 is {a1,...,an} and S2 is {b1,...,bm}");
		Trace.log("    return R_DifferenceOfExtensionalAndExtensionalSet({a1,...,an}, {b1,...,bm}, 1, 1)");

		Justification.beginEqualityStep("computing difference of extensional sets");
		Expression result = process.rewrite(R_DifferenceOfExtensionalAndExtensionalSet,
								LPIUtil.argForDifferenceOfExtensionalAndExtensionalSetRewriteCall(set1, set2, 0, 0));
		Justification.endEqualityStep(result);
		
		return result;
	}

	private Expression rewriteS1IntensionalS2ExtensionalUniSets(Expression set1, Expression set2, RewritingProcess process) {

		Trace.log("if S1 is { Alpha | C }_I and S2 is {b1,...,bm}");

		Expression result = null;
		if (ExtensionalSet.cardinality(set2) == 0) {
			Justification.beginEqualityStep("second set is empty, so result is simply the first set"); // * - closed at end
			result = set1;
		} 
		else {
			Trace.log("    { Alpha' | C' }_I' <- standardize { Alpha | C }_I apart from {b1,...,bm}");
			
			List<Expression> disjuncts = new ArrayList<Expression>();

			Expression saS1 = StandardizedApartFrom.standardizedApartFrom(set1, set2, process);

			Expression alphaPrime      = ((IntensionalSet) saS1).getHead();
			Expression cPrime          = ((IntensionalSet) saS1).getCondition();
			IndexExpressionsSet iPrime = ((IntensionalSet) saS1).getIndexExpressions();

			for (Expression b_i : ExtensionalSet.getElements(set2)) {
				Expression equality = Equality.make(alphaPrime, b_i);
				disjuncts.add(equality);
			}
			
			Justification.beginEqualityStep("difference is intensional set constrained so that its elements are not equal any element in " + set2);
			
			Trace.log("    C'' <- R_formula_implification(C' and not (Disjunction_i Alpha' = b_i)) with cont. variables extended by I'");
			RewritingProcess processIPrime = LPIUtil.extendContextualSymbolsWithIntensionalSetIndicesInferringDomainsFromUsageInRandomVariables(saS1, process);
			
			BranchRewriteTask[] disjunctRewriters = new BranchRewriteTask[disjuncts.size()];
			for (int i = 0; i < disjuncts.size(); i++) {
				disjunctRewriters[i] = new BranchRewriteTask(
						new RewriteOnBranch() {
							
							@Override
							public Expression rewrite(Expression[] expressions, RewritingProcess process) {
								Expression result = process.rewrite(R_formula_simplification, expressions[0]);
								return result;
							}
						},
						new Expression[] {disjuncts.get(i)});
 			}
			
			Expression disjunction = GrinderUtil.branchAndMergeOnADisjunction(disjunctRewriters, processIPrime);
			
			Expression cPrimePrime = null;
			if (disjunction.equals(Expressions.TRUE)) {
				Trace.log("// Shortcircuited, disjunct always true, therefore FALSE.");
				// C' and not or( ..., true, ...) is always false
				cPrimePrime = Expressions.FALSE;
				Justification.beginEqualityStep("last simplified condition is always true, so set condition is always false"); // * - closed at end
			} 
			else {
				Expression notDisjunction          = Not.make(disjunction);
				Expression cPrimeAndNotDisjunction = CardinalityUtil.makeAnd(cPrime, notDisjunction);
				Justification.beginEqualityStep("simplifying set condition"); // * - closed at end
				cPrimePrime = processIPrime.rewrite(R_formula_simplification, cPrimeAndNotDisjunction);
			}
			result = IntensionalSet.make(Sets.getLabel(saS1), iPrime, alphaPrime, cPrimePrime);
		}

		// * - this is closing three beginSteps above, marked with *
		Justification.endEqualityStep(result);

		Trace.log("    return R_basic({ Alpha' | C'' }_I')");
		
		Justification.beginEqualityStep("simplifying overall expression");
		result = process.rewrite(R_basic, result);
		Justification.endEqualityStep(result);

		return result;
	}

	private Expression rewriteS1andS2IntensionalUniSets(Expression set1, Expression set2, RewritingProcess process) {
		Expression result = null;
		
		Trace.log("if S1 is { Alpha | C }_I and S2 is { Alpha' | C' }_I'");
		
		Expression alpha      = ((IntensionalSet) set1).getHead();
		Expression alphaPrime = ((IntensionalSet) set2).getHead(); 
		
		// Perform a cheap disequality first
		if (CheapDisequalityModule.isACheapDisequality(alpha, alphaPrime, process)) {
			Justification.beginEqualityStep("is guaranteed Alpha != Alpha'");
			
			Trace.log("    is guaranteed Alpha != Alpha'");
			Trace.log("    return S1"); 
			
			result = set1;
			
			Justification.endEqualityStep(result);
		}
		else {			
			Expression          c = ((IntensionalSet) set1).getCondition();
			IndexExpressionsSet i = ((IntensionalSet) set1).getIndexExpressions();

			Trace.log("    { Alpha' | C' }_I' <- standardize { Alpha' | C' }_I' apart from (Alpha, C)");
			
			Expression tupleAlphaC = Tuple.make(alpha, c);
			Expression saS2        = StandardizedApartFrom.standardizedApartFrom(set2, tupleAlphaC, process);
			
			alphaPrime                 = ((IntensionalSet) saS2).getHead(); 
			Expression          cPrime = ((IntensionalSet) saS2).getCondition();
			IndexExpressionsSet iPrime = ((IntensionalSet) saS2).getIndexExpressions();
	
			Expression disequality      = Disequality.make(alpha, alphaPrime);
			Expression implication      = Implication.make(cPrime, disequality);
			Expression forAllIPrime     = ForAll.make(iPrime, implication);
			Expression cAndForAllPrimeI = CardinalityUtil.makeAnd(c, forAllIPrime);
	
			if (Justification.isEnabled()) {
				Justification.beginEqualityStep("difference set is an intensional set with the condition of the first and the negation of the condition of the second");
				Expression currentExpression = IntensionalSet.make(Sets.getLabel(set1), i, alpha, cAndForAllPrimeI);
				Justification.endEqualityStep(currentExpression);
			}
			
			Trace.log("    C''<- R_formula_simplification(C and for all I' : C' => Alpha != Alpha') with cont. variables extended by I");
			Justification.beginEqualityStep("simplifying set condition");
			
			// Note: As an optimization, instead of simplifying the overall conjunct as described in the
			// pseudo-code in a single shot, i.e:
			//    RewritingProcess processI = GrinderUtil.extendContextualSymbols(i, process);
			//    Expression    cPrimePrime = processI.rewrite(R_formula_simplification, cAndForAllPrimeI);
			// We will break it apart so that the more expensive quantified formula is resolved first.
			// However, we do this simplification under the assumption 'c' is true (this is the same
			// theoryWithEquality/logic used in ConjunctsHoldTrueForEachOther). A more constrained context can
			// help improve overall performance.
			RewritingProcess processI = GrinderUtil.extendContextualSymbolsAndConstraintWithIntensionalSet(set1, process);
			forAllIPrime              = processI.rewrite(R_normalize, forAllIPrime);
			cAndForAllPrimeI          = CardinalityUtil.makeAnd(c, forAllIPrime);
			processI                  = LPIUtil.extendContextualSymbolsWithIntensionalSetIndicesInferringDomainsFromUsageInRandomVariables(set1, process);
			Expression cPrimePrime    = processI.rewrite(R_formula_simplification, cAndForAllPrimeI);
			
			result = IntensionalSet.make(Sets.getLabel(set1), i, alpha, cPrimePrime);
			Justification.endEqualityStep(result);
			
			Trace.log("    return R_basic({ Alpha | C'' }_I)");
			Justification.beginEqualityStep("simplifying overall expression");
			result = process.rewrite(R_basic, result);
			Justification.endEqualityStep(result);
		}

		return result;
	}

	private Expression rewriteS1ExtensionalS2IntensionalUniSets(Expression set1, Expression set2, RewritingProcess process) {
		Trace.log("if S1 is {a1,...,an} and S2 is { Alpha | C }_I");
		Trace.log("     return R_DifferenceOfExtensionalAndIntensionalSet ({a1,...,an}, { Alpha | C }_I, 1)");

		Justification.beginEqualityStep("difference between extensional and intensional set");
		Expression result = process.rewrite(R_DifferenceOfExtensionalAndIntensionalSet,
								LPIUtil.argForDifferenceOfExtensionalAndIntensionalSetRewriteCall(set1, set2, 0));
		Justification.endEqualityStep(result);

		return result;
	}
	
	//
	private RewriteOnBranch newCallSetDifferenceRewrite() {
		return new RewriteOnBranch() {
			@Override
			public Expression rewrite(Expression[] expressions, RewritingProcess process) {
				Expression result = process.rewrite(R_set_diff, LPIUtil.argForSetDifferenceRewriteCall(expressions[0], expressions[1]));
				return result;
			}
		};
	}

	private static Expression getFirstOfUnion(Expression union) {
		Expression result = null;

		if (union.numberOfArguments() == 0) {
			// Handle the case 'union()'
			result = _emptySet;
		} 
		else {
			result = union.get(0);
		}

		return result;
	}

	private static Expression getRestOfUnion(Expression union) {
		Expression result = null;

		List<Expression> rest = union.getArguments();
		if (rest.size() > 0) {
			rest = Util.rest(union.getArguments());
		}
		switch (rest.size()) {
		case 0: // Handle the case 'union()'
			result = _emptySet;
			break;
		case 1: // Handle the case 'union({...})'
			result = union.get(1);
			break;
		default:
			result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.UNION, rest.toArray());
			break;
		}

		return result;
	}
}
