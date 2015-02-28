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

import java.util.Arrays;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.DefaultIntensionalUniSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasKind;
import com.sri.ai.grinder.core.HasNumberOfArguments;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.helper.PickSingleElement;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.Cardinality;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPRewriter;

/**
 * <pre>
 * R_lift_product_of_factor_to_variable(prod_{{(on I) Alpha | C}})
 * An atomic rewriter to be used within the context of R_normalize that rewrites/lifts expressions of the form:
 * product({{(on I) Alpha | C }})
 * Whereby Alpha is a representative message from a factor to the variable.
 * Rewriting only occurs if Alpha is a conditional numeric expression or if Alpha does not depend on the indices. 
 * (that means Alpha may be dependent on I, but the if then else leaves are constant in I -- only the conditions depend on I)
 * Returns an equivalent numeric expression that does not depend on I, computed in time independent on the number of values of the type of I
 * or the input argument if it cannot be lifted.
 * Cases:
 * Alpha is if C' then Alpha_1 else Alpha_2
 *     return R_lift_product_of_factor_to_variable(prod_{{(on I) Alpha_1 | C and C'}}) * R_lift_product_of_factor_to_variable(prod_{{(on I) Alpha_2 | C and not C'})
 * if R_complete_normalize({(on I) true | C}) is empty set
 *     return 1
 * if Alpha <- pick_single_element({(on I) Alpha | C}) succeed
 *     return Alpha ^ {@link Cardinality R_card}(| C |_I)
 * otherwise
 *     return input expression
 * </pre>
 * 
 * @author oreilly
 *
 */
@Beta
public class LiftProductOfFactorToVariable extends AbstractRewriter {
	
	/**
	 * A key for the rewriting process global objects map, that when set, throws
	 * an exception when it is not possible to lift the given set. This is
	 * useful when we get to stages of algorithms in which we know that only
	 * direct message values will be used (as opposed to things like previous
	 * message expressions in the context of loopy BP).
	 */
	public static final String GLOBAL_KEY_LIFT_PRODUCT_OF_FACTOR_TO_VARIABLE_MUST_ALWAYS_LIFT = "LiftProductOfFactorToVariable must always lift";

	public LiftProductOfFactorToVariable() {
		this.setReifiedTests(new HasKind(FunctorConstants.PRODUCT),
	             new HasNumberOfArguments(1));
	}
	
	/**
	 * Determine if an exception should be thrown if it is not possible to lift
	 * a given set.
	 * 
	 * @param process
	 *            the current rewriting process.
	 * @return true if an exception should be thrown if it is not possible to
	 *         lift a given set, false otherwise.
	 */
	public static boolean isMustAlwaysLift(RewritingProcess process) {
		boolean result = false; // By default we assume we must not always lift
		
		Boolean mustAlwaysLift = (Boolean) process.getGlobalObject(GLOBAL_KEY_LIFT_PRODUCT_OF_FACTOR_TO_VARIABLE_MUST_ALWAYS_LIFT);
		if (mustAlwaysLift != null) {
			// Use the value set on the global process.
			result = mustAlwaysLift;
		}
		
		return result;
	}
	
	/**
	 * Set whether an exception should be thrown if it is not possible to lift a
	 * given set.
	 * 
	 * @param mustAlwaysLift
	 *            true if an exception should be thrown, false otherwise.
	 * @param process
	 *            the current rewriting process.
	 */
	public static void setMustAlwaysLift(boolean mustAlwaysLift, RewritingProcess process) {
		process.getGlobalObjects().put(GLOBAL_KEY_LIFT_PRODUCT_OF_FACTOR_TO_VARIABLE_MUST_ALWAYS_LIFT, mustAlwaysLift);
	}
	
	//
	// START-Rewriter
	@Override
	public String getName() {
		return "R_lift_product_of_factor_to_variable";
	}
	
	// END-Rewriter
	//
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = expression;
		if (LPIUtil.isProductExpression(expression)) {
			Expression prodSet    = expression.get(0);
			List<Expression> onI  = ((ExtensionalIndexExpressionsSet) ((IntensionalSet) prodSet).getIndexExpressions()).getList();
			Expression alpha      = ((IntensionalSet) prodSet).getHead();
			Expression conditionC = ((IntensionalSet) prodSet).getCondition();
			
			RewritingProcess processExtendedByProductSet = GrinderUtil.extendContextualSymbolsAndConstraintWithIntensionalSet(prodSet, process);
			
			// the following logic depends on the head being normalized.
			alpha = processExtendedByProductSet.rewrite(LBPRewriter.R_normalize, alpha);
			
			if (isARepresentativeMessageFromAFactorToTheVariable(alpha, processExtendedByProductSet)) {
				// Alpha is if C' then Alpha_1 else Alpha_2
				if (IfThenElse.isIfThenElse(alpha)) {
					Expression conditionCPrime = IfThenElse.getCondition(alpha);
					if (FormulaUtil.isFormula(conditionCPrime, processExtendedByProductSet)) {
						// return R_lift_product(prod_{{(on I) Alpha_1 | C and C'}}) * R_lift_product(prod_{{(on I) Alpha_2 | C and not C'})
						Expression alpha1        = IfThenElse.getThenBranch(alpha);
						Expression thenCondition = And.make(conditionC, conditionCPrime);
						Expression thenSet       = ((IntensionalSet)prodSet).setHeadAndCondition(alpha1, thenCondition);
						Expression thenProduct   = Expressions.apply(FunctorConstants.PRODUCT, thenSet);
		
						Expression alpha2        = IfThenElse.getElseBranch(alpha);
						Expression elseCondition = And.make(conditionC, Not.make(conditionCPrime));
						Expression elseSet       = ((IntensionalSet)prodSet).setHeadAndCondition(alpha2, elseCondition);
						Expression elseProduct   = Expressions.apply(FunctorConstants.PRODUCT, elseSet);
						
						Expression thenResult = rewrite(thenProduct, process);
						Expression elseResult = rewrite(elseProduct, process);
						Expression times      = Times.make(Arrays.asList(thenResult, elseResult));
						
						result = times;
					}
				} 
				
				// Alpha isn't if C' then Alpha_1 else Alpha_2, where C' is a formula depending on I'
				if (result == expression) { 
					// if R_complete_normalize({(on I) true | C}) is empty set
					Expression isEmptySet = process.rewrite(LBPRewriter.R_complete_normalize, new DefaultIntensionalUniSet(onI, Expressions.TRUE, conditionC));
					if (Sets.isEmptySet(isEmptySet)) {
						// then return 1
						result = Expressions.ONE;
					}
					else {
						// if Alpha <- pick_single_element({(on I) Alpha | C}) succeeds
						Expression singletonIntensionalSet = new DefaultIntensionalUniSet(onI, alpha, conditionC);
						Expression singleAlpha             = PickSingleElement.pickSingleElement(singletonIntensionalSet, process);				
						if (singleAlpha != null) {
							// return alpha ^ {@link Cardinality R_card}(| C |_I)
							IndexExpressionsSet indexExpressions = ((IntensionalSet) prodSet).getIndexExpressions();
							List<Expression> indexExpressionsList = ((ExtensionalIndexExpressionsSet) indexExpressions).getList();
							Expression cardinalityOfIndexedFormula = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(conditionC, indexExpressionsList.toArray(new Expression[indexExpressionsList.size()]));
							Justification.beginEqualityStep("cardinality of equality boolean formula");
							Justification.log(cardinalityOfIndexedFormula);
							Expression cardinalityResult = process.rewrite(CardinalityRewriter.R_card, cardinalityOfIndexedFormula);
							Justification.endEqualityStep(cardinalityResult);
							Expression exponentiation = Expressions.apply(FunctorConstants.EXPONENTIATION, singleAlpha, cardinalityResult);
							result = exponentiation;
						}
						else if (isMustAlwaysLift(process)) {
							Trace.log("Unable to lift {} constrained by {} ", expression, process.getContextualConstraint());
							throw new IllegalStateException("Unable to lift " + expression + " constrained by "+process.getContextualConstraint());
						}
					}
				}
			}
			
			// otherwise leave as is
			// return input expression
		} 
		
		return result;
	}
	
	//
	// PRIVATE METHODS
	//
	private boolean isARepresentativeMessageFromAFactorToTheVariable(Expression alpha, RewritingProcess process) {
		boolean result = false;
		
		if (IfThenElse.isIfThenElse(alpha)) {
			if (LPIUtil.isMessageValue(alpha, process)) {
				result = true;
			}
			else {
				// Conditional
				result = isARepresentativeMessageFromAFactorToTheVariable(IfThenElse.getThenBranch(alpha), process) 
						 &&
						 isARepresentativeMessageFromAFactorToTheVariable(IfThenElse.getElseBranch(alpha), process);
			}
		}
		else {
			result = LPIUtil.isMessageValue(alpha, process);
		}
		
		return result;
	}
}
