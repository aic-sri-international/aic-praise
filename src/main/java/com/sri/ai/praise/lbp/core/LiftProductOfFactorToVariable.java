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
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.Variables;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.praise.LPIUtil;

/**
 * <pre>
 * R_lift_product_of_factor_to_variable(prod_{{(on I) Alpha | C}})
 * An atomic rewriter to be used within the context of R_simplify that rewrites/lifts expressions of the form:
 * product({{(on I) Alpha | C }})
 * Whereby Alpha is a representative message from a factor to the variable.
 * Rewriting only occurs if Alpha is a conditional numeric expression or if Alpha does not depend on the indices. 
 * (that means Alpha may be dependent on I, but the if then else leaves are constant in I -- only the conditions depend on I)
 * Returns an equivalent numeric expression that does not depend on I, computed in time independent on the number of values of the domain of I
 * or the input argument if it cannot be lifted.
 * Cases:
 * Alpha is if C' then Alpha_1 else Alpha_2
 *     where C' is a formula depending on I
 *     return R_lift_product_of_factor_to_variable(prod_{{(on I) Alpha_1 | C and C'}}) * R_lift_product_of_factor_to_variable(prod_{{(on I) Alpha_2 | C and not C'})
 * if Alpha <- pick_single_element({{(on I) Alpha | C}}) succeed
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
	
	public LiftProductOfFactorToVariable() {
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
			Expression onI        = IntensionalSet.getScopingExpression(prodSet);
			Expression alpha      = IntensionalSet.getHead(prodSet);
			Expression conditionC = IntensionalSet.getCondition(prodSet);
			
			if (isARepresentativeMessageFromAFactorToTheVariable(alpha, process)) {
				// Alpha is if C' then Alpha_1 else Alpha_2
				if (IfThenElse.isIfThenElse(alpha)) {
					Expression conditionCPrime = IfThenElse.getCondition(alpha);
					// where C' is a formula depending on I 
					if (CardinalityUtil.isFormula(conditionCPrime, process) 
						&&
						dependsOnTheIndices(conditionCPrime, onI, process)
						) {
						// return R_lift_product(prod_{{(on I) Alpha_1 | C and C'}}) * R_lift_product(prod_{{(on I) Alpha_2 | C and not C'})
						Expression alpha1        = IfThenElse.getThenBranch(alpha);
						Expression thenCondition = And.make(conditionC, conditionCPrime);
						Expression thenSet       = IntensionalSet.copyWithNewHeadAndCondition(prodSet, alpha1, thenCondition);
						Expression thenProduct   = Expressions.apply(FunctorConstants.PRODUCT, thenSet);
		
						Expression alpha2        = IfThenElse.getElseBranch(alpha);
						Expression elseCondition = And.make(conditionC, Not.make(conditionCPrime));
						Expression elseSet       = IntensionalSet.copyWithNewHeadAndCondition(prodSet, alpha2, elseCondition);
						Expression elseProduct   = Expressions.apply(FunctorConstants.PRODUCT, elseSet);
						
						Expression thenResult = rewrite(thenProduct, process);
						Expression elseResult = rewrite(elseProduct, process);
						Expression times      = Times.make(Arrays.asList(thenResult, elseResult));
						
						result = times;
					}
				} 
				
				// if is not 'Alpha is if C' then Alpha_1 else Alpha_2 where C' is a formula depending on I'
				if (result == expression) { 
					// if Alpha <- pick_single_element({{(on I) Alpha | C}}) succeeds 			
					// Note: pick_single_element expects uni-intensional sets.
					Expression singletonIntensionalSet = IntensionalSet.makeUniSet(onI, alpha, conditionC);
					Expression singleAlpha             = LPIUtil.pickSingleElement(singletonIntensionalSet, process);				
					if (singleAlpha != null) {
						// return alpha ^ {@link Cardinality R_card}(| C |_I)
						List<Expression> setIndices = new ArrayList<Expression>(IntensionalSet.getIndices(prodSet));
						Expression cardinalityOfIndexedFormula = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(conditionC, setIndices.toArray(new Expression[setIndices.size()]));
						Justification.beginEqualityStep("cardinality of equality boolean formula");
						Justification.log(cardinalityOfIndexedFormula);
						Expression cardinality    = process.rewrite(CardinalityRewriter.R_card, cardinalityOfIndexedFormula);
						Justification.endEqualityStep(cardinality);
						Expression exponentiation = Expressions.apply(FunctorConstants.EXPONENTIATION, singleAlpha, cardinality);
						result = exponentiation;
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
	
	private boolean dependsOnTheIndices(Expression expression, Expression onI, RewritingProcess process) {
		boolean result = false;
		
		Set<Expression> expressionVars = Variables.freeVariables(expression, process);
		Set<Expression> onIVars        = Variables.freeVariables(onI, process);
		
		// If the set reduces in size, this indicates
		// expression is dependent on the indices
		int expressionVarsInitialSize = expressionVars.size();
		expressionVars.removeAll(onIVars);
		if (expressionVars.size() != expressionVarsInitialSize) {
			result = true;
		}
		
		return result;
	}
}
