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
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.StandardizedApartFrom;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPRewriter;

/**
 * Default implementation of {@link LBPRewriter#R_neigh_v_parf}.
 * 
 * @author oreilly
 * 
 */
@Beta
public class NeighborsOfRandomVariableInParfactor extends AbstractLBPHierarchicalRewriter implements LBPRewriter {
	
	public NeighborsOfRandomVariableInParfactor() {
	}
	
	@Override
	public String getName() {
		return R_neigh_v_parf;
	}
	
	/**
	 * @see LBPRewriter#R_neigh_v_parf
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		
		// Assert input arguments
		// a tuple of the form: ([ Ev ], PF).
		if (!Tuple.isTuple(expression) || Tuple.size(expression) != 2) {
			throw new IllegalArgumentException("Invalid input argument expression:"+expression);
		}
		
		Expression randomVariable = Tuple.get(expression, 0);
		Expression parfactor      = Tuple.get(expression, 1);
		
		Expression result = null;
		
		Expression currentExpression = null;
		if (Justification.isEnabled()) {
			currentExpression = Expressions.apply(LPIUtil.FUNCTOR_NEIGHBOR, randomVariable, parfactor);
			Justification.log(currentExpression);
		}
		
		LPIUtil.assertRandomVariableOk(randomVariable, process);

		if (ExtensionalSet.isExtensionalSet(parfactor)) {
			Trace.log("if PF is { [Ef_1], ..., [Ef_n]  } where [Ef_i] are factor expressions");
			Trace.log("    Let Fi be the expression");
			Trace.log("    if rv_is_referenced_by([Ev], Ef_i)");
			Trace.log("        then { [ Ef_i ] }");
			Trace.log("        else {}");
			Trace.log("    return R_basic(F1 union ... union F_n)");

			List<Expression> unionArguments = new ArrayList<Expression>(ExtensionalSet.getElements(parfactor).size());
			for (Expression factor : ExtensionalSet.getElements(parfactor)) {
				Expression factorValue  = LPIUtil.getFactorValueExpression(factor, process);
				Expression referencedBy = Expressions.apply("referenced by", randomVariable, factorValue);
				Expression fI =
					IfThenElse.make(
							referencedBy,
							ExtensionalSet.makeSingletonOfSameTypeAs(parfactor, factor),
							ExtensionalSet.makeEmptySetExpression());
				unionArguments.add(fI);
				// I am not conditioning the creation of expressions above to RewriterJustification.isEnabled()
				// because some of its subexpressions are useful below, even without justifications.
			}
			
			Expression union = Expressions.apply(FunctorConstants.UNION, unionArguments);

			Justification.beginEqualityStep("each factor is a neighbor only if random variable occurs in it");
			Justification.endEqualityStep(union);

			int argumentIndex = 0;
			for (Expression fI : union.getArguments()) {
				Expression factorValue = IfThenElse.getCondition(fI).get(1);
				
				Justification.beginEqualityStep("by determining occurrence conditions");
				Expression rvIsReferencedBy = 
						LPIUtil.randomVariableIsReferencedByExpression(randomVariable, factorValue, process);
				union = union.set(argumentIndex, IfThenElse.copyWithReplacedCondition(fI, rvIsReferencedBy));
				Justification.endEqualityStep(union);

				argumentIndex++;
			}
			
			Justification.beginEqualityStep("by simplifying");
			result = process.rewrite(R_basic, union);
			Justification.endEqualityStep(result);

		} 
		else if (Sets.isIntensionalSet(parfactor)) {
			Trace.log("if PF is an intensionally defined set {{ [ Ef ] | C }}_I) (where [ Ef ] is a factor expression)");
			Trace.log("    {{ [ Ef' ] | C' }}_I' <- standardize {{ [ Ef ] | C }}_I apart from [ Ev ]");

			Justification.beginEqualityStep("standardizing apart");
			Expression saParfactor = StandardizedApartFrom.standardizedApartFrom(parfactor, randomVariable, process);
			if (Justification.isEnabled()) {
				currentExpression = currentExpression.set(1, saParfactor);
				Justification.endEqualityStep(currentExpression);
			}
			
			Expression factorPrime            = ((IntensionalSet) saParfactor).getHead();
			Expression factorValuePrime       = LPIUtil.getFactorValueExpression(factorPrime, process);
			Expression conditionPrime         = ((IntensionalSet) saParfactor).getCondition();
			IndexExpressionsSet indexExpressionsPrime = ((IntensionalSet) saParfactor).getIndexExpressions();
			
			Trace.log("    Extend contextual symbols with I'");
			RewritingProcess processIPrime = LPIUtil.extendContextualSymbolsWithIntensionalSetIndicesInferringDomainsFromUsageInRandomVariables(saParfactor, process);
			
			Trace.log("    return R_intensional_simplification(");
			Trace.log("                    {{ [ Ef' ] | R_formula_simplification(C' and rv_is_referenced_by([Ev], Ef')) }}_I')");
			
			if (Justification.isEnabled()) {
				Justification.beginEqualityStep("set of neighbors is set of factors restricted to those in which the random variable occurs");
				currentExpression = IntensionalSet.make(
						Sets.getLabel(parfactor),
						indexExpressionsPrime,
						factorPrime,
						CardinalityUtil.makeAnd(
								conditionPrime,
								Expressions.apply("referenced by", randomVariable, factorValuePrime)));
				Justification.endEqualityStep(currentExpression);
			}

			Justification.beginEqualityStep("by computing conditions for occurrence");
			Expression conditionForBeingReferencedBy = LPIUtil.randomVariableIsReferencedByExpression(randomVariable, factorValuePrime, processIPrime);
			Expression finalUnsimplifiedCondition    = And.make(conditionPrime, conditionForBeingReferencedBy);
			if (Justification.isEnabled()) {
				currentExpression = IntensionalSet.make(Sets.getLabel(parfactor), indexExpressionsPrime, factorPrime, finalUnsimplifiedCondition);
			}
			Justification.endEqualityStep(currentExpression);
			
			Justification.beginEqualityStep("by simplifying condition");
			Expression simplified = processIPrime.rewrite(R_formula_simplification, finalUnsimplifiedCondition);
			currentExpression     = IntensionalSet.make(Sets.getLabel(parfactor), indexExpressionsPrime, factorPrime, simplified);
			// the above line is needed even without justifications because it is used below.
			Justification.endEqualityStep(currentExpression);

			Justification.beginEqualityStep("by simplifying intensional set");
			// Ensure is not simplified to an extensional or conditional on creation.
			if (Sets.isIntensionalSet(currentExpression)) {
				result = processIPrime.rewrite(R_intensional_simplification, currentExpression);
			} 
			else {
				result = currentExpression;
			}
			Justification.endEqualityStep(result);
			
		} 
		else {
			throw new IllegalArgumentException("Illegal parfactor (i.e. is not a set of factors):=" + parfactor);
		}

		return result;
	}
}
