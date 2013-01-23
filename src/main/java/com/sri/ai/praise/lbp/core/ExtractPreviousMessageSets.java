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
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.CommutativeAssociative;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.ScopedVariables;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.util.Util;

/**
 * Default implementation of {@link LBPRewriter#R_extract_previous_msg_sets}.
 * 
 * @author oreilly
 * 
 */
@Beta
public class ExtractPreviousMessageSets extends AbstractLBPHierarchicalRewriter implements LBPRewriter {	
	public ExtractPreviousMessageSets() {
		
	}
	
	@Override
	public String getName() {
		return R_extract_previous_msg_sets;
	}
	
	
	/**
	 * @see LBPRewriter#R_extract_previous_msg_sets
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression expressionE = null;
		Expression scopingVariables = null;
		
		// Assert input arguments
		// It can be either
		// a tuple of the form: (E, tuple(I1, ..., In))
		// where E is expected to be a basic expression and I1, ..., In are scoping variables
		// or E alone
		// We may want to enforce the two-argument form down the line.
		if (Tuple.isTuple(expression) && Tuple.size(expression) == 2) {
			expressionE      = Tuple.get(expression, 0);
			scopingVariables = Tuple.get(expression, 1);
		}
		else {
			expressionE = expression;
			List<Expression> elements = Collections.emptyList();
			scopingVariables          = Tuple.make(elements);
		}		
		
		Expression result = null;

		List<Expression> extractedPreviousMessages = new LinkedList<Expression>();
			
		extractPreviousMessages(extractedPreviousMessages, expressionE, scopingVariables.getArguments(), process.getContextualConstraint(), process);	

		result = CommutativeAssociative.make(FunctorConstants.UNION, extractedPreviousMessages, Sets.EMPTY_SET);
		
		return result;
	}
	
	//
	// PRIVATE METHODS
	//
	private void extractPreviousMessages(List<Expression> extractedPreviousMessages, Expression expressionE, List<Expression> scopingVariables, Expression constrainingCondition, RewritingProcess process) {
		if (LPIUtil.isPreviousMessage(expressionE)) {
			Expression tuplePair         = Tuple.make(Arrays.asList(expressionE.get(0), expressionE.get(1)));
			Expression scopingExpression = IntensionalSet.makeScopingExpression(scopingVariables);
			extractedPreviousMessages.add(IntensionalSet.makeMultiSet(scopingExpression, tuplePair, constrainingCondition));
		} 
		else {		
			List<Expression> newTotalScopingVariables = new LinkedList<Expression>(scopingVariables);
			List<Expression> scopedVariablesInE = ScopedVariables.get(expressionE, process);
			newTotalScopingVariables.addAll(scopedVariablesInE);
			Util.removeElementsSatisfying(newTotalScopingVariables, new LPIUtil.IsRandomVariableValueExpression(process));
			// the above is needed because we may have lambda expressions scoped by random variable values

			Iterator<ExpressionAndContext> subExpressionAndContextsIterator = expressionE.getImmediateSubExpressionsAndContextsIterator(process);
			while (subExpressionAndContextsIterator.hasNext()) {
				ExpressionAndContext subExpressionAndContext     = subExpressionAndContextsIterator.next();
				Expression subExpressionE                        = subExpressionAndContext.getExpression();
				Expression subExpressionEConstrainingCondition   = subExpressionAndContext.getConstrainingCondition();
				Expression extentendedConstrainingCondition      = constrainingCondition;
				if (!subExpressionEConstrainingCondition.equals(Expressions.TRUE) 
					&&
					CardinalityUtil.isFormula(subExpressionEConstrainingCondition, process)) {
					extentendedConstrainingCondition = CardinalityUtil.makeAnd(constrainingCondition, subExpressionEConstrainingCondition);
				}

				extractPreviousMessages(extractedPreviousMessages, subExpressionE, newTotalScopingVariables, extentendedConstrainingCondition, process);
			}
		}
	}
}
