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
package com.sri.ai.praise.model;

import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.FunctionSignature;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.PredicateIterator;

/**
 * Utility class for retrieving random variables.
 * 
 * @author braz
 *
 */
@Beta
public class GetRandomVariables {

	public static List<Expression> getCataloguedRandomVariableValueExpressions(Expression expression, RewritingProcess process) {
		Iterator<Expression> subExpressionsIterator = new SubExpressionsDepthFirstIterator(expression);
		Iterator<Expression> randomVariableValueExpressionsIterator = new PredicateIterator<Expression>(subExpressionsIterator, new IsRandomVariableValueExpression(process));
		List<Expression> result = Util.listFrom(randomVariableValueExpressionsIterator);
		return result;
	}
	
	/**
	 * Obtains an iterator ranging over the random variable value expressions involved in a given expression.
	 */
	public static Iterator<FunctionSignature> determineRandomPredicates(
			Expression expression,
			RewritingProcess process) {
		
		Iterator<Expression> randomVariableValueExpressionIterator = getRandomVariableValueExpressionsIterator(
				expression, process);
		Iterator<FunctionSignature> result =
			new FunctionIterator<Expression, FunctionSignature>(randomVariableValueExpressionIterator, FunctionSignature.MAKER_FROM_EXPRESSION);
		return result;
	}

	public static Iterator<Expression> getRandomVariableValueExpressionsIterator(
			Expression expression,
			RewritingProcess process) {
		Predicate<Expression> isRandomVariableValueExpressionPredicate = new IsRandomVariableValueExpression(process);
		SubExpressionsDepthFirstIterator subExpressionsIterator = new SubExpressionsDepthFirstIterator(expression);
		subExpressionsIterator.setPruneChildrenPredicate(isRandomVariableValueExpressionPredicate);

		Iterator<Expression> randomVariableValueExpressionIterator = 
			new PredicateIterator<Expression>(subExpressionsIterator, isRandomVariableValueExpressionPredicate);
		// note that the iterator is being both filtered and pruned by the same UnaryPredicate.
		// The effect obtained is that only random variable value expressions are returned (filtering), and that their children are not examined (pruning).
		// The reason we prune children is because the functor is a subexpression of a random variable value expression.
		// For example, p(X) can be a random variable value expression, and p is its functor, and therefore subexpression.
		// If we didn't prune subexpressions, p would be identified as a random variable value expression as well, and we don't want that.
		// At first, it may seem like rejecting symbols would solve this problem, but we DO want p in "p and q" to be taken as a random variable value expression.
		return randomVariableValueExpressionIterator;
	}
	
	/**
	 * Obtains an iterator on the random variable value expressions involved in a given expression.
	 * Needs process containing a model in global objects.
	 */
	public static Iterator<Expression> getRandomVariableValueExpressionsIteratorFromModel(
			Expression expression,
			RewritingProcess process) {
		Iterator<Expression> result =
			new PredicateIterator<Expression>(
					new SubExpressionsDepthFirstIterator(expression),
					new IsRandomVariableValueExpression(process));
		return result;
	}

	/**
	 * Same as {@link #getRandomVariableValueExpressionsIteratorFromModel(Expression, RewritingProcess)},
	 * but returning a list instead.
	 */
	public static List<Expression> getRandomVariableValueExpressionsFromModel(
			Expression expression,
			RewritingProcess process) {
		return Util.listFrom(getRandomVariableValueExpressionsIteratorFromModel(expression, process));
	}
}
