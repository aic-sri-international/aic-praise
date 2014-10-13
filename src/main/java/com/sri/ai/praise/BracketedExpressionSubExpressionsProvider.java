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
package com.sri.ai.praise;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.sri.ai.expresso.api.BracketedExpression;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.core.DefaultLambdaExpression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.NoOpRewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.library.equality.CheapDisequalityModule;
import com.sri.ai.grinder.library.function.InjectiveModule;
import com.sri.ai.grinder.library.function.MutuallyExclusiveCoDomainsModule;
import com.sri.ai.praise.model.IsRandomVariableValueExpression;

/**
 * 
 * @author braz
 *
 */
@Beta
public class BracketedExpressionSubExpressionsProvider extends AbstractRewriter
implements
NoOpRewriter,
CheapDisequalityModule.Provider,
InjectiveModule.Provider,
MutuallyExclusiveCoDomainsModule.Provider {

	public static final String SYNTAX_TREE_LABEL = "[ . ]";
	//
	private Cache<Expression, Expression> injectiveFunctionTokenCache = CacheBuilder.newBuilder().maximumSize(100).build();

	public static boolean isBracketedExpression(Expression expression) {
		return expression.getSyntaxTree().getLabel().equals(SYNTAX_TREE_LABEL);
	}
	
	public static Expression make(Expression valueExpression) {
		return Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(BracketedExpressionSubExpressionsProvider.SYNTAX_TREE_LABEL, valueExpression);
	}

	@Override
	public void rewritingProcessInitiated(RewritingProcess process) {
		CheapDisequalityModule.register(this, process);
		InjectiveModule.register(this, process);
		MutuallyExclusiveCoDomainsModule.register(this, process);
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		// Note: is a NoOpRewriter
		return expression; // will be removed eventually, not a real rewriter, just a module.
	}

	@Override
	public boolean haveMutuallyExclusiveCoDomains(Expression expression1,
			Expression expression2, RewritingProcess process) {
		if (isBracketedExpression(expression1) && isBracketedExpression(expression2) /* &&
				isRandomVariable(expression1, process) &&
				isRandomVariable(expression2, process) */) {
			
			Object functionToken1 = getInjectiveFunctionToken(expression1, process);
			Object functionToken2 = getInjectiveFunctionToken(expression2, process);
			return ! functionToken1.equals(functionToken2);
		}
		return false;
	}

	public static boolean isRandomVariable(Expression expression, RewritingProcess process) {
		return isBracketedExpression(expression) && IsRandomVariableValueExpression.apply(((BracketedExpression) expression).getInnerExpression(), process);
	}
	
	@Override
	public boolean isCheapDisequality(Expression e1, Expression e2, RewritingProcess process) {
		boolean result = false;
		
		if (isBracketedExpression(e1) && isBracketedExpression(e2)) {
			if (!getInjectiveFunctionToken(e1, process).equals(getInjectiveFunctionToken(e2, process))) {
				// if e1 and e2's injective function tokens are different we are guaranteed
				// that they are not equal. 
				result = true;
			}
		}
		
		return result;
	}
	
	
	
// TODO - currently does not handle bracketed expressions containing quantified sub-expressions.
	// Note: The injective function token for a bracketed expression consists of constructing a lambda
	// expression, where a normalized parameter is created per sub-expression position in the original
	// bracketed expression. Sub-expressions of a bracketed expression are the constants and logical
	// variables contained in random variable value expressions within the bracketed expression.
	// Theses will be substituted with the normalized parameters when constructing the
	// lambda injective function token.
	@Override
	public Object getInjectiveFunctionToken(Expression expression, RewritingProcess process) {
		Expression result = null;
		if (isBracketedExpression(expression)) {
			// Check if we have already calculated first
			result = injectiveFunctionTokenCache.getIfPresent(expression);
			if (result == null) {
				// Collect up the sub-expressions to be parameterized and their corresponding paths.
				// (i.e. constants and logical variables used inside of random variable predicates).		
				Iterator<ExpressionAndContext> subExpressionsIterator = expression.getImmediateSubExpressionsAndContextsIterator();
				int i = 1;
				List<Expression> parameters = new ArrayList<Expression>();
				Expression       lambdaBody = expression;
				while (subExpressionsIterator.hasNext()) {
					ExpressionAndContext subExpressionAndContext = subExpressionsIterator.next();
					
					// Note: By using paths for the replacement call, 
					// we don't need to worry about having to ensure there is not a 
					// clash between the normalized parameter names to be used (i.e. X1 to Xn) 
					// and the existing parameters, as each instance of an existing parameter (even if repeated)
					// is replaced with a new unique normalized parameter.
					Expression parameter = Expressions.makeSymbol("X"+i++);
					parameters.add(parameter);
					Expression result1 = subExpressionAndContext.getAddress().replace(lambdaBody, parameter);
					lambdaBody = result1; 
				}
				
				// Create the token
				result = new DefaultLambdaExpression(parameters, lambdaBody);
				
				injectiveFunctionTokenCache.put(expression, result);
			}
		}
		return result;
	}
	
	public static class GetRandomVariableValueExpression implements Function<SyntaxTree, SyntaxTree> {
		@Override
		public SyntaxTree apply(SyntaxTree syntaxTree) {
			return syntaxTree.getSubTree(0); // does need to be sub tree
		}
	}
}
