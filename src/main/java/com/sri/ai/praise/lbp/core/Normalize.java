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
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.ScopedVariables;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.controlflow.IfThenElseIrrelevantCondition;
import com.sri.ai.grinder.library.controlflow.IfThenElseSubExpressionsAndImposedConditionsProvider;
import com.sri.ai.grinder.library.equality.injective.DisequalityOnInjectiveSubExpressions;
import com.sri.ai.grinder.library.equality.injective.DisequalityOnMutuallyExclusiveCoDomainExpressions;
import com.sri.ai.grinder.library.equality.injective.EqualityOnInjectiveSubExpressions;
import com.sri.ai.grinder.library.equality.injective.EqualityOnMutuallyExclusiveCoDomainExpressions;
import com.sri.ai.grinder.library.function.InjectiveModule;
import com.sri.ai.grinder.library.function.MutuallyExclusiveCoDomainsModule;
import com.sri.ai.grinder.library.lambda.Lambda;
import com.sri.ai.grinder.library.lambda.LambdaApplication;
import com.sri.ai.grinder.library.lambda.LambdaApplication.PerformApplication;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.set.extensional.ProductOnExtensionalSet;
import com.sri.ai.grinder.library.set.extensional.UnionOnExtensionalSets;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.BracketedExpressionSubExpressionsProvider;
import com.sri.ai.praise.BreakConditionsContainingBothLogicalAndRandomVariables;
import com.sri.ai.praise.BreakConditionsContainingBothLogicalAndRandomVariablesHierarchical;
import com.sri.ai.praise.CardinalityOfTypeAlwaysDistinctFromZero;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.MoveAllRandomVariableValueExpressionConditionsDownHierarchical;
import com.sri.ai.praise.MoveRandomVariableValueExpressionConditionDown;
import com.sri.ai.praise.Type;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.util.base.Pair;

/**
 * @see LBPRewriter#R_normalize
 * 
 * @author oreilly
 *
 */
@Beta
public class Normalize extends com.sri.ai.grinder.library.equality.cardinality.direct.core.Normalize implements LBPRewriter {

	@Override
	public String getName() {
		return LBPRewriter.R_normalize;
	}
	
	private Rewriter breakConditionsContainingBothLogicalAndRandomVariablesHierarchical = new BreakConditionsContainingBothLogicalAndRandomVariablesHierarchical();
	private Rewriter moveAllRandomVariableValueExpressionConditionsDownHierarchical = new MoveAllRandomVariableValueExpressionConditionsDownHierarchical();
	private Rewriter simplify = new Simplify();
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		// Note that the order used below is far from arbitrary.
		// MoveAllRandomVariableValueExpressionConditionsDownHierarchical requires
		// its input to have already all conditional expressions on top, which is enforced by
		// IfThenElseExternalizationHierarchical.
		expression = simplify.rewrite(expression, process); // this first pass rewrites prod({{ <message value> | C }}) into exponentiated message values through lifting, rending a basic expression
		// it should be replaced by a normalizer with the single goal of lifting such expressions
		expression = breakConditionsContainingBothLogicalAndRandomVariablesHierarchical.rewrite(expression, process);
		expression = ifThenElseExternalizationHierarchical.rewrite(expression, process);
		expression = moveAllRandomVariableValueExpressionConditionsDownHierarchical.rewrite(expression, process);
		expression = simplify.rewrite(expression, process);
		return expression;
	}

	//
	// PROTECTED METHODS
	// 
	@Override
	protected List<Rewriter> getAtomicRewriters() {
		List<Rewriter> atomicRewriters = super.getAtomicRewriters();
		
		atomicRewriters = extendAtomicRewriters(atomicRewriters);
	
		return atomicRewriters;
	}
		
	@SuppressWarnings("unchecked")
	protected static List<Rewriter> extendAtomicRewriters(List<Rewriter> atomicRewriters) {
		atomicRewriters = addRewritersBefore(atomicRewriters,
				//
				// Support for: Bracketed Expressions
				new Pair<Class<?>, Rewriter>(
						IfThenElseSubExpressionsAndImposedConditionsProvider.class,
						new BracketedExpressionSubExpressionsProvider()),			
				//
				// Support for: type handling
				// e.g.:
				// type(p(X)) -> {{false, true}}
				new Pair<Class<?>, Rewriter>(
						Plus.class, 
						new Type()),
				// Support for:
				// | type(.) | > 0 -> true
				// | type(.) | = 0 -> false
				// if flag set.
				new Pair<Class<?>, Rewriter>(
						Type.class, 
						new CardinalityOfTypeAlwaysDistinctFromZero()),
				//
				// Support for: Injective functions
				// e.g.:
				// [p(a)] = [p(b)] -> a = b -> false 
				// [p(X)] = [p(Y)] -> X = Y 
				// (1, 2) = (1, 3) -> 1 = 1 and 2 = 3 -> false 
				new Pair<Class<?>, Rewriter>(
						IfThenElse.class,
						new EqualityOnInjectiveSubExpressions()),
				new Pair<Class<?>, Rewriter>(
						IfThenElse.class,
						new DisequalityOnInjectiveSubExpressions()),
				new Pair<Class<?>, Rewriter>(
						IfThenElseSubExpressionsAndImposedConditionsProvider.class,
						new InjectiveModule()),
				//
				// Support for: Mutually exclusive Co-domains
				// e.g.:
				// [ p(a, X) ] = [ q(Y, b) ] -> false 
				// (X, Y) = (1, 2, 3)        -> false 
				new Pair<Class<?>, Rewriter>(
						ScopedVariables.class,
						new EqualityOnMutuallyExclusiveCoDomainExpressions()),
				new Pair<Class<?>, Rewriter>(
						ScopedVariables.class,
						new DisequalityOnMutuallyExclusiveCoDomainExpressions()),
				new Pair<Class<?>, Rewriter>(
						IfThenElseSubExpressionsAndImposedConditionsProvider.class,
						new MutuallyExclusiveCoDomainsModule()),
				//
				// Support for: Tuples (injective and mutually exclusive co-domains).
				new Pair<Class<?>, Rewriter>(
						IfThenElseSubExpressionsAndImposedConditionsProvider.class,
						new Tuple()),
				//
				// Support for: Union on Extensional Sets
				// e.g.:
				// {x} union {y} -> {x, y}
				new Pair<Class<?>, Rewriter>(
						Equality.class,
						new UnionOnExtensionalSets()),
				// 
				// Support for: Splitting conditionals on random variables 
				// e.g.:
				// 'if X != a and p(X) then E1 else E2' 
				// ->
				// 'if X != a the if p(X) then E1 else E2 else E2'
				//
				// 'if p(X) then if X = a then Alpha else Beta else Gamma' 
				// ->
				// 'if X = a then if p(X) then Alpha else Gamma else if p(X) then Beta else Gamma' 
						
//						new Pair<Class<?>, Rewriter>(IfThenElseIrrelevantCondition.class,
//								new MoveAllConditionsOnRandomVariablesDown()),	 

						new Pair<Class<?>, Rewriter>(IfThenElseIrrelevantCondition.class,
								new BreakConditionsContainingBothLogicalAndRandomVariables()),	 
				new Pair<Class<?>, Rewriter>(BreakConditionsContainingBothLogicalAndRandomVariables.class,
						new MoveRandomVariableValueExpressionConditionDown()),
						
				//
				// Support for: lifting products of factors
			    // prod_{{(on I) Alpha | C}}
			    // ->
				// Alpha ^ R_card(| C |_I)
				//
			    // prod_{}
				// ->
				// 1

//								new Pair<Class<?>, Rewriter>(MoveAllConditionsOnRandomVariablesDown.class,
								new Pair<Class<?>, Rewriter>(MoveRandomVariableValueExpressionConditionDown.class,
										
						new LiftProductOfFactorToVariable()),
				new Pair<Class<?>, Rewriter>(LiftProductOfFactorToVariable.class,
						new ProductOnExtensionalSet()),
				//
				// Support for: lambda applications on messages
				// 	
				// (lambda q(Y) : if q(Y) then 1 else 0)(false) 
				// ->
				// if false then 1 else 0 
				new Pair<Class<?>, Rewriter>(IntensionalSet.class,
						new Lambda()), // Note: is a quantifier sub-expression and scoped variable provider
				new Pair<Class<?>, Rewriter>(LiftProductOfFactorToVariable.class,
						new LambdaApplication(new PerformApplication() {
							@Override
							public boolean isApplicationToBePerformed(Expression lambdaExpression, RewritingProcess process) {
								Expression lambdaBody = Lambda.getBody(lambdaExpression);
								boolean result = !LPIUtil.containsPreviousMessageExpressions(lambdaBody) &&
										         !ConvexRewriterOnMessageBounds.containsPlaceholderExpression(lambdaBody);
								return result;
							}
						}))
				);
		
		return atomicRewriters;
	}
}
