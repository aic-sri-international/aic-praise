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
import com.sri.ai.expresso.api.LambdaExpression;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.controlflow.DisequalityToEqualityInIfThenElseCondition;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.controlflow.IfThenElseSubExpressionsAndImposedConditionsProvider;
import com.sri.ai.grinder.library.equality.injective.DisequalityOnInjectiveSubExpressions;
import com.sri.ai.grinder.library.equality.injective.DisequalityOnMutuallyExclusiveCoDomainExpressions;
import com.sri.ai.grinder.library.equality.injective.EqualityOnInjectiveSubExpressions;
import com.sri.ai.grinder.library.equality.injective.EqualityOnMutuallyExclusiveCoDomainExpressions;
import com.sri.ai.grinder.library.function.InjectiveModule;
import com.sri.ai.grinder.library.function.MutuallyExclusiveCoDomainsModule;
import com.sri.ai.grinder.library.lambda.LambdaApplication;
import com.sri.ai.grinder.library.lambda.LambdaApplication.PerformApplication;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.set.extensional.ProductOnExtensionalSet;
import com.sri.ai.grinder.library.set.extensional.UnionOnExtensionalSets;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.BracketedExpressionSubExpressionsProvider;
import com.sri.ai.praise.CardinalityOfTypeAlwaysDistinctFromZero;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.Type;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.util.base.Pair;

/**
 * @see LBPRewriter#R_simplify
 * 
 * @author oreilly
 *
 */
@Beta
public class Simplify extends com.sri.ai.grinder.library.equality.cardinality.direct.core.Simplify implements LBPRewriter {

	@Override
	public String getName() {
		return LBPRewriter.R_simplify;
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
		atomicRewriters = GrinderUtil.addRewritersBefore(atomicRewriters,
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
						
				// Support for:
				// (previous) message to [ p(X) ] from [ if p(X) then 0.2 else 0.8 ] -> if p(X) then 0.2 else 0.8		
				// (previous) message to [ if p(X) then 1 else 0 ] from [ p(X) ] -> if p(X) then 1 else 0		
				new Pair<Class<?>, Rewriter>(
						CardinalityOfTypeAlwaysDistinctFromZero.class, 
						new SimpleMessageDefinitionRewritesToItsValue()),
						
				// Support for:
				// if RVV then Alpha else 0 -> if RVV then 1 else 0 (as well as flipped case)		
				new Pair<Class<?>, Rewriter>(
						SimpleMessageDefinitionRewritesToItsValue.class, 
						new MessageValueOnBooleanRandomVariableValueWithZeroInOneBranchIsDeterministic()),
						
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
				
				// Support for: Mutually exclusive Co-domains
				// e.g.:
				// [ p(a, X) ] = [ q(Y, b) ] -> false 
				// (X, Y) = (1, 2, 3)        -> false 
				new Pair<Class<?>, Rewriter>(
						IfThenElseSubExpressionsAndImposedConditionsProvider.class,
						new EqualityOnMutuallyExclusiveCoDomainExpressions()),
				new Pair<Class<?>, Rewriter>(
						IfThenElseSubExpressionsAndImposedConditionsProvider.class,
						new DisequalityOnMutuallyExclusiveCoDomainExpressions()),
				new Pair<Class<?>, Rewriter>(
						IfThenElseSubExpressionsAndImposedConditionsProvider.class,
						new MutuallyExclusiveCoDomainsModule()),
				
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
				// Support for: lifting products of factors
			    // prod_{{(on I) Alpha | C}}
			    // ->
				// Alpha ^ R_card(| C |_I)
				//
			    // prod_{}
				// ->
				// 1
				new Pair<Class<?>, Rewriter>(UnionOnExtensionalSets.class,
										
						new LiftProductOfFactorToVariable()),
				new Pair<Class<?>, Rewriter>(LiftProductOfFactorToVariable.class,
						new ProductOnExtensionalSet()),

				// Support for: lambda applications on messages
				// 	
				// (lambda q(Y) : if q(Y) then 1 else 0)(false) 
				// ->
				// if false then 1 else 0 
				new Pair<Class<?>, Rewriter>(LiftProductOfFactorToVariable.class,
						new LambdaApplication(new PerformApplication() {
							@Override
							public boolean isApplicationToBePerformed(Expression lambdaExpression, RewritingProcess process) {
								Expression lambdaBody = ((LambdaExpression) lambdaExpression).getBody();
								boolean result = !LPIUtil.containsPreviousMessageExpressions(lambdaBody) &&
										         !ConvexRewriterOnMessageBounds.containsPlaceholderExpression(lambdaBody);
								return result;
							}
						})),
					
				// Support for:
				// if RandomVaribleValue = Formula then Alpha else Beta
				// --->
				// if Formula then if RandomVariableValue then Alpha else Beta else if RandomVariableValue then Beta else Alpha
				new Pair<Class<?>, Rewriter>(
						DisequalityToEqualityInIfThenElseCondition.class,
						new EqualityOnRandomVariableAndFormulaInIfThenElseConditionToFormulaOnTop())
				);
		
		return atomicRewriters;
	}
}
