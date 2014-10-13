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
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.BracketedExpression;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.lbp.core.AbstractLBPHierarchicalRewriter;
import com.sri.ai.util.math.Rational;

/**
 * <p>
 * A rewriter that simplifies applications of 'convex hull' applications on a
 * set of messages. A message bound is the convex hull of a set of messages,
 * where messages can be seen as a vector in space. The dimension of the vector
 * is the number of values taken by the corresponding random variable (so a
 * boolean random variable message has dimension two). The convex hull of two
 * points on a plane is the line segment linking then. So the two messages on
 * [p]:
 * </p>
 * 
 * <pre>
 * if p then 0.8 else 0.2
 * if p then 0.3 else 0.7
 * </pre>
 * 
 * form a convex hull with infinite messages, including
 * 
 * <pre>
 * if p then 0.8 else 0.2
 * if p then 0.79 else 0.21
 * if p then 0.75 else 0.25
 * ...
 * if p then 0.4 else 0.6
 * if p then 0.35 else 0.65
 * if p then 0.3 else 0.7
 * </pre>
 * 
 * <p>
 * This is just a fancy way of saying that the potential of p = true is in [0.3,
 * 0.8]. Initially, it was considered to use bounds in this latest form (i.e. as
 * an interval/range), but it gets tricky for random variables with more than
 * two values, and would be hard if not impossible to generalize to random
 * variables with more values.
 * </p>
 * <p>
 * So a message bound is one such convex hull. The nice thing about it is that,
 * for boolean random variables, we only need two messages to represent an
 * entire convex hull.
 * </p>
 * <p>
 * Technically, a convex hull of S is the set of convex combinations of elements
 * in S, that is, { sum a_i Si | ai in [0,1] and sum_i ai = 1 }. This means that
 * S could be of arbitrary size, even when the same convex hull could be
 * represented only by its two extrema (in the case of 2 dimensions). For points
 * in a plane, we can always discard the points that are in the inner region of
 * the hull and keep the extrema only, without losing information.
 * </p>
 * <p>
 * During our calculations we will need to determine the convex hull of a set of
 * messages with a size greater than two, even for boolean random variables
 * only. Naturally, we want to discard the unnecessary messages and keep only
 * the extrema. This is very easy to do for normalized messages; we normalize
 * them and keep the ones with the greatest and smallest potentials for the true
 * value. This is what SimplifyMessagesConvexHull does:
 * </p>
 * 
 * <pre>
 * 'convex hull'( { [if p then 0.8 else 0.2], [if p then 600 else 400], [if p then 300 else 700] } )
 * ----&gt;
 * 'convex hull'( { [if p then 0.8 else 0.2], [if p then 0.3 else 0.7] } )
 * </pre>
 * 
 * <b>Note</b>: This logic currently only works with random variables whose
 * type is {false, true}.<br>
 * <br>
 * <b>Note</b>: All messages are currently normalized internally in order to
 * ensure the logic can work correctly without the need to rely on the messages
 * being normalized externally.<br>
 * <br>
 * 
 * @author oreilly
 * 
 */
@Beta
public class SimplifyMessagesConvexHull extends AbstractLBPHierarchicalRewriter {

	//
	// START-Rewriter
	@Override
	public String getName() {
		return "R_simplify_messages_convex_hull";
	}
	
	// END-Rewriter
	//
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, final RewritingProcess process) {
		Expression result = expression;
		if (isConvexHullRequiringPossibleSimplification(expression)) {
			// is of the form: 
			// 'convex hull'({if X = a [M1] else [M2], ..., if X = a [Mn-1] else [Mn] }) 
			// or
			// 'convex hull'({[if X = a then M1 else M2], ..., [if X = a Mn-1 else Mn] })
			if (isConditional(expression, process)) {
				// Step 1: Externalize conditional messages first, i.e:
				// 'convex hull'({[if X = a then M1 else M2], ..., [if X = a Mn-1 else Mn] })
				Expression externalizedConditionalMessages = expression.get(0).replaceAllOccurrences(new Function<Expression, Expression>() {
					@Override
					public Expression apply(Expression input) {
						Expression result = input;
						if (BracketedExpressionSubExpressionsProvider.isBracketedExpression(input)) {
							result = externalizeMessage(input, process);
						}
						
						return result;
					}
				},
				process);
				// Step 2: Externalize conditional elements, i.e.:
				// 'convex hull'({if X = a [M1] else [M2], ..., if X = a [Mn-1] else [Mn] })
				//
				// Externalize the extensional set.
				// to get something like this instead:
				// if X = a {[M1],...,[Mn-1]} else {[M2],...,[Mn]}
				Expression conditionalExtensionalSetsOfMessages = process.rewrite(LBPRewriter.R_normalize, externalizedConditionalMessages);
				// Now place a 'convex hull' application around each conditioned
				// extensional set of messages to get something like:
				// if X = a 'convex hull'{[M1],...,[Mn-1]} else 'convex hull'{[M2],...,[Mn]}
				result = conditionalExtensionalSetsOfMessages.replaceAllOccurrences(new Function<Expression, Expression>() {
					@Override
					public Expression apply(Expression input) {
						Expression result = input;
						boolean isExtensionalSetOfMessages = false;
						if (Sets.isExtensionalUniSet(input)) {
							isExtensionalSetOfMessages = true;
							for (Expression element : ExtensionalSet.getElements(input)) {
								if (!(BracketedExpressionSubExpressionsProvider.isBracketedExpression(element)
									  &&
									  LPIUtil.isMessageValue(((BracketedExpression) element).getInnerExpression(), process))) {
									isExtensionalSetOfMessages = false;
									break;
								}
							}
						}
						
						if (isExtensionalSetOfMessages) {
							// i.e. have: {[M1],..., [Mn]}
							// and want : 'convex hull'({[M1],..., [Mn]})
							// unconditionally simplified.
							result = unconditionalSimplification(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(LPIUtil.FUNCTOR_CONVEX_HULL, input), process);
						}
						
						return result;
					}
				},
				process);
				
				// Ensure final simplification of the overall expression occurs
				result = process.rewrite(LBPRewriter.R_normalize, result);
			}
			else {
				result = unconditionalSimplification(expression, process);
			}
		}
		
		return result;
	}

	//
	// PRIVATE
	//	
	private boolean isConvexHullRequiringPossibleSimplification(Expression expression) {
		boolean result = false;
		if (Expressions.hasFunctor(expression, LPIUtil.FUNCTOR_CONVEX_HULL) &&
			expression.numberOfArguments() == 1) {
			Expression convexHullArgument = expression.get(0);
			if (ExtensionalSet.isExtensionalSet(convexHullArgument)) {
				result = true;
			}
		}
		return result;
	}
	
	// Check if is of the form: 
	// 'convex hull'({if X = a [M1] else [M2], ..., if X = a [Mn-1] else [Mn] })
	// or
	// 'convex hull'({[if X = a then M1 else M2], ..., [if X = a Mn-1 else Mn] })
	private boolean isConditional(Expression expression, RewritingProcess process) {
		boolean result = false;
		
		for (Expression element : ExtensionalSet.getElements(expression.get(0))) {
			// If there is a conditional then this is a conditional bounds expression
			if (IfThenElse.isIfThenElse(element)) {
				result = true;
				break;
			}
			
			if (BracketedExpressionSubExpressionsProvider.isBracketedExpression(element)) {
				if (!LPIUtil.isMessageValue(((BracketedExpression) element).getInnerExpression(), process)) {
					// implies it is a conditional message that needs to be externalized
					result = true;
					break;
				}
			}
		}
		
		return result;
	}
	
	private Expression externalizeMessage(Expression message, RewritingProcess process) {
		Expression result = message;
		
		Expression messageValue = ((BracketedExpression) message).getInnerExpression();
		if (!LPIUtil.isMessageValue(messageValue, process)) {
			if (IfThenElse.isIfThenElse(messageValue)) {
				Expression thenBranch = externalizeMessage(BracketedExpressionSubExpressionsProvider.make(IfThenElse.getThenBranch(messageValue)), process);
				Expression elseBranch = externalizeMessage(BracketedExpressionSubExpressionsProvider.make(IfThenElse.getElseBranch(messageValue)), process);
				result = IfThenElse.make(IfThenElse.getCondition(messageValue), thenBranch, elseBranch);
			}
			else {
				throw new IllegalArgumentException("Not a recognized message: "+message);
			}
		}
		
		return result;
	}
	
	private Expression unconditionalSimplification(Expression expression, RewritingProcess process) {
		Expression result = null;
		
		List<Expression> unnormalizedMessageValues = LPIUtil.extractMessageValuesFromConvexHull(expression, process);
		if (unnormalizedMessageValues.size() == 0) {
			throw new IllegalArgumentException("Not a legal convex hull expression: "+expression);
		}

		// Normalize all messages
		List<Expression> normalizedMessageValues = new ArrayList<Expression>();
		for (Expression unnormalizedMessageValue : unnormalizedMessageValues) {
			Expression normalizedValue = normalizedMessageValue(unnormalizedMessageValue);
			// This implies we have a message like:
			// [if p(X) then 0 else 0]
			// which indicates the whole bound needs to be treated as a trivial bound.
			if (normalizedValue.equals(Expressions.ZERO)) {
				result = LPIUtil.makeTrivialBound(IfThenElse.getCondition(unnormalizedMessageValue));
				break;
			}
			normalizedMessageValues.add(normalizedValue);
		}
		
		// If I've not run into a case like: [if p then 0 else 0], which requires a trivial bound to be returned.
		if (result == null) {
			// Find indices and values of maximum probabilities for 'true' in all messages
			Rational minTrueValue  = getTrueValue(normalizedMessageValues.get(0));
			int      minTrueIndex  = 0;
			Rational maxTrueValue  = minTrueValue;
			int      maxTrueIndex  = 0;
			for (int i = 1; i < normalizedMessageValues.size(); i++) {
				Expression normalizedMessageValue = normalizedMessageValues.get(i);
				Rational trueValue  = getTrueValue(normalizedMessageValue);
				if (maxTrueValue.compareTo(trueValue) < 0) {
					maxTrueValue = trueValue;
					maxTrueIndex = i;
				}
				if (minTrueValue.compareTo(trueValue) > 0) {
					minTrueValue = trueValue;
					minTrueIndex = i;
				}
			}
			
			if (maxTrueIndex == minTrueIndex) {
				// i.e. the convex hull has collapsed to a known value
				result = normalizedMessageValues.get(maxTrueIndex);
			}
			else {
				// Ensure the original order is respected 
				// i.e. could already be simplified.
				int firstElement  = maxTrueIndex;
				int secondElement = minTrueIndex;
				if (firstElement > secondElement) {
					firstElement  = minTrueIndex;
					secondElement = maxTrueIndex;
				}				
				Expression extensionalSet =
						ExtensionalSet.makeUniSet( 
								BracketedExpressionSubExpressionsProvider.make(normalizedMessageValues.get(firstElement)), 
								BracketedExpressionSubExpressionsProvider.make(normalizedMessageValues.get(secondElement))
								);
				result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(LPIUtil.FUNCTOR_CONVEX_HULL, extensionalSet);
				if (result.equals(expression)) {
					// i.e. no actual simplification occurred
					result = expression;
				}
			}
		}
		
		return result;
	}
	
	private Expression normalizedMessageValue(Expression messageValue) {
		Expression result   = null;
		Rational trueValue  = getTrueValue(messageValue);
		Rational falseValue = getFalseValue(messageValue);
		
		Rational   partition = trueValue.add(falseValue);
		if (partition.isZero()) {
			result = Expressions.ZERO;
		}
		else if (messageValue.getSyntacticFormType().equals("Symbol")) {
			result = Expressions.ZERO_POINT_FIVE;
		}
		else {
			result  = IfThenElse.make(IfThenElse.getCondition(messageValue), 
					Expressions.makeSymbol(trueValue.divide(partition)), 
					Expressions.makeSymbol(falseValue.divide(partition)));
		}
		
		return result;
	}
	
	private Rational getTrueValue(Expression messageValue) {
		Rational result = null;
		
		if (messageValue.getSyntacticFormType().equals("Symbol")) {
			result = messageValue.rationalValue();
		}
		else {
			Expression thenValue = IfThenElse.getThenBranch(messageValue);
			if (thenValue.getSyntacticFormType().equals("Symbol")) {
				result = thenValue.rationalValue();
			}
			else {
				throw new IllegalArgumentException("Random Variable for message cannot be determined ["+messageValue+"]");
			}
		}
		
		return result;
	}
	
	private Rational getFalseValue(Expression messageValue) {
		Rational result = null;
		
		if (messageValue.getSyntacticFormType().equals("Symbol")) {
			result = messageValue.rationalValue();
		}
		else {
			Expression elseValue = IfThenElse.getElseBranch(messageValue);
			if (elseValue.getSyntacticFormType().equals("Symbol")) {
				result = elseValue.rationalValue();
			}
			else {
				throw new IllegalArgumentException("Random Variable for message cannot be determined ["+messageValue+"]");
			}
		}
		
		return result;
	}
}
