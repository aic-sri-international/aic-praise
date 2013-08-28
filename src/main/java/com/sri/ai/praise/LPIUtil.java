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
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.core.AbstractReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Apply;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.IsApplicationOf;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.TotalRewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.Substitute;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.model.IsRandomVariableValueExpression;
import com.sri.ai.praise.model.RandomPredicate;
import com.sri.ai.praise.model.RandomPredicateCatalog;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.PredicateIterator;

/**
 * General purpose utility routines related to lifted belief propagation.
 * 
 * 
 */
@Beta
public class LPIUtil {

	// FUNCTOR CONSTANTS
	public static final String FUNCTOR_BELIEF                = "belief";
	public static final String FUNCTOR_NORMALIZE             = "normalize";
	public static final String FUNCTOR_NEIGHBOR              = "Neigh";
	public static final String FUNCTOR_IN                    = "in";
	public static final String FUNCTOR_MSG_TO_FROM           = "message to . from .";
	public static final String FUNCTOR_PREVIOUS_MSG_TO_FROM  = "previous message to . from .";
	public static final String FUNCTOR_NEIGHBORS_OF_FROM     = "neighbors of . from .";
	public static final String FUNCTOR_CONVEX_HULL           = "convex hull";
	
	/**
	 * Returns an expression describing the conditions under which a random
	 * variable's value occurs in a given expression.
	 */
	public static Expression conditionForRandomVariableValueToOccurInExpression(
			Expression randomVariable, Expression expression,
			RewritingProcess process) {
		
		Expression randomVariableValueExpression = BracketedExpressionSubExpressionsProvider.getRandomVariableValueExpression(randomVariable);
		//
		RandomPredicateCatalog           catalog               = RandomPredicateCatalog.getFromBracketedModel(process);
		List<Expression>                 occurrenceConditions  = new LinkedList<Expression>();
		SubExpressionsDepthFirstIterator subExpressionIterator = new SubExpressionsDepthFirstIterator(expression);
		
		while (subExpressionIterator.hasNext()) {
			Expression subExpression = subExpressionIterator.next();
			if (catalog.contains(new RandomPredicate(subExpression))
					&& randomVariableValueExpressionsMatch(randomVariableValueExpression, subExpression)) {
				
				Expression result = Equality
						.conditionForSubExpressionsEquality(
								randomVariableValueExpression,
								subExpression);
				
				if (result.equals(Expressions.TRUE)) {
					// Short-circuit
					return Expressions.TRUE;
				}
				
				occurrenceConditions.add(result);
			}
		}
		Expression occurs = Or.make(occurrenceConditions);
		return occurs;
	}
	
	/**
	 * Determine whether an expression is a conditional whose condition does not contain
	 * random variable value expressions.
	 * 
	 * @param expressions
	 *            the expression to be tested.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return true if the passed in expression is a conditional whose condition
	 *         does not contain random variable value expressions, false otherwise.
	 */
	public static boolean isConditionalNotContainingRandomVariableValueExpressionsInCondition(
			Expression expression, RewritingProcess process) {
		boolean result = false;
		
		if (IfThenElse.isIfThenElse(expression)) {
			Expression condition = IfThenElse.getCondition(expression);
			
			result = !containsRandomVariableValueExpression(condition, process);
		}
		
		return result;
	}

	/**
	 * A utility routine to simplify call R_set_diff(N\{E}).
	 *
	 * @param set
	 *            a Set expression (intensional or extensional) that is to be
	 *            subtracted from.
	 * @param toSubtract
	 *            an Expression that is a set itself or an element (if an
	 *            element it will be added to a uniset before
	 *            R_set_diff(N\{E})is called.).
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return R_set_diff(N\{E})
	 */
	public static Expression callSetDiff(Expression set, Expression toSubtract, RewritingProcess process) {
		// R_set_diff(N\{E})
		Expression SetV = toSubtract;
		if (!Sets.isSet(SetV)) {
			SetV = ExtensionalSet.makeUniSet(Arrays.asList(new Expression[] { toSubtract }));
		}

		Expression result = process.rewrite(LBPRewriter.R_set_diff, LPIUtil.argForSetDifferenceRewriteCall(set, SetV));

		return result;
	}

	/**
	 * Construct a product of messages of the form:
	 * 
	 * <pre>
	 * prod_A in S m_B<-A
	 * 
	 * which concretely as an expression is:
	 * 
	 * product({{ (on A in S) m_B<-A | C }})
	 * 
	 * <pre>
	 * 
	 * @param index
	 *            the index expression 'A'.
	 * @param indexDomain
	 *            the domain 'S'.
	 * @param message
	 *            the message of the form m_B<-A.
	 * @param condition
	 *            the condition C on the scoping expression.
	 * @return a product of message of the form: product({{ (on A in S) m_B<-A | C }}).
	 */
	public static Expression makeProductOfMessages(Expression index,
			Expression indexDomain, Expression message, Expression condition) {

		Expression prodIntensionalSet = IntensionalSet
				.makeMultiSetWithASingleIndexExpression(index, indexDomain,
						message, condition);
		Expression result = Expressions.make(FunctorConstants.PRODUCT,
				prodIntensionalSet);

		return result;
	}

	/**
	 * Extract the random variable value expression from a random variable
	 * expression.
	 * 
	 * @param randomVariable
	 *            a Random Variable of the form [ v ].
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return v from [ v ].
	 * @throws IllegalArgumentException
	 *             if randomVariable is not of the form [ v ].
	 */
	public static Expression getRandomVariableValueExpression(
			Expression randomVariable, RewritingProcess process) {
		Expression result = null;
		// Extract v from [ v ]
		if (BracketedExpressionSubExpressionsProvider.isRandomVariable(
				randomVariable, process)) {
			result = BracketedExpressionSubExpressionsProvider
					.getRandomVariableValueExpression(randomVariable);
		} 
		else {
			throw new IllegalArgumentException(
					"Random variable does not have the form [ v ], instead is = "
							+ randomVariable);
		}

		return result;
	}

	/**
	 * 
	 * @param factor
	 *            a factor of the form [ f ].
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return f from [ f ].
	 * @throws IllegalArgumentException
	 *             if factor is not of the form [ f ] or is a random variable.
	 */
	public static Expression getFactorValueExpression(Expression factor,
			RewritingProcess process) {
		Expression result = null;

		assertFactorOk(factor, process);

		// Extract f from [ f ]
		result = BracketedExpressionSubExpressionsProvider
				.getExpressionInBrackets(factor);

		return result;
	}

	/**
	 * <pre>
	 * rv_is_referenced_by([ Ev ], E)
	 * [ Ev ] is a random variable expression
	 * E is any expression
	 * Returns a condition that is true if and only if E depends on the value of [ Ev ]
	 * return R_formula_simplification(Disjunction_{E' subexpression of E} [ Ev ] = [ E' ]) ))
	 * // in practice, it is best to run R_formula_simplification([ Ev ] = [ E' ]) for each E', because if this is true,
	 * // we can short-circuit the computation.
	 * </pre>
	 * 
	 * @param randomVariable
	 *            is a random variable expression of the form [Ev].
	 * @param expression
	 *            is any expression (E).
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return a condition that is true if and only if expression (E) depends on the
	 *         value of the random variable [ Ev ].
	 * @throws IllegalArgumentException
	 *             if randomVariable is not a random variable expression of the
	 *             form [Ev].
	 */
	public static Expression randomVariableIsReferencedByExpression(
			final Expression randomVariable, Expression expression,
			final RewritingProcess process) {

		Trace.in("+rv_is_referenced_by([Ev]={}, E={})", randomVariable, expression);

		assertRandomVariableOk(randomVariable, process);

		Trace.log("return R_formula_simplification(Disjunction_{E' subexpression of E} [ Ev ] = [ E' ]) ))");

		Expression result = LPIUtil
				.conditionForRandomVariableValueToOccurInExpression(
						randomVariable, expression, process);

		result = process.rewrite(LBPRewriter.R_formula_simplification, result);

		Trace.out("-rv_is_referenced_by={}", result);

		return result;
	}

	/**
	 * Utility for testing if two random variable value expressions match, that is, have the same functor and the same number of arguments
	 * 
	 * @param v1
	 *            the first random variable value expression to compare.
	 * @param v2
	 *            the second random variable value expression to compare.
	 * @return true if the match, false otherwise.
	 */
	public static boolean randomVariableValueExpressionsMatch(Expression v1, Expression v2) {
		boolean result = false;

		// Note: a random variable value expression may just be a symbol
		// as opposed to a functor with arguments. Therefore, need
		// to take this into account when comparing expressions
		if (v1.getFunctor() == null && v2.getFunctor() == null) {
			result = v1.equals(v2);
		} 
		else if (v1.getFunctor() != null && v2.getFunctor() != null) {
			result = Expressions.hasFunctor(v1, v2.getFunctor())
					&& v1.numberOfArguments() == v2.numberOfArguments();
		}

		return result;
	}

	/**
	 * Utility for asserting that an expression is of the form:
	 * 
	 * <pre>
	 * neighbors of . from .
	 * </pre>
	 * 
	 * @param neigborsOfFrom
	 *            the expression being asserted to be of the form 'neighbors of
	 *            . from .'
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @throws IllegalArgumentException
	 *             if the argument neigborsOfFrom is not of the correct form.
	 */
	public static void assertNeighborsOfFromOk(Expression neigborsOfFrom,
			RewritingProcess process) {
		if (!Expressions.hasFunctor(neigborsOfFrom,
				LPIUtil.FUNCTOR_NEIGHBORS_OF_FROM)
				|| neigborsOfFrom.numberOfArguments() != 2) {
			throw new IllegalArgumentException(
					"Argument neigborsOfFrom is not a legal neighbors of from expression:="
							+ neigborsOfFrom);
		}
	}

	/**
	 * Utility for asserting that an expression is of the form:
	 * 
	 * <pre>
	 * message to . from .
	 * </pre>
	 * 
	 * @param messageToFrom
	 *            the expression being asserted to be of the form 'message to .
	 *            from .'
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @throws IllegalArgumentException
	 *             if the argument messageToFrom is not of the correct form.
	 */
	public static void assertMessageToFromOk(Expression messageToFrom,
			RewritingProcess process) {
		if (!Expressions
				.hasFunctor(messageToFrom, LPIUtil.FUNCTOR_MSG_TO_FROM)
				|| messageToFrom.numberOfArguments() != 2) {
			throw new IllegalArgumentException(
					"Argument messageToFrom is not a legal message to from expression:="
							+ messageToFrom);
		}
	}
	
	/**
	 * A utility for asserting that a belief expression is of the form 'belief([
	 * v ])', where [ v ] is a random variable that exists in the model
	 * assocaited with the rewriting process.
	 * 
	 * @param belief
	 *            a belief expression 'belief([ v ])'.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @throws IllegalArgumentException
	 *             if belief is not a legal belief expression.
	 */
	public static void assertBeliefOk(Expression belief, RewritingProcess process) {
		// Map input arguments
		if (!belief.hasFunctor(LPIUtil.FUNCTOR_BELIEF) && belief.numberOfArguments() == 1) {
			throw new IllegalArgumentException("Not a valid belief expression:"+belief);
		}
		Expression randomVariable = belief.get(0);
		LPIUtil.assertRandomVariableOk(randomVariable, process);
	}

	/**
	 * Utility for asserting that a random variable is of the form [ v ] and
	 * that it exists in the model associated with the rewriting process.
	 * 
	 * @param randomVariable
	 *            is a random variable expression [Ev].
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @throws IllegalArgumentException
	 *             if randomVariable is not a random variable expression of the
	 *             form [Ev].
	 */
	public static void assertRandomVariableOk(Expression randomVariable,
			RewritingProcess process) {
		if (!BracketedExpressionSubExpressionsProvider
				.isBracketedExpression(randomVariable)
				|| !BracketedExpressionSubExpressionsProvider.isRandomVariable(
						randomVariable, process)) {
			throw new IllegalArgumentException(
					"Random Variable is not a random variable expression of the form [Ev]:="
							+ randomVariable);
		}
	}

	/**
	 * Utility for asserting that a factor is of the form [ f ].
	 * 
	 * @param factor
	 *            is a factor expression [ f ].
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @throws IllegalArgumentException
	 *             if factor is not a factor expression of the form [ f ] or is
	 *             in fact a random variable expression in the model.
	 */
	public static void assertFactorOk(Expression factor,
			RewritingProcess process) {
		if (!BracketedExpressionSubExpressionsProvider
				.isBracketedExpression(factor)
				|| BracketedExpressionSubExpressionsProvider.isRandomVariable(
						factor, process)) {
			throw new IllegalArgumentException(
					"Factor is not a legal factor expression of the form [ f ]:="
							+ factor);
		}
	}

	/**
	 * Utility for asserting that a product expression, which is a
	 * multiplication on intensionally or extensionally defined sets is of the
	 * following legal forms:
	 * 
	 * <pre>
	 * product({Alpha_1,...,Alpha_n})
	 * or
	 * product({{Alpha_1,...,Alpha_n}})
	 * or
	 * product({ (S) Alpha | C }) 
	 * or
	 * product({{ (S) Alpha | C }}) 
	 * 
	 * whereby:
	 * 
	 * - S: an optional scoping expression, consisting of 1 to n index
	 *      (I) and indexDomain (D) expression pairs, i.e.:        
	 *      
	 *      (on I_1 in D_1,...,I_n in D_n) 
	 *    
	 *      where I is a logical variable and D is a Set expression 
	 *      (intensional or extensional).
	 *       
	 * - Alpha: any legal expressions for the context, that is being 
	 *          multiplied.
	 *          
	 * - C: an optional condition on the indices in the scoping 
	 *      expression S.
	 * 
	 * Free variables are allowed in any of the sub-expressions.
	 * </pre>
	 * 
	 * @param product
	 *            an expression of the form prod_{V in N'} E
	 * 
	 * @throws IllegalArgumentException
	 *             if product does not conform to its expected form.
	 */
	public static void assertProductOk(Expression product) {
		if (!Expressions.hasFunctor(product, FunctorConstants.PRODUCT)) {
			throw new IllegalArgumentException(
					"Product does not represent an expected product expression: ="
							+ product);
		}
		if (product.numberOfArguments() != 1) {
			throw new IllegalArgumentException("product[" + product
					+ "] should only have 1 argument.");
		}
		Expression productArgument = product.get(0);
		if (!Sets.isSet(productArgument)) {
			throw new IllegalArgumentException(
					"Argument to product expression[" + product
							+ "] should be a set.");
		}
	}
	
	/**
	 * Determine if the expression is of the form: product({{(on I) Alpha | C }}).
	 * @param expression
	 *        the expression to be tested.
	 * @return true if a product expression, false otherwise.
	 */
	public static boolean isProductExpression(Expression expression) {
		boolean result = false;
		if (expression.hasFunctor(FunctorConstants.PRODUCT)    &&
			IntensionalSet.isIntensionalSet(expression.get(0)) &&
			Sets.isMultiSet(expression.get(0))                   ) {
			result = true;
		}
		return result;
	}
	
	/**
	 * Determine whether or not an expression is or contains 'previous message
	 * to . from . ' expressions.
	 * 
	 * @param expression
	 * @return true if the expression passed contains 'previous message to .
	 *         from .' expressions.
	 */
	public static boolean containsPreviousMessageExpressions(Expression expression) {		
		boolean result = false;
		if (Expressions.hasFunctor(expression, LPIUtil.FUNCTOR_PREVIOUS_MSG_TO_FROM)) {
			result = true;
		} 
		else {
			Iterator<Expression> subExpressionsIterator = new SubExpressionsDepthFirstIterator(
					expression);
			result = Util.thereExists(subExpressionsIterator, new IsApplicationOf(LPIUtil.FUNCTOR_PREVIOUS_MSG_TO_FROM));
		}
		
		return result;
	}
	
	/**
	 * Determine whether or not an expression is or contains bounds, 
	 * i.e. 'convex hull(...)' expressions.
	 * 
	 * @param expression
	 * @return true if the expression passed contains 'previous message to .
	 *         from .' expressions.
	 */
	public static boolean containsBoundExpressions(Expression expression) {		
		boolean result = false;
		if (Expressions.hasFunctor(expression, LPIUtil.FUNCTOR_CONVEX_HULL)) {
			result = true;
		} 
		else {
			Iterator<Expression> subExpressionsIterator = new SubExpressionsDepthFirstIterator(
					expression);
			result = Util.thereExists(subExpressionsIterator, new IsApplicationOf(LPIUtil.FUNCTOR_CONVEX_HULL));
		}
		
		return result;
	}
	
	/**
	 * Determine whether or not an expression is or contains 'product(...)' expressions.
	 * 
	 * @param expression
	 * @return true if the expression passed contains 'product(...)' expressions.
	 */
	public static boolean containsProductExpressions(Expression expression) {		
		boolean result = false;
		if (Expressions.hasFunctor(expression, FunctorConstants.PRODUCT)) {
			result = true;
		} 
		else {
			Iterator<Expression> subExpressionsIterator = new SubExpressionsDepthFirstIterator(expression);
			result = Util.thereExists(subExpressionsIterator, new IsApplicationOf(FunctorConstants.PRODUCT));
		}
		
		return result;
	}
	
	public static boolean isRandomVariable(Expression expression, RewritingProcess process) {
		return BracketedExpressionSubExpressionsProvider.isRandomVariable(expression, process);
	}	

	public static boolean isRandomVariableValueExpression(Expression expression, RewritingProcess process) {
		RandomPredicateCatalog catalog = RandomPredicateCatalog.getFromBracketedModel(process);
		if (catalog == null) {
			throw new IllegalArgumentException("Trying to decide if an expression is a random variable value without a random predicate catalog available in process");
		}
		RandomPredicate randomPredicate = new RandomPredicate(expression);
		boolean result = catalog.contains(randomPredicate);
		return result;
	}

	public static boolean containsRandomVariableValueExpression(
			Expression expression, RewritingProcess process) {
		Iterator<Expression> subExpressionsIterator = new SubExpressionsDepthFirstIterator(
				expression);
		boolean result = Util.thereExists(subExpressionsIterator, new IsRandomVariableValueExpression(process));
		return result;
	}

	/**
	 * Create a beingComputed expression. This is a conditional union of 
	 * sets of pairs of nodes (destination, origin), representing messages 
	 * being computed and depending on the current computation.
	 * 
	 * @return an expression representing an initial beingComputed value.
	 */
	public static Expression createNewBeingComputedExpression() {
		return ExtensionalSet.makeEmptySetExpression();
	}
	
	/**
	 * Extract the message value expressions (i.e. factors on a single random
	 * variable) from a bounds (i.e. 'convex hull').
	 * 
	 * @param expression
	 *            a bounds expression (i.e. a 'convex hull'({[message 1],
	 *            [message 2]})).
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return a list of the message value expressions contained in the bounds
	 *         expression (i.e. a list of 'if <random variable value> then
	 *         <number value> else <number value>').
	 * @throws IllegalArgumentException
	 *             if any of the elements in the 'convex hull's extensional
	 *             uni-set is not an expression of the form: 
	 *             [ a non-random variable value expression ].
	 */
	public static List<Expression> extractMessageValuesFromConvexHull(Expression expression, RewritingProcess process) {
		List<Expression> result = new ArrayList<Expression>();
		if (Expressions.hasFunctor(expression, LPIUtil.FUNCTOR_CONVEX_HULL) &&
			expression.numberOfArguments() == 1 &&
			ExtensionalSet.isExtensionalSet(expression.get(0))) {
			// Note: At this point I know I'm of this form:
			// 'convex hull'({item1, item2, ...})
			// as isConvexHullRequiringSimplification() will have been called.
			Expression extensionalSet      = expression.get(0);
			for (Expression element : ExtensionalSet.getElements(extensionalSet)) {
				boolean legalMessage = false;
				// Ensure the element is legal
				if (BracketedExpressionSubExpressionsProvider.isBracketedExpression(element) &&
					!BracketedExpressionSubExpressionsProvider.isRandomVariable(element, process)) {
					// I know I'm a bracketed expression and not a random variable
					Expression bracketedValue = BracketedExpressionSubExpressionsProvider.getExpressionInBrackets(element);
					result.add(bracketedValue);	
					legalMessage = true;
				}
				
				if (!legalMessage) {
					throw new IllegalArgumentException("Not a legal message: "+element);
				}
			}
		}
		
		return result;
	}
	
	/**
	 * Determine if an expression is of the form:<br>
	 * 'if &lta random variable value&gt then &ltmessageValue1&gt else &ltmessageValue2&gt'<br>
	 * or:<br>
	 * &lta number&gt<br>
	 * or:<br>
	 * &lta cardinality&gt<br> 
	 * or:<br>
	 * &ltan arithmetic expression (+, -, *, /, ^), containing message value terms&gt<br>
	 * <br>
	 * <b>Note:</b> arithmetic and cardinality expressions need to be handled in cases where
	 * the domain size is not known.
	 * 
	 * @param expression
	 *            the expression to be tested whether or not it is a message
	 *            value.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return true if a message value, false otherwise.
	 */
	public static boolean isMessageValue(Expression expression, RewritingProcess process) {
		boolean result = false;
		
		if (IfThenElse.isIfThenElse(expression)) {
			Expression condition = IfThenElse.getCondition(expression);
			if (LPIUtil.isRandomVariableValueExpression(condition, process)) {		
				result = isMessageValue(IfThenElse.getThenBranch(expression), process)
						 &&
						 isMessageValue(IfThenElse.getElseBranch(expression), process);
			}
		} 
		else if (expression instanceof Symbol) {
			if (((Symbol)expression).getValue() instanceof Number) {
				result = true;
			}
		}
		else if (Expressions.hasFunctor(expression, FunctorConstants.CARDINALITY)) {
			result = true;
		}
		else if (Expressions.hasFunctor(expression, FunctorConstants.PLUS)     ||
				 Expressions.hasFunctor(expression, FunctorConstants.MINUS)    ||
				 Expressions.hasFunctor(expression, FunctorConstants.TIMES)    ||
				 Expressions.hasFunctor(expression, FunctorConstants.DIVISION) ||
				 Expressions.hasFunctor(expression, FunctorConstants.EXPONENTIATION)) {
			result = true;
			for (Expression arg : expression.getArguments()) {
				if (!isMessageValue(arg, process)) {
					result = false;
					break;
				}
				
			}
		}
		
		return result;
	}
	
	/**
	 * Make a trivial bound expression for the specified random variable value, i.e:<br>
	 * <pre>
	 * 'convex hull'({[if randomVariableValue then 1 else 0],
	 *                [if randomVariableValue then 0 else 1] })
	 * </pre>
	 * 
	 * @param randomVariableValue
	 *            the random variable value expression (i.e. the V of [V]) for
	 *            which a trivial bound is to be made.
	 * @return a trivial bound for the specified random variable value.
	 */
	public static Expression makeTrivialBound(Expression randomVariableValue) {
		List<Expression> elements = new ArrayList<Expression>();
		elements.add(BracketedExpressionSubExpressionsProvider.make(IfThenElse.make(randomVariableValue, Expressions.ONE, Expressions.ZERO)));
		elements.add(BracketedExpressionSubExpressionsProvider.make(IfThenElse.make(randomVariableValue, Expressions.ZERO, Expressions.ONE)));
		
		Expression result = Expressions.make(LPIUtil.FUNCTOR_CONVEX_HULL, ExtensionalSet.makeUniSet(elements));
		
		return result;
	}
	
	/**
	 * Extend the beingComputed (a conditional union of sets of pairs of nodes
	 * (to, from) by a new pair.
	 *
	 * @param beingComputed
	 *            a conditional union of multi-sets of pairs of nodes (to, from),
	 * @param by
	 *            a tuple (to, from) or a singleton multi-set {{(to, from)}} to be
	 *            unioned with beingComputed.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return R_basic(beingComputed union by)
	 */
	public static Expression extendBeingComputed(Expression beingComputed, Expression by, RewritingProcess process) {
		Expression result = beingComputed;
		
		if (Tuple.isTuple(by)) {
			by = ExtensionalSet.makeMultiSet(Arrays.asList(by));
		} 
		if (!(Sets.isSingletonExtensionalSet(by) && Sets.isMultiSet(by))) {
			throw new IllegalArgumentException("by must be a singleton extensional multi-set:"+by);
		}
		if (!Tuple.isTuple(by.get(0)) || Tuple.size(by.get(0)) != 2) {
			throw new IllegalArgumentException("by must be a set containing a pair (to, from):"+by);
		}
		
				
		if (Sets.isEmptySet(beingComputed)) {
			result = by;
		} 
		else if (Expressions.hasFunctor(beingComputed, FunctorConstants.UNION)) {
			List<Expression> elements = new ArrayList<Expression>(beingComputed.getArguments());
			elements.add(by);
			result = Expressions.make(FunctorConstants.UNION, elements.toArray());
		} 
		else {
			// We are assuming beingComputed is a set or a conditional, 
			// so we create a top level union with the set/conditional and by
			result = Expressions.make(FunctorConstants.UNION, beingComputed, by);
		}
		
		result = process.rewrite(LBPRewriter.R_basic, result);
		
		return result;
	}
	
	/**
	 * Extend the beingComputed (a conditional union of sets of pairs of nodes
	 * (to, from) by a new pair contained within an intensional multi-set.
	 * 
	 * @param beingComputed
	 *            a conditional union of multi-sets of pairs of nodes (to, from),
	 * @param by
	 *            a tuple (to, from) representing the head of the intensional set 
	 *            to be unioned with beingComputed.
	 * @param conditionC
	 *            the condition of the intensional set to be unioned with beingComputed.
	 * @param scopingExpressionI
	 *            the scoping expression of the intensional set to be unioned with
	 *            beingComputed.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return R_basic(beingComputed union {{ by | C }}_I)
	 */
	public static Expression extendBeingComputedWithIntensionalMultiSet(Expression beingComputed, Expression by, Expression conditionC, 
			Expression scopingExpresisonI, RewritingProcess process) {
		Expression result = beingComputed;
		
		if (!Tuple.isTuple(by) || Tuple.size(by) != 2) {
			throw new IllegalArgumentException("by must be a tuple (to, from):"+by);
		}
		
		Expression intensionalSetBy = IntensionalSet.makeMultiSet(scopingExpresisonI, by, conditionC);
				
		if (Sets.isEmptySet(beingComputed)) {
			result = intensionalSetBy;
		} 
		else if (Expressions.hasFunctor(beingComputed, FunctorConstants.UNION)) {
			List<Expression> elements = new ArrayList<Expression>(beingComputed.getArguments());
			elements.add(intensionalSetBy);
			result = Expressions.make(FunctorConstants.UNION, elements.toArray());
		} 
		else {
			// We are assuming beingComputed is a set or a conditional, 
			// so we create a top level union with the set/conditional and by intensional set
			result = Expressions.make(FunctorConstants.UNION, beingComputed, intensionalSetBy);
		}
		
		return result;
	}

	public static boolean isPreviousMessage(Expression expression) {
		if (Expressions.hasFunctor(expression, FUNCTOR_PREVIOUS_MSG_TO_FROM)) {
			return true;
		}
		return false;
	}
	
	/** Indicates whether an expression is a constraint as used in LPI. */
	public static boolean isConstraint(Expression expression, RewritingProcess process) {
		return FormulaUtil.isFormula(expression, process);
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * ({a_1,...,a_n} \ {b_1,...,b_m}, i, j)<br>
	 * when calling:<br>
	 * R_DifferenceOfExtensionalAndExtensionalSet({a_1,...,a_n}, {b_1,...,b_m}, i, j).<br>
	 * 
	 * @param extensionalSetA
	 *            the extensional set {a_1,...,a_n}.
	 * @param extensionalSetB
	 *            the extensional set {b_1,...,b_m}.
	 * @param i
	 *            current index into extensional set A.
	 * @param j
	 *            current index into extensional set B.
	 * @return a tuple argument of the form: ({a_1,...,a_n} \ {b_1,...,b_m}, i, j).
	 */
	public static Expression argForDifferenceOfExtensionalAndExtensionalSetRewriteCall(
			Expression extensionalSetA, Expression extensionalSetB, int i, int j) {
		
		Expression setDiff = Expressions.make(FunctorConstants.SET_DIFFERENCE, extensionalSetA, extensionalSetB);
		Expression result  = Tuple.make(setDiff, DefaultSymbol.createSymbol(i), DefaultSymbol.createSymbol(j));

		return result;
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * ({a_1,...,a_n} \ { Alpha | C}_I, i)<br>
	 * when calling:<br>
	 * R_DifferenceOfExtensionalAndIntensionalSet({a_1,...,a_n}, { Alpha | C}_I, i).<br>
	 * 
	 * @param extensionalSet
	 *            the extensional set {a_1,...,a_n}.
	 * @param intensionalSet
	 *            the intensional set { Alpha | C }_I.
	 * @param i
	 *            current index into extensional set.
	 * @return a tuple argument of the form: ({a_1,...,a_n} \ { Alpha | C}_I, i).
	 */
	public static Expression argForDifferenceOfExtensionalAndIntensionalSetRewriteCall(
			Expression extensionalSet, Expression intensionalSet, int i) {
		Expression setDiff = Expressions.make(FunctorConstants.SET_DIFFERENCE, extensionalSet, intensionalSet);
		Expression result  = Tuple.make(setDiff, DefaultSymbol.createSymbol(i));
		
		return result;
	}
	
	/**
	 * Convenience method for constructing an argument of the form:<br> 
	 * Alpha in Set<br>
	 * when calling:<br>
     * R_in(Alpha, Set).<br>
	 * 
	 * @param alpha
	 *            an object to be checked if in Set.
	 * @param set
	 *            a Set in which Alpha is being tested for membership.
	 * @return an argument expression of the form: Alpha in Set.
	 */
	public static Expression argForInRewriteCall(Expression alpha, Expression set) {
		Expression result = Expressions.make(LPIUtil.FUNCTOR_IN, Arrays.asList(alpha, set));
		return result;
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * (Set1, Set2)<br>
	 * when calling:<br>
	 * R_intersection(Set1 intersection Set2).<br>
	 * 
	 * @param Set1
	 *            a set.
	 * @param Set2
	 *            a set.
	 * @return a tuple argument of the form: (Set1, Set2)
	 */
	public static Expression argForIntersectionRewriteCall(Expression set1, Expression set2) {
		Expression result = Tuple.make(set1, set2);

		return result;
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * (m_F<-V, beingComputed)<br>
	 * when calling:<br>
	 * R_m_to_f_from_v(m_F<-V, beingComputed).<br>
	 * 
	 * @param messageToFFromV
	 *            an expression of the form 'message to F from V, where F is a
	 *            factor and V a random variable.
	 * @param beingComputed
	 *            is a conditional union of sets of pairs of nodes, representing
	 *            messages being computed and depending on this computation.
	 * @return a tuple argument of the form: (m_F<-V, beingComputed)
	 */
	public static Expression argForMessageToFactorFromVariableRewriteCall(Expression messageToFFromV, Expression beingComputed) {
		Expression result = Tuple.make(messageToFFromV, beingComputed);

		return result;
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * (m_V<-F, true, (on ), beingComputed)<br>
	 * when calling:<br>
	 * R_m_to_v_from_f(m_V<-F, C, I, beingComputed).<br>
	 * <br>
	 * where 3rd and 4th arguments are defaulted to 'true' and '(on )' respectively.
	 * 
	 * @param msgToV_F
	 *            an expression of the form 'message to V from F, where V is a
	 *            random variable and F is a factor.
	 * @param beingComputed
	 *            is a conditional union of sets of pairs of nodes, representing
	 *            messages being computed and depending on this computation.
	 * @return a tuple argument of the form: (m_V<-F, true, (on ), beingComputed)
	 */
	public static Expression argForMessageToVariableFromFactorRewriteCall(Expression msgToV_F, Expression beingComputed) {
		Expression result = Tuple.make(msgToV_F, Expressions.TRUE, IntensionalSet.EMPTY_SCOPING_EXPRESSION, beingComputed);
		
		return result;
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * (m_V<-F, C, I, beingComputed)<br>
	 * when calling:<br>
	 * R_m_to_v_from_f(m_V<-F, C, I, beingComputed).<br>
	 * 
	 * @param msgToV_F
	 *            an expression of the form 'message to V from F, where V is a
	 *            random variable and F is a factor.
	 * @param conditionC
	 *            the constraint on the the intensional set from which the
	 *            representative factor F was selected.
	 * @param scopingExpressionI
	 *            the scoping expression on the intensional set from which the
	 *            representative factor F was selected.
	 * @param beingComputed
	 *            is a conditional union of sets of pairs of nodes, representing
	 *            messages being computed and depending on this computation.
	 * @return a tuple argument of the form: (m_V<-F, C, I, beingComputed)
	 */
	public static Expression argForMessageToVariableFromFactorRewriteCall(Expression msgToV_F,
			Expression conditionC, Expression scopingExpressionI, Expression beingComputed) {
		Expression result = Tuple.make(msgToV_F, conditionC, scopingExpressionI, beingComputed);
		
		return result;
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * ([ Ev ], PF)<br>
	 * when calling:<br>
	 * R_neigh_v_parf( [ Ev ], PF).<br>
	 * 
	 * @param randomVariable
	 *            a Random Variable expression [Ev].
	 * @param parfactor
	 *            a parfactor (PF) (i.e. a set of factors).
	 * @return a tuple argument of the form: ([ Ev ], PF)
	 */
	public static Expression argForNeighborsOfRandomVariableInParfactorRewriteCall(Expression randomVariable, Expression parfactor) {
		Expression result = Tuple.make(randomVariable, parfactor);
		
		return result;
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * ([ v ], E)<br>
	 * when calling:<br>
	 * R_normalize([ v ], E).<br>
	 * 
	 * @param randomVariable
	 *            a random variable of the form: [ v ].
	 * @param expressionE
	 *            a conditional arithmetic expression (E).
	 * @return  a tuple argument of the form: ([ v ], E)
	 */
	public static Expression argForNormalizeRewriteCall(Expression randomVariable, Expression expressionE) {
		Expression result = Tuple.make(randomVariable, expressionE);

		return result;
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * ([ v ], E)<br>
	 * when calling:<br>
	 * R_normalize_random_variable_condition([ v ], E).<br>
	 * 
	 * @param randomVariable
	 *            a random variable of the form: [ v ].
	 * @param expressionE
	 *            a conditional arithmetic expression (E).
	 * @return  a tuple argument of the form: ([ v ], E)
	 */
	public static Expression argForNormalizeRandomVariableConditionRewriteCall(Expression randomVariable, Expression expressionE) {
		Expression result = Tuple.make(randomVariable, expressionE);

		return result;
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * (prod_F in S m_V<-F, beingComputed)<br>
	 * when calling:<br>
	 * R_prod_factor(prod_F in S m_V<-F, beingComputed).<br>
	 * 
	 * @param productOfFactorsToVariable
	 *            an expression of the form: prod_F in S m_V<-F.
	 * @param beingComputed
	 *            is a conditional union of sets of pairs of nodes, representing
	 *            messages being computed and depending on this computation.
	 * @return a tuple argument of the form: (prod_F in S m_V<-F, beingComputed).
	 */
	public static Expression argForProductFactorRewriteCall(Expression productOfFactorsToVariable, Expression beingComputed) {
		Expression result = Tuple.make(productOfFactorsToVariable, beingComputed);

		return result;
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * (m * prod_F in S m_V<-F, beingComputed)<br>
	 * when calling:<br>
	 * R_prod_m_and_prod_factor(m * prod_F in S m_V<-F, beingComputed).<br>
	 * 
	 * @param conditionalMessage
	 *            a conditional message.
	 * @param productOfFactorsToVariable
	 *            an expression of the form: prod_F in S m_V<-F.
	 * @param beingComputed
	 *            is a conditional union of sets of pairs of nodes, representing
	 *            messages being computed and depending on this computation.
	 * @return a tuple argument of the form: (m * prod_F in S m_V<-F, beingComputed).
	 */
	public static Expression argForProductMessageAndProductFactorRewriteCall(
			Expression conditionalMessage, Expression productOfFactorsToVariable, Expression beingComputed) {
		Expression times  = Expressions.make(FunctorConstants.TIMES, Arrays.asList(conditionalMessage, productOfFactorsToVariable));
		Expression result = Tuple.make(times, beingComputed);
		
		return result;
	}
	
	/**
	 * Convenience method for constructing an expression of the form:<br> 
	 * S1 \ S2<br>
	 * when calling:<br>
	 * R_set_diff(S1 \ S2).<br>
	 * 
	 * @param set1
	 *            a conditional union of normalized sets (S1).
	 * @param set2
	 *            a conditional union of normalized sets (S2).
	 * @return an expression of the form: S1 \ S2.
	 */
	public static Expression argForSetDifferenceRewriteCall(Expression set1, Expression set2) {
		Expression result = Expressions.make(FunctorConstants.SET_DIFFERENCE, set1, set2);
		
		return result;
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * (sum_N E prod_{V in N'} m_F<-V, beingComputed)<br>
	 * when calling:<br>
	 * R_sum(sum_N E prod_{V in N'} m_F<-V, beingComputed).<br>
	 * 
	 * @param summationIndexN
	 *            is a conditional extensionally defined set of random variables
	 *            indexing the summation (N).
	 * @param E
	 *            a basic expression (E).
	 * @param productOfIncomingMessages
	 *            prod_{V in N'} m_F<-V, where N' is a conditional extensionally
	 *            defined set of random variables.
	 * @param T
	 *            the target random variable on which the message is being computed.
	 * @param beingComputed
	 *            is a conditional union of sets of pairs of nodes, representing
	 *            messages being computed and depending on this computation.
	 * @returna tuple argument of the form: (sum_N E prod_{V in N'} m_F<-V, beingComputed)
	 */
	public static Expression argForSumRewriteCall(Expression summationIndexN, Expression E,
			Expression productOfIncomingMessages, Expression expressionT, Expression beingComputed) {
		Expression result = Tuple.make(summationIndexN, E, productOfIncomingMessages, expressionT, beingComputed);

		return result;
	}
	
	/**
	 * <pre>
	 * pick_single_element({ (on I) Alpha | C })
	 * Returns its singleton element if the given uniset is a singleton, or null otherwise.
	 * R <- indices in I that Alpha depends on 
	 * if R is empty, return Alpha.
	 * let SomeIndex be an index in R
	 * value = pick_value(SomeIndex, I, C)
	 * if value is null, return null
	 * let I' be I - {SomeIndex}
	 * return pick_single_element({ (on I') Alpha[SomeIndex/value] | C[SomeIndex/value] })
	 * </pre>
	 * 
	 * @param intensionalSet
	 *            an intensional uniset that is assumed to be a singleton.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return its singleton element or null if one cannot be determined..
	 */
	public static Expression pickSingleElement(Expression intensionalSet, RewritingProcess process) {
		Expression result = null;
		
		// intensionalSet is of the form: { (on I) Alpha | C }
		if (!Sets.isIntensionalUniSet(intensionalSet)) {
			throw new IllegalArgumentException("Not a valid intensional set expression:"+intensionalSet);
		}
		
		Trace.in("+pick_single_element({}) constrained by {}", intensionalSet, process.getContextualConstraint());
		
		Expression       alpha       = IntensionalSet.getHead(intensionalSet);
		Trace.log("R <- indices in {} that {} depends on", IntensionalSet.getScopingExpression(intensionalSet), alpha);
		Set<Expression>  alphaVars   = Expressions.freeVariables(alpha, process);
		List<Expression> indicesI    = new ArrayList<Expression>(IntensionalSet.getIndices(intensionalSet));
		Set<Expression>  tempIndices = new LinkedHashSet<Expression>(indicesI);
		tempIndices.retainAll(alphaVars);
		List<Expression> indicesR  = new ArrayList<Expression>(tempIndices);
		Trace.log("// R = {}", indicesR);
		
		if (indicesR.size() == 0) {
			Trace.log("if R is empty");
			Trace.log("    return Alpha // Alpha = {}", alpha);
			result = alpha;
		} 
		else {
			Trace.log("let SomeIndex be an index in R");
			Expression someIndex = indicesR.get(0); 
			Trace.log("// SomeIndex = {}", someIndex);
			
			Trace.log("value = pick_value(SomeIndex, I, C)");
			Expression variablesI = ExtensionalSet.makeUniSetExpression(indicesI);
			Expression formulaC   = IntensionalSet.getCondition(intensionalSet);
			Expression value = pickValue(someIndex, variablesI, formulaC, process);
			Trace.log("// value = {}", value);
			
			if (value == null) {
				Trace.log("if value is null, return null");
			}
			else {
				Trace.log("let I' be I - {SomeIndex}");
				List<Expression> indexExpressionsIPrime = new ArrayList<Expression>(indicesI);
				indexExpressionsIPrime.remove(someIndex);
				Trace.log("// I' = {}", indexExpressionsIPrime);
				Trace.log("return pick_single_element({ (on I') Alpha[X/value] | C[X/value] })");
				Expression alphaSubX          = Substitute.replace(alpha, someIndex, value, process);
				Expression formulaCSubX       = Substitute.replace(formulaC, someIndex, value, process);
				Expression intensionalSetSubX = IntensionalSet.makeUniSetFromIndexExpressionsList(indexExpressionsIPrime, alphaSubX, formulaCSubX);
	
				result = pickSingleElement(intensionalSetSubX, process);
			}
		}
		
		Trace.out("-pick_single_element={}", result);
		
		return result;
	}
	
	/**
	 * <pre>
	 * pick_value(X, I, C)
	 * Takes a variable X, a set of variables I (which C is dependent/scoped by), and a formula C
	 * Assumes that there is a single assignment to X and I satisfying C
	 * Returns a value v such that there exists I : C <=> X = v or null if it cannot be determined.
	 * if C is a conjunction containing conjunct X = value
	 * 	    return value
	 * formula_on_X = R_formula_simplification(there exists I’ : C) // where I' is I \ {X}
	 * if X = value can be unambiguously extracted from formula_on_X
	 *      return value
	 *      
	 * // Need to use full satisfiability check to pick value
	 * possible_determined_values <- (constants of C) union ( (free variables of formula_on_X and context) \ {X}) // (1)
	 * result <- get_conditional_single_value_or_null_if_not_defined_in_all_contexts(X, formula_on_X, possible_determined_values)
	 * if result is null, return null
	 * return R_complete_simplify(result)
	 * 
	 * Implementation Notes:
	 * (1) Preference is to check constants before free variables.
	 * </pre>
	 * 
	 * @param variableX
	 *            a variable X
	 * @param variablesI
	 *            an extensional uni-set of indices variables I
	 * @param formulaC
	 *            a formula C
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return a value v such that there exists I : C <=> X = v or null if not
	 *         able to pick.
	 */
	public static Expression pickValue(Expression variableX, Expression variablesI, Expression formulaC, final RewritingProcess process) {
		Expression result = null;
		
		Trace.in("+pick_value({}, {}, {})", variableX, variablesI, formulaC);
		
		// if C is a conjunction containing conjunct X = value, return value
		result = extractValueForXFromConjunction(variableX, ExtensionalSet.getElements(variablesI), formulaC, process);
		if (result != null) {
			Trace.log("if C is a conjunction containing conjunct X = value");
			Trace.log("    return value");
		} 
		else {
			Trace.log("formula_on_X = R_formula_simplification(there exists I' : C)"); 
			List<Expression> variablesIPrime = new ArrayList<Expression>();
			for (Expression i : ExtensionalSet.getElements(variablesI)) {
				// where I' is I \ {X}
				if (!variableX.equals(i)) {
					variablesIPrime.add(i);
				}
			}
			Expression thereExists = ThereExists.make(variablesIPrime, formulaC);
			Expression formulaOnX  = process.rewrite(LBPRewriter.R_formula_simplification, thereExists);
			
			result = extractValueForXFromFormula(variableX, ExtensionalSet.getElements(variablesI), formulaOnX, process);
			if (result != null) {
				Trace.log("if X = value can be unambiguously extracted from formula_on_X");
				Trace.log("    return value");
			}
			else {
				// Need to use full satisfiability check to pick value
								
				Trace.log("possible_determined_values <- (constants of C) union ( (free variables of formula_on_X and context) \\ {X})");
				
				// (1) Preference is to check constants before free variables.
				Set<Expression> possibleDeterminedValues = new LinkedHashSet<Expression>();
				// constants of C
				possibleDeterminedValues.addAll(FormulaUtil.getConstants(formulaC, process));
				// union ( (free variables of formula_on_X and context) \\ {X})
				Expression formulaOnXAndContext = CardinalityUtil.makeAnd(formulaOnX, process.getContextualConstraint());
				possibleDeterminedValues.addAll(Expressions.freeVariables(formulaOnXAndContext, process));
				possibleDeterminedValues.remove(variableX);
				Trace.log("// possible_determined_values = {}", possibleDeterminedValues);
				
				Trace.log("result <- get_conditional_single_value_or_null_if_not_defined_in_all_contexts(X, formula_on_X, possible_determined_values)");
				result = getConditionalSingleValueOrNullIfNotDefinedInAllContexts(variableX, formulaOnX, possibleDeterminedValues, process);
				if (result == null) {
					Trace.log("if result is null, return null");
				}
				else {
					Trace.log("return R_complete_simplify(result)");
					result = process.rewrite(LBPRewriter.R_complete_simplify, result);		
				}
			}
			
		} 
		
		Trace.out("-pick_value={}", result);
		
		return result;
	}
	
	/**
	 * <pre>
	 * get_conditional_single_value_or_null_if_not_defined_in_all_contexts(X, formula_on_X, possible_values)
	 * 
	 * if possible_values is empty, return null // X can be any value not forbidden by context, so it is not constrained to a single value
	 * (first, remaining_possible_values) <- get_first_and_remaining(possible_values)
	 * condition_for_first <- R_complete_simplify(formula_on_X[X/first])
	 * if condition_for_first is 'true'
	 *     return first
	 * among_remaining <- get_conditional_single_value_or_null_if_not_defined_in_all_contexts(X, formula_on_X, remaining_possible_values) under context extended by not(condition_for_first)
	 * if among_remaining is null
	 *     return null
	 * return if condition_for_first then first else among_remaining 
	 * 
	 * Implementation Notes:
	 * (1) Preference is to check constants before free variables.
	 * </pre>
	 * 
	 * @param variableX
	 *            a variable X
	 * @param formulaOnX
	 *            a formula on X
	 * @param possibleValues
	 *            possible values for X
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return a conditional single value for X or null if not defined in all contexts.
	 */
	public static Expression getConditionalSingleValueOrNullIfNotDefinedInAllContexts(Expression variableX, Expression formulaOnX, Set<Expression> possibleValues, RewritingProcess process) {
		Expression result = null;
		
		Trace.in("+get_conditional_single_value_or_null_if_not_defined_in_all_contexts({}, {}, {}) under: {}", variableX, formulaOnX, possibleValues, process.getContextualConstraint());
		
		if (possibleValues.size() == 0) {
			Trace.log("if possible_values is empty, return null // X can be any value not forbidden by context, so it is not constrained to a single value");
			result = null;
		}
		else {
			Trace.log("(first, remaining_possible_values) <- get_first_and_remaining(possible_values)");
			Expression first = possibleValues.iterator().next();
			Set<Expression> remainingPossibleValues = new LinkedHashSet<Expression>(possibleValues);
			remainingPossibleValues.remove(first);
			Trace.log("// first                    : {}", first);
			Trace.log("// remaining_possible_values: {}", remainingPossibleValues);
			
			Trace.log("condition_for_first <- R_complete_simplify(formula_on_X[X/first])");
			Expression conditionForFirst = formulaOnX.replaceAllOccurrences(variableX, first, process);
			conditionForFirst = process.rewrite(LBPRewriter.R_complete_simplify, conditionForFirst);
			Trace.log("// condition_for_first: {}, first: {}", conditionForFirst, first);
			if (conditionForFirst.equals(Expressions.TRUE)) {
				Trace.log("if condition_for_first is \"true\"");
				Trace.log("    return first");
				result = first;
			}
			else {
				Trace.log("among_remaining <- get_conditional_single_value_or_null_if_not_defined_in_all_contexts(X, formula_on_X, remaining_possible_values) under context extended by not(condition_for_first)");
				//  under context extended by not(condition_for_first)
				RewritingProcess underNotConditionForFirst = GrinderUtil.extendContextualConstraint(CardinalityUtil.makeNot(conditionForFirst), process);
				Expression amongRemaining = getConditionalSingleValueOrNullIfNotDefinedInAllContexts(variableX, formulaOnX, remainingPossibleValues, underNotConditionForFirst);
				if (amongRemaining == null) {
					Trace.log("if among_remaining is null");
					Trace.log("    return null");
					result = null;
				}
				else {
					Trace.log("return if condition_for_first then first else among_remaining");
					result = IfThenElse.make(conditionForFirst, first, amongRemaining);
				}				
			}
		}
				
		Trace.out("-get_conditional_single_value_or_null_if_not_defined_in_all_contexts={}", result);
		
		return result;
	}
	
	/**
	 * Returns the set of random variables used in an expression assumed to have the same
	 * se of logical variables everywhere, that is, an expression that does not quantify over logical variables.
	 */
	public static Expression getRandomVariablesUsedIn(Expression expression, RewritingProcess process) {
		SubExpressionsDepthFirstIterator subExpressionsDepthFirstIterator =
			new SubExpressionsDepthFirstIterator(expression);

		Iterator<Expression> randomVariableValuesIterator =
			new PredicateIterator<Expression>(
					subExpressionsDepthFirstIterator,
					new com.sri.ai.praise.model.IsRandomVariableValueExpression(process));

		List<Expression> randomVariables =
			Util.mapIntoList(
					randomVariableValuesIterator,
					new Apply(BracketedExpressionSubExpressionsProvider.SYNTAX_TREE_LABEL));

		// Ensure duplicates are removed (want to maintain order).
		randomVariables   = new ArrayList<Expression>(new LinkedHashSet<Expression>(randomVariables));
		Expression uniset = ExtensionalSet.makeUniSet(randomVariables);
		return uniset;
	}
	
	/**
	 * Detects pairs of random variable value expressions and their respective contexts that do not unify with a given random variable expression.
	 * This method can be used to test message values against the occurrence of random variable values other than
	 * the one they are supposed to use (destination random variable value, if this is a message to an RV, or
	 * origin random variable value, if this is a message *from* an RV).
	 * STILL BEING TESTED.
	 */
	public static List<Pair<Expression,Expression>> findRandomVariableValueExpressionsThatAreNotNecessarilyTheSameAsAGivenOne(Expression expression, final Expression randomVariableValue, RewritingProcess process) {

		final List<Pair<Expression,Expression>> result = new LinkedList<Pair<Expression,Expression>>();
		
		ReplacementFunctionWithContextuallyUpdatedProcess searchFunction = new AbstractReplacementFunctionWithContextuallyUpdatedProcess() {
			public Expression apply(Expression subExpression, RewritingProcess process) {
				if (LPIUtil.isRandomVariableValueExpression(subExpression, process) && ! subExpression.equals(randomVariableValue)) {
					Expression randomVariable              = BracketedExpressionSubExpressionsProvider.make(randomVariableValue);
					Expression subExpressionRandomVariable = BracketedExpressionSubExpressionsProvider.make(subExpression);
					Expression comparison                  = process.rewrite(LBPRewriter.R_complete_simplify, Equality.make(subExpressionRandomVariable, randomVariable));
					if ( ! comparison.equals(Expressions.TRUE)) { // that is, it is not guaranteed to be the same random variable value expression in this context
						result.add(new Pair<Expression, Expression>(subExpression, process.getContextualConstraint()));
					}
				}
				return subExpression; // never replaces anything, just collects information
			}
		};
		
		expression.replaceAllOccurrences(searchFunction, process);
		
		return result;
	}
	
	//
	// PRIVATE
	//
	private static Expression extractValueForXFromConjunction(Expression variableX, List<Expression> variablesI, Expression expression, RewritingProcess process) {
		Expression result = null;
		
		if (And.isConjunction(expression) || expression.hasFunctor(FunctorConstants.EQUAL)) {
			result = extractValueForXFromFormula(variableX, variablesI, expression, process);
		}
		
		return result;
	}
	
	private static Expression extractValueForXFromFormula(Expression variableX, List<Expression> variablesI, Expression formula, RewritingProcess process) {
		Expression result = null;
		
		for (Expression possibleValue : extractPossibleValuesForXFromFormula(variableX, formula, process)) {
			if (process.isConstant(possibleValue)) {
				result = possibleValue;
			} 
			else {
				// if assigned already, then let constants or prior
				// variable assignments take precedence
				if (result == null) {
					// Only assign free variables
					if (isIndependentOf(possibleValue, variablesI, process)) {					
						result = possibleValue;
					}
				}
			}
		}
		
		return result;
	}
		
	private static Set<Expression> extractPossibleValuesForXFromFormula(Expression variableX, Expression formula, RewritingProcess process) {
		Set<Expression> result = new LinkedHashSet<Expression>();
		
		if (And.isConjunction(formula)) {
			for (Expression conjunct : And.getConjuncts(formula)) {
				result.addAll(extractPossibleValuesForXFromFormula(variableX, conjunct, process));
			}
		} 
		else if (formula.hasFunctor(FunctorConstants.EQUAL)) {
			result = extractValuesForXFromEquality(variableX, formula, process);
		} 
		else if (Or.isDisjunction(formula)) {
			// This logic is to handle expressions of the form:
			// X = X' = person1 or X = X' = person2 or X = X' = person3
			// which, if variableX = X should give me:
			// X' 
			// as its common to all 3 disjuncts.
			boolean first = true;
			for (Expression disjunct : formula.getArguments()) {
				Set<Expression> disjunctResult = extractPossibleValuesForXFromFormula(variableX, disjunct, process);
				if (first) {
					first = false;
					result.addAll(disjunctResult);
				} 
				else {
					result.retainAll(disjunctResult);
					if (result.isEmpty()) {
						break;
					}
				}
			}
		}
		
		return result;
	}
	
	private static Set<Expression> extractValuesForXFromEquality(Expression variableX, Expression equality, RewritingProcess process) {
		Set<Expression> result = new LinkedHashSet<Expression>();
		
		boolean containsX = false;
		// Want to handle equalities on more than two variables, e.g.:
		// X = X' = constant
		for (Expression possibleValue : equality.getArguments()) {
			if (possibleValue.equals(variableX)) {
				containsX = true;
			} 
			else {
				result.add(possibleValue);
			}
		}
		
		if (!containsX) {
			result.clear();
		}
		
		return result;
	}
	
	private static boolean isIndependentOf(Expression alpha, List<Expression> indicesI, RewritingProcess process) {
		boolean result = false;
		
		Set<Expression> alphaFreeVariables            = new HashSet<Expression>();
		Set<Expression> indexExpressionsFreeVariables = new HashSet<Expression>();
		
		// Collect the free variables
		alphaFreeVariables.addAll(Expressions.freeVariables(alpha, process));
		indexExpressionsFreeVariables.addAll(indicesI);
		
		// check the intersection
		alphaFreeVariables.retainAll(indexExpressionsFreeVariables);
		if (alphaFreeVariables.size() == 0) {
			// to see if its empty
			result = true; // is independent if intersection if empty
		}
		
		return result;
	}
	
	/**
	 * Given a simplified conditional expression with conditions on equalities and RVs, with no conditions involving both,
	 * returns an equivalent expression equality conditions are always above RV ones.
	 */
	public static Expression moveAllRandomVariableConditionsDown(Expression expression, RewritingProcess process) {
		TotalRewriter totalRewriter =
				new TotalRewriter(
						"Moving conditions on RVs down in LBPUtil",
						Util.list( (Rewriter) new MoveRandomVariableValueExpressionConditionDown(),
						           (Rewriter) new ExternalizeConditionalOnLogicalVariables())
						);
		Expression result = totalRewriter.rewrite(expression, process);
		return result;
	}
}
