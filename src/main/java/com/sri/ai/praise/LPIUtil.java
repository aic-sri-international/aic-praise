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

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.BracketedExpression;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.ReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.core.AbstractReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Apply;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.IsApplicationOf;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.FunctionSignature;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.PickSingleElement;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.StandardizedApartFrom;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.lbp.core.IsDeterministicBooleanMessageValue;
import com.sri.ai.praise.lbp.core.MessageValueOnBooleanRandomVariableValueWithZeroInOneBranchIsDeterministic;
import com.sri.ai.praise.model.IsRandomVariableValueExpression;
import com.sri.ai.praise.model.RandomPredicateCatalog;
import com.sri.ai.praise.model.RandomVariableDeclaration;
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
		
		Expression randomVariableValueExpression = ((BracketedExpression) randomVariable).getInnerExpression();
		//
		RandomPredicateCatalog           catalog               = RandomPredicateCatalog.getFromBracketedModel(process);
		List<Expression>                 occurrenceConditions  = new LinkedList<Expression>();
		SubExpressionsDepthFirstIterator subExpressionIterator = new SubExpressionsDepthFirstIterator(expression);
		
		while (subExpressionIterator.hasNext()) {
			Expression subExpression = subExpressionIterator.next();
			if (catalog.contains(new FunctionSignature(subExpression))
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
	 * @param indexType
	 *            the type 'S'.
	 * @param message
	 *            the message of the form m_B<-A.
	 * @param condition
	 *            the condition C on the scoping expression.
	 * @return a product of message of the form: product({{ (on A in S) m_B<-A | C }}).
	 */
	public static Expression makeProductOfMessages(Expression index,
			Expression indexType, Expression message, Expression condition) {

		Expression prodIntensionalSet = new DefaultIntensionalMultiSet(list(apply("in", index, indexType)), message, condition);
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.PRODUCT,
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
			result = ((BracketedExpression) randomVariable).getInnerExpression();
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
		result = ((BracketedExpression) factor).getInnerExpression();

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
					"Random Variable is not a random variable expression of the form [Ev]: "
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
		if (!BracketedExpressionSubExpressionsProvider.isBracketedExpression(factor)
				|| BracketedExpressionSubExpressionsProvider.isRandomVariable(factor, process)) {
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
			Sets.isIntensionalSet(expression.get(0)) &&
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
		FunctionSignature randomPredicate = new FunctionSignature(expression);
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
					Expression bracketedValue = ((BracketedExpression) element).getInnerExpression();
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
	 * the type size is not known.
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
		else if (expression.getSyntacticFormType().equals("Symbol")) {
			if (expression.getValue() instanceof Number) {
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
		
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(LPIUtil.FUNCTOR_CONVEX_HULL, ExtensionalSet.makeUniSet(elements));
		
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
			result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.UNION, elements.toArray());
		} 
		else {
			// We are assuming beingComputed is a set or a conditional, 
			// so we create a top level union with the set/conditional and by
			result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.UNION, beingComputed, by);
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
	 * @param indexExpressions
	 *            the index expressions of the intensional set to be unioned with
	 *            beingComputed.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return R_basic(beingComputed union {{ by | C }}_I)
	 */
	public static Expression extendBeingComputedWithIntensionalMultiSet(Expression beingComputed, Expression by, Expression conditionC, 
			IndexExpressionsSet indexExpressions, RewritingProcess process) {
		Expression result = beingComputed;
		
		if (!Tuple.isTuple(by) || Tuple.size(by) != 2) {
			throw new IllegalArgumentException("by must be a tuple (to, from):"+by);
		}
		
		Expression intensionalSetBy = new DefaultIntensionalMultiSet(indexExpressions, by, conditionC);
				
		if (Sets.isEmptySet(beingComputed)) {
			result = intensionalSetBy;
		} 
		else if (Expressions.hasFunctor(beingComputed, FunctorConstants.UNION)) {
			List<Expression> elements = new ArrayList<Expression>(beingComputed.getArguments());
			elements.add(intensionalSetBy);
			result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.UNION, elements.toArray());
		} 
		else {
			// We are assuming beingComputed is a set or a conditional, 
			// so we create a top level union with the set/conditional and by intensional set
			result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.UNION, beingComputed, intensionalSetBy);
		}
		
		return result;
	}

	/**
	 * Indicates whether an expression is a message definition ("message to Alpha from Beta" or "previous message to Alpha from Beta").
	 */
	public static boolean isMessageDefinition(Expression expression) {
		boolean result = expression.hasFunctor(FUNCTOR_PREVIOUS_MSG_TO_FROM) || expression.hasFunctor(FUNCTOR_MSG_TO_FROM); 
		return result;
	}

	public static boolean isPreviousMessageDefinition(Expression expression) {
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
		
		Expression setDiff = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.SET_DIFFERENCE, extensionalSetA, extensionalSetB);
		Expression result  = Tuple.make(setDiff, i, j);

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
		Expression setDiff = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.SET_DIFFERENCE, extensionalSet, intensionalSet);
		Expression result  = Tuple.make(setDiff, i);
		
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
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(LPIUtil.FUNCTOR_IN, Arrays.asList(alpha, set));
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
		Expression result = Tuple.make(msgToV_F, Expressions.TRUE, makeScopingSyntaxTree(new ExtensionalIndexExpressionsSet(new ArrayList<Expression>())), beingComputed);
		return result;
	}
	
	/** Makes a scoping expression out of a list of scoping variables. */
	private static SyntaxTree makeScopingSyntaxTree(IndexExpressionsSet indexExpressions) {
		List<Expression> indexExpressionsList = ((ExtensionalIndexExpressionsSet) indexExpressions).getList();
		Expression kleeneListExpression = Expressions.makeKleeneListIfNeeded(indexExpressionsList);
		SyntaxTree kleeneListSyntaxTree = kleeneListExpression.getSyntaxTree();
		SyntaxTree result = SyntaxTrees.makeCompoundSyntaxTree(IntensionalSet.SCOPED_VARIABLES_LABEL, kleeneListSyntaxTree);
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
	 * @param expressionI
	 *            the index expressions on the intensional set from which the
	 *            representative factor F was selected.
	 * @param beingComputed
	 *            is a conditional union of sets of pairs of nodes, representing
	 *            messages being computed and depending on this computation.
	 * @return a tuple argument of the form: (m_V<-F, C, I, beingComputed)
	 */
	public static Expression argForMessageToVariableFromFactorRewriteCall(Expression msgToV_F,
			Expression conditionC, List<Expression> expressionI, Expression beingComputed) {
		Expression result = Tuple.make(msgToV_F, conditionC, Tuple.make(expressionI), beingComputed);
		
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
	 * R_normalize_message([ v ], E).<br>
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
		Expression times  = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.TIMES, Arrays.asList(conditionalMessage, productOfFactorsToVariable));
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
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.SET_DIFFERENCE, set1, set2);
		
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
	 * Returns the set of random variables used in an expression assumed to have the same
	 * set of logical variables everywhere, that is, an expression that does not quantify over logical variables.
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
	 */
	public static List<Pair<Expression,Expression>> findRandomVariableValueExpressionsThatAreNotNecessarilyTheSameAsAGivenOne(Expression expression, final Expression randomVariableValue, RewritingProcess process) {

		final List<Pair<Expression,Expression>> result = new LinkedList<Pair<Expression,Expression>>();
		
		ReplacementFunctionWithContextuallyUpdatedProcess searchFunction = new AbstractReplacementFunctionWithContextuallyUpdatedProcess() {
			@Override
			public Expression apply(Expression subExpression, RewritingProcess process) {
				if (LPIUtil.isRandomVariableValueExpression(subExpression, process) && ! subExpression.equals(randomVariableValue)) {
					Expression randomVariable              = BracketedExpressionSubExpressionsProvider.make(randomVariableValue);
					Expression subExpressionRandomVariable = BracketedExpressionSubExpressionsProvider.make(subExpression);
					Expression comparison                  = process.rewrite(LBPRewriter.R_complete_normalize, Equality.make(subExpressionRandomVariable, randomVariable));
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
	@SuppressWarnings("unused")
	private static Expression extractValueForXFromConjunction(Expression variableX, List<Expression> variablesI, Expression expression, RewritingProcess process) {
		Expression result = null;
		
		if (And.isConjunction(expression) || expression.hasFunctor(FunctorConstants.EQUAL)) {
			result = PickSingleElement.extractValueForXFromFormula(variableX, variablesI, expression, process);
		}
		
		return result;
	}
	
	/**
	 * Same as {@link #extendContextualSymbolsAndConstraintWithIntensionalSetInferringDomainsFromUsageInRandomVariables(Expression, RewritingProcess)},
	 * but only for the indices (that is, it does not extend the contextual constraint with the intensional set's condition.
	 */
	public static RewritingProcess extendContextualSymbolsWithIntensionalSetIndicesInferringDomainsFromUsageInRandomVariables(Expression intensionalSet, RewritingProcess process) {
		Map<Expression, Expression> indexToTypeMap =
				DetermineSortsOfLogicalVariables.getIndicesTypeMapFromIntensionalSetIndexExpressionsAndUsageInRandomVariables(intensionalSet, process);
		RewritingProcess result = GrinderUtil.extendContextualSymbolsAndConstraint(indexToTypeMap, Expressions.TRUE, process);
		return result;
	}

	/**
	 * Identifies logical variables in a given expression that are free variables and returns process with them as contextual symbols,
	 * using their usage as random variable value expression arguments to infer their type.
	 */
	public static RewritingProcess extendContextualSymbolsWithFreeVariablesInferringDomainsFromUsageInRandomVariables(Expression expression, RewritingProcess process) {
		Map<Expression, Expression> freeSymbolsAndTypes = DetermineSortsOfLogicalVariables.getFreeSymbolsAndTypesFromUsageInRandomVariables(expression, null, process);
		RewritingProcess result = GrinderUtil.extendContextualSymbols(freeSymbolsAndTypes, process);
		return result;
	}

	/**
	 * @return the random variable declaration associated with a name and arity, or <code>null</code> if there is none.
	 */
	public static RandomVariableDeclaration getRandomVariableDeclaration(String name, int arity, Collection<RandomVariableDeclaration> randomVariableDeclarations) {
		for (RandomVariableDeclaration declaration : randomVariableDeclarations) {
			if (declaration.getName().equals(name) && declaration.getArityValue() == arity) {
				return declaration;
			}
		}
		return null;
	}

	/**
	 * @return the random variable declaration associated with a random variable value expression,
	 * or <code>null</code> if it is not such an expression, or there is none.
	 */
	public static RandomVariableDeclaration getRandomVariableDeclaration(Expression randomVariableValueExpression, Collection<RandomVariableDeclaration> randomVariableDeclarations) {
		RandomVariableDeclaration result = null;
		if (randomVariableValueExpression.getSyntacticFormType().equals("Function application")) {
			result =
					getRandomVariableDeclaration(
							randomVariableValueExpression.getFunctor().toString(),
							randomVariableValueExpression.numberOfArguments(),
							randomVariableDeclarations);
		}
		return result;
	}

	public static IndexExpressionsSet getIndexExpressionsFromRandomVariableUsage(Expression expression, Set<Expression> randomVariableDeclarationsExpressions, RewritingProcess process) {
		Map<Expression, Expression> freeSymbolsAndTypes = DetermineSortsOfLogicalVariables.getFreeSymbolsAndTypesFromUsageInRandomVariables(expression, randomVariableDeclarationsExpressions, process);
		IndexExpressionsSet indexExpressions = IndexExpressions.getIndexExpressionsFromSymbolsAndTypes(freeSymbolsAndTypes);
		return indexExpressions;
	}

	public static LinkedHashSet<Expression> getRandomVariableValueExpressions(Expression expression, final RewritingProcess process) {
		return Expressions.getSubExpressionsSatisfying(expression, new Predicate<Expression>() {
			@Override
			public boolean apply(Expression arg) {
				boolean result = isRandomVariableValueExpression(arg, process);
				return result;
			}
		});
	}

	/** Convenience method creating an expression of the type "belief([query])" given an expression 'query'. */
	public static Expression makeBelief(Expression query) {
		Expression randomVariable = BracketedExpressionSubExpressionsProvider.make(query);
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(LPIUtil.FUNCTOR_BELIEF, randomVariable);
		return result;
	}

	
	// BEGINNING OF SIMPLE MESSAGES TREATMENT
	
	/**
	 * Receives a message and returns its value in terms of the random variable value,
	 * if the message is simple, and the original message definition otherwise;
	 * a message is simple if its (up to normalization) value can be determined from the message definition alone, without involving other messages;
	 * this is possible when the message is from a factor [ if RV then V1 else V2 ] to a random variable [RV],
	 * or from a random variable [RV] to a <i>deterministic</i> factor [ if RV then V1 else V2 ] (V1 or V2 is equal to 0),
	 * assuming that the probabilistic model being used is consistent (that is, it will not lend total mass to another value of the same random variable).
	 */
	public static Expression valueOfSimpleMessageOrSelfIfNotSimpleMessage(Expression message, RewritingProcess process) {
		Expression result = message;
		if (LPIUtil.isRandomVariable(message.get(0), process)) {
			Expression randomVariable      = message.get(0);
			Expression factor              = message.get(1);
			Expression factorValue         = ((BracketedExpression) factor).getInnerExpression();
			if (factorValueIsSimpleWithRespectToRandomVariable(factorValue, randomVariable, process)) {
				result = factorValue;
			}
		}
		else {
			Expression factor         = message.get(0);
			Expression factorValue    = ((BracketedExpression) factor).getInnerExpression();
			if (IsDeterministicBooleanMessageValue.isDeterministicMessageValue(factorValue, process)) {
				result = factorValue;
			}
		}
		return result;
	}

	/**
	 * Indicates that a factor is simple with respect to a random variable, that is,
	 * an expression of the form <if RVV then V1 else V2> for RVV the random variable value.
	 */
	public static boolean factorValueIsSimpleWithRespectToRandomVariable(Expression factorValue, Expression randomVariable, RewritingProcess process) {
		Expression randomVariableValue = ((BracketedExpression) randomVariable).getInnerExpression();
		boolean result = factorValueIsSimpleWithRespectToRandomVariable(factorValue, randomVariable, randomVariableValue, process);
		return result;
	}
	
	/**
	 * Slightly more efficient version of {@link #factorValueIsSimpleWithRespectToRandomVariable(Expression, Expression, RewritingProcess)}.
	 */
	private static boolean factorValueIsSimpleWithRespectToRandomVariable(Expression factorValue, Expression randomVariable, Expression randomVariableValue, RewritingProcess process) {
		boolean result = false;
		
		if (IfThenElse.isIfThenElse(factorValue)) {
			factorValue = IfThenElse.equivalentWithNonNegatedCondition(factorValue);
			Expression condition = IfThenElse.getCondition(factorValue);
			
			boolean conditionEqualsRandomVariableValue = false;
			if (condition.equals(randomVariableValue)) {
				conditionEqualsRandomVariableValue = true;
			}
			else {
				Expression randomVariableConditionInFactor = BracketedExpressionSubExpressionsProvider.make(condition);
				conditionEqualsRandomVariableValue =
						process.rewrite(LBPRewriter.R_complete_normalize, Equality.make(randomVariableConditionInFactor, randomVariable)).equals(Expressions.TRUE);
			}
			
			result =
					conditionEqualsRandomVariableValue
					&& factorValueIsSimpleWithRespectToRandomVariable(IfThenElse.getThenBranch(factorValue), randomVariable, randomVariableValue, process)
					&& factorValueIsSimpleWithRespectToRandomVariable(IfThenElse.getElseBranch(factorValue), randomVariable, randomVariableValue, process);
		}
		else if (Expressions.isNumber(factorValue) || ! LPIUtil.containsRandomVariableValueExpression(factorValue, process)) {
			result = true;
		}
		
		return result;
	}

	// END OF SIMPLE MESSAGES TREATMENT

	/**
	 * Key for RewritingProcess global object indicating which random variable is being normalized at a certain point.
	 * At the time of this writing, this is used by rewriter {@link MessageValueOnBooleanRandomVariableValueWithZeroInOneBranchIsDeterministic}.
	 * @see #setRandomVariableBeingNormalizedAndReturnPreviousOne(Expression, RewritingProcess)
	 * @see #restorePreviousRandomVariableBeingNormalized(Expression, RewritingProcess)
	 */
	public static final String RANDOM_VARIABLE_BEING_NORMALIZED = "random variable being normalized";

	/**
	 * Key for RewritingProcess global object indicating which random variable is being normalized at a certain point.
	 * At the time of this writing, this is used by rewriter {@link MessageValueOnBooleanRandomVariableValueWithZeroInOneBranchIsDeterministic}.
	 * @see #RANDOM_VARIABLE_BEING_NORMALIZED
	 * @see #restorePreviousRandomVariableBeingNormalized(Expression, RewritingProcess)
	 */
	public static Expression setRandomVariableBeingNormalizedAndReturnPreviousOne(Expression randomVariable, RewritingProcess process) {
		Expression previousRandomVariableBeingNormalized = (Expression) process.getGlobalObject(RANDOM_VARIABLE_BEING_NORMALIZED);
		process.putGlobalObject(RANDOM_VARIABLE_BEING_NORMALIZED, randomVariable);
		return previousRandomVariableBeingNormalized;
	}

	/**
	 * Key for RewritingProcess global object indicating which random variable is being normalized at a certain point.
	 * At the time of this writing, this is used by rewriter {@link MessageValueOnBooleanRandomVariableValueWithZeroInOneBranchIsDeterministic}.
	 * @see #RANDOM_VARIABLE_BEING_NORMALIZED
	 * @see #setRandomVariableBeingNormalizedAndReturnPreviousOne(Expression, RewritingProcess)
	 */
	public static void restorePreviousRandomVariableBeingNormalized(Expression previousRandomVariableBeingNormalized, RewritingProcess process) {
		if (previousRandomVariableBeingNormalized == null) { // this check is needed in case the global objects map being used is of a type that does not store null values.
			process.getGlobalObjects().remove(RANDOM_VARIABLE_BEING_NORMALIZED);
		}
		else {
			process.putGlobalObject(RANDOM_VARIABLE_BEING_NORMALIZED, previousRandomVariableBeingNormalized);
		}
	}
	
	/**
	 * Given an intensional set <code>M = {(on I) (key, value) | C }</code> of 2-tuples,
	 * where <code>key</code> and <code>value</code> are arbitrary expressions
	 * and such that, for every <code>key</code> there is a single <code>value</code> such that <code>(key,value) in M</code>,
	 * and an expression <code>key'</code>,
	 * returns the simplification of the (either singleton or empty) set <code>{(on I) value | C and key = key'}</code>
	 * (if M is a multi-set, then this is either a multiset with multiple copies of a single value, or an empty multi-set).
	 * Note that, due to the uniqueness of <code>value</code> given <code>key</code>,
	 * the simplified set will have no indices (they will be equated to the corresponding values in <code>key'</code>
	 * and eliminated by the simplification).
	 * <p>
	 * The effect is to look at the intensional set as an intensional representation of a map and
	 * retrieve a (possibly conditional) singleton set containing the value of a given key
	 * (which can contain free variables itself).
	 * For example, looking up <code>Y</code> in <code>{ (on X) (X, X + 1) | X != a }</code>
	 * returns <code>{ (on ) Y + 1 | Y != a }</code>.
	 * This will be the singleton set <code>{Y + 1}</code> if <code>Y != a</code>,
	 * and the empty set otherwise.
	 * <p>
	 * The resulting set may have a condition, but it will be either a tautology or a contradiction,
	 * although it may not have been simplified to <code>true</code> or <code>false</code>
	 * because the method uses non-complete simplification for efficiency's sake
	 * (the invoking code can always perform the complete simplification itself if needed).
	 * <p>
	 * Note also that the result may be a <i>conditional</i> set depending on the simplification rules employed.
	 * For example, <code>{ (on ) Y + 1 | Y != a }</code> may be simplified to
	 * <code>if Y != a then { Y + 1 } else { }</code> (again, depending on the simplification rules used).
	 * <p>
	 */
	public static Expression symbolicLookUp(Expression set, Expression keyPrime, RewritingProcess process) {
		
		set = StandardizedApartFrom.standardizedApartFrom(set, keyPrime, process);
		
		Expression       head      = ((IntensionalSet) set).getHead();
		Expression       key       = Tuple.get(head, 0);
		Expression       value     = Tuple.get(head, 1);
		Expression       condition = ((IntensionalSet) set).getCondition();
		
		Expression newCondition = And.make(condition, Equality.make(key, keyPrime));
		Expression result       = ((IntensionalSet)set).setHeadAndCondition(value, newCondition);
		           result       = process.rewrite(LBPRewriter.R_simplify, result);
		
		return result;
	}
}
