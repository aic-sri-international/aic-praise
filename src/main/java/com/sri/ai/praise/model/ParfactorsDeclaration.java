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

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.praise.BracketedExpressionSubExpressionsProvider;
import com.sri.ai.util.Util;

/**
 * A collection of extensionally or intensionally defined sets of parameterized
 * factors (parfactor) declarations. The basic structure of a parfactors
 * declaration is as follows:<br>
 * 
 * <pre>
 * parfactors(  
 *     'union or partition'(parfactor1,...,parfactorN)
 * )
 * 
 * 'union or parition'(...) or {...} or {{...}}:
 * . mandatory: a single union or partition of parfactors decribing the model
 *   or alternatively a list of parfactors.
 *   
 *   Note: it is legal to construct a parfactor declaration directly using a 
 *   union, partition, or a single uni or multiset parfactor declaration, 
 *   instead of explicitly creating a surrounding 'parfactors()' expression.
 *   
 * . parfactor(s): (extensionally or intensionally defined sets of parameterized factors) 
 *   ::defined via an extensional (uni or multi) set:
 *   - e.g:
 *     
 *     {[if q(a1) then 1 else 0], [if q(a2) then 1 else 0]}
 *     
 *     each element in the set must be a legal parfactor expression (i.e.
 *     a bracketed expression that does not equate to a random variable
 *     expression in the model).
 *     
 *   ::defined via an intensional (uni or multi) set:
 *   - e.g:
 *    
 *     {{ (on X in People, Y) [if p(X) and q(X, Y) then 1 else 0] | X != a1}}
 *     
 *     The head of the intensional set expression must be a legal parfactor 
 *     expression. If the type of the index is not specified (as in 
 *     the index Y in the above e.g.) it must be possible to determine it 
 *     uniquely from its usage within the parfactor expression in the head 
 *     of the intensional set expression.
 * 
 * </pre>
 * 
 * <b>Note:</b> It is legal to have free logical variables in a parfactor
 * declaration, for example, one could use a free variable to represent an
 * unknown entity:
 * 
 * <pre>
 * {{ (on Person) 
 *    [ if Person = Boss then if happy(Person) then 1 else 0 else if happy(Person) then 0 else 1 ] }}
 * </pre>
 * 
 * and then a query:
 * 
 * <pre>
 * happy(tom)
 * </pre>
 * 
 * would provide an answer:
 * 
 * <pre>
 * if tom = Boss then if happy(tom) then 1 else 0 else if happy(tom) then 0 else 1
 * </pre>
 * 
 * 
 * @author oreilly
 * 
 */
@Beta
public class ParfactorsDeclaration {
	//
	public static final String FUNCTOR_PARFACTORS_DECLARATION = "parfactors";
	//
	private Expression parfactorsDefinition = null;
	private List<Expression> parfactors = new ArrayList<Expression>();

	/**
	 * Construct a parfactors declaration from a declaring expression.
	 * 
	 * @param expression
	 *            An expression in one of the following forms:<br>
	 * 
	 *            <ul>
	 *            <li>parfactors(parfactor1,...,parfactorN)</li>
	 *            <li>parfactors(union(parfactor1,...,parfactorN))</li>
	 *            <li>parfactors(partition(parfactor1,...,parfactorN))</li>
	 *            <li>union(parfactor1,...,parfactorN)</li>
	 *            <li>partition(parfactor1,...,parfactorN)</li>
	 *            </ul>
	 * 
	 * @throws IllegalArgumentException
	 *             if the expression is not a legal parfactors declaration.
	 */
	public ParfactorsDeclaration(Expression expression) {
		this.parfactors.addAll(collectAndAssertParfactorsOk(expression));
		parfactorsDefinition = expression;
	}

	/**
	 * 
	 * @return the expression representing the definition of the parfactors.
	 */
	public Expression getDefinition() {
		return parfactorsDefinition;
	}

	/**
	 * 
	 * @return the parfactors associated with this declaration.
	 */
	public List<Expression> getParfactors() {
		return Collections.unmodifiableList(parfactors);
	}

	//
	// STATIC UTILITY EXPRESSION
	//

	/**
	 * Determine if an expression is a legal parfactors declaration.
	 * 
	 * @param expression
	 *            an expression to be checked whether or not it is a legal
	 *            parfactors declaration.
	 * @return true if a legal parfactors declaration, false otherwise.
	 */
	public static boolean isParfactorsDeclaration(Expression expression) {
		boolean isParfactorsDeclaration = false;

		try {
			collectAndAssertParfactorsOk(expression);
			isParfactorsDeclaration = true;
		} catch (IllegalArgumentException iae) {
			isParfactorsDeclaration = false;
		}

		return isParfactorsDeclaration;
	}
	
	/**
	 * Check if a parfactors declaration is comprised solely of parfactors considered legal
	 * to be used for evidence purposes.
	 * 
	 * @param declaration
	 *            the parfactors declaration.
	 * @param process
	 *            the rewriting process in which the evidence is being checked.
	 * @return true if an evidence only parfactors declaration, false otherwise.
	 */
	public static boolean isEvidenceOnly(ParfactorsDeclaration declaration, RewritingProcess process) {
		boolean result = true;
		
		for (Expression parfactor : declaration.getParfactors()) {
			if (!isEvidenceOnlyParfactor(parfactor, process)) {
				result = false;
				break;
			}
		}
		
		return result;
	}

	/**
	 * Determine if an expression takes on the legal form of an extensionally or
	 * intensionally defined set of parameterized factors (i.e. a parfactor).
	 * Ensures that the elements of extensionally defined sets are bracketed
	 * expressions. Similarly for the head of an intensional set. However, it
	 * does not check whether or not the expression inside the brackets is legal
	 * (e.g. could be an random variable and not a parfactor) as more context
	 * (e.g. a full model) is required to determine this.
	 * 
	 * @param expression
	 *            an expression to be checked whether or not it is a legal
	 *            parfactor expression.
	 * @return true if the expression takes the form of a parfactor, false
	 *         otherwise.
	 */
	public static boolean isParfactor(Expression expression) {
		return isParfactor(expression, null);
	}

	/**
	 * Determine if an expression takes on the legal form of an extensionally or
	 * intensionally defined set of parameterized factors (i.e. a parfactor).
	 * Ensures that the elements of extensionally defined sets are bracketed
	 * expressions. Similarly for the head of an intensional set. If a rewriting
	 * process is passed to this method (optional), it does check whether or not
	 * the expression inside the brackets is legal (e.g. could be a random
	 * variable and not a parfactor).
	 * 
	 * @param expression
	 *            an expression to be checked whether or not it is a legal
	 *            parfactor expression.
	 * @param process
	 *            an optional (can be null) rewriting process which will permit
	 *            checking whether or not a bracketed expression is a random
	 *            variable or a parfactor.
	 * @return true if the expression takes the form of a parfactor, false
	 *         otherwise.
	 */
	public static boolean isParfactor(Expression expression,
			RewritingProcess process) {
		boolean isParfactor = false;

		// Handle the extensional case
		if (Sets.isExtensionalSet(expression)) {
			// Assume ok but check that each element is
			// a bracketed expression at minimum.
			isParfactor = true;
			for (Expression element : ExtensionalSet.getElements(expression)) {
				if (!BracketedExpressionSubExpressionsProvider.isBracketedExpression(element)) {
					isParfactor = false;
					break;
				} 
				else if (process != null) { 
					// If I have a process I can ensure its not a random variable
					if (BracketedExpressionSubExpressionsProvider.isRandomVariable(element, process)) {
						isParfactor = false;
						break;
					}
				}
			}
		}
		// Handle the intensional case
		else if (Sets.isIntensionalSet(expression)) {
			// Ensure the head expression is a bracketed expression at minimum
			Expression head = ((IntensionalSet) expression).getHead();
			if (BracketedExpressionSubExpressionsProvider.isBracketedExpression(head)) {
				// If I have a process I can ensure its not a random variable
				if (process != null) {
					if (!BracketedExpressionSubExpressionsProvider.isRandomVariable(head, process)) {
						isParfactor = true;
					}
				} 
				else {
					isParfactor = true;
				}
			}
		}

		return isParfactor;
	}
	
	/**
	 * Check if the parfactor expression passed can be interpreted as
	 * representing evidence.
	 * 
	 * @param parfactor
	 *            the parfactor to be tested.
	 * 
	 * @param process
	 *            the rewriting process in which the parfactor is being used.
	 * @return true if the parfactor can be considered to contain only evidence,
	 *         false otherwise.
	 */
	public static boolean isEvidenceOnlyParfactor(Expression parfactor,
			RewritingProcess process) {
		// Any legal parfactor can be considered evidence
		return isParfactor(parfactor, process);
	}

	/**
	 * Convenience routine for constructing a parfactors declaration from an
	 * array of parfactors.
	 * 
	 * @param parfactors
	 * @return a Parfactors declaration constructed from the provided
	 *         parfactors.
	 * @throws IllegalArgumentException
	 *             if any of the parfactors is not an actual parfactor.
	 */
	public static ParfactorsDeclaration makeParfactorsDeclaration(
			Expression... parfactors) {
		// Ensure a parfactors
		for (int i = 0; i < parfactors.length; i++) {
			if (!isParfactor(parfactors[i])) {
				throw new IllegalArgumentException("Not a legal parfactor:"
						+ parfactors[i]);
			}
		}

		Expression parfactorsDefinition = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
				ParfactorsDeclaration.FUNCTOR_PARFACTORS_DECLARATION,
				(Object[]) parfactors);

		ParfactorsDeclaration result = new ParfactorsDeclaration(
				parfactorsDefinition);

		return result;
	}

	//
	// PRIVATE METHODS
	//
	private static List<Expression> collectAndAssertParfactorsOk(Expression expression) {
		List<Expression> collectedParfactors = new ArrayList<Expression>();
		boolean illegal = true;
		List<Expression> notParfactors = new LinkedList<Expression>();

		Expression unionOrPartition = expression;
		if (Expressions.hasFunctor(expression, FUNCTOR_PARFACTORS_DECLARATION)) {
			// Can only have a single argument if defined via:
			// parfactors('union | partition'());
			if (expression.numberOfArguments() == 1) {
				if (Sets.isSet(expression.get(0))) {
					// Is a set argument therefore treat as a single element
					// union for processing purposes.
					unionOrPartition = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.UNION,
							expression.getArguments().toArray());
				} 
				else {
					unionOrPartition = expression.get(0);
				}
			}
			// if no arguments default to an empty union
			else if (expression.numberOfArguments() == 0) {
				unionOrPartition = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.UNION);
			}
			// more than 1 argument assume are all parfactors
			// and add to a union
			else {
				unionOrPartition = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.UNION,
						expression.getArguments().toArray());
			}
		} 
		else {
			unionOrPartition = expression;
			if (Sets.isSet(unionOrPartition)) {
				// Is a set argument therefore treat as a single element
				// union for processing purposes.
				unionOrPartition = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.UNION, unionOrPartition);
			}
		}

		if (Expressions.hasFunctor(unionOrPartition, FunctorConstants.UNION)
				|| Expressions.hasFunctor(unionOrPartition, FunctorConstants.PARTITION)) {
			// Assume ok at this point but check that each
			// argument is a parfactor
			illegal = false;
			for (Expression parfactor : unionOrPartition.getArguments()) {
				if (!isParfactor(parfactor)) {
					illegal = true;
					notParfactors.add(parfactor);
					break;
				}
				else {
					collectedParfactors.add(parfactor);
				}
			}
		}

		if (illegal) {
			if (notParfactors.isEmpty()) {
				throw new IllegalArgumentException(
						"Not a parfactor set: " + expression);
			}
			else {
				throw new IllegalArgumentException(
						"Not parfactors: [" + Util.join(notParfactors) + "]");
			}
		}

		return collectedParfactors;
	}
}
