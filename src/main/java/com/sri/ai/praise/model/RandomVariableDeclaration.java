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
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;

/**
 * A parametric random variable declaration. The basic structure of a random
 * variable declaration is as follows:<br>
 * 
 * <pre>
 * // A random variable declaration
 * randomVariable(name, arity, parameterSortName1,...,parameterSortNameN, rangeSortName),
 * 
 * name:
 * . mandatory: must be a unique string valued symbol within the model for identifying the random variable.
 * 
 * arity:
 * . optional: defaults to an integer valued symbol 0.
 *   specifies the number of parameters that the
 *   random variable takes.
 *   
 * parameterSortName<n>:
 * . optional: only if arity is = 0. Otherwise a sort name
 *   must be specified for each parameter that the 
 *   random variable takes (i.e. number specified must match
 *   the declared arity).
 * 
 * rangeSortName:
 * . optional: defaults to the in-built sort 'Boolean'. If specified
 *   must follow the parameterSortNames (i.e. there would be arity+1 sort
 *   names in the declaration).
 * 
 * </pre>
 * 
 * @author oreilly
 * 
 */
@Beta
public class RandomVariableDeclaration {

	//
	public static final String FUNCTOR_RANDOM_VARIABLE_DECLARATION = "randomVariable";
	//
	private Expression name = null;
	private Expression arity = null;
	private int intArity = 0;
	private List<Expression> parameters = new ArrayList<Expression>();
	private Expression range = null;
	private Expression randomVariableDeclaration = null;

	/**
	 * Default constructor. Will default the arity of the random variable
	 * declaration to 0 and range to be of sort 'Boolean'.
	 * 
	 * @param name
	 *            a unique string valued symbol for the random variable
	 *            declaration.
	 */
	public RandomVariableDeclaration(Expression name) {
		this(name, Expressions.ZERO, SortDeclaration.IN_BUILT_BOOLEAN.getName());
	}

	/**
	 * Constructor. Will default range to be of sort 'Boolean' if not explicitly
	 * added to end of list of parameters.
	 * 
	 * @param name
	 *            a unique string valued symbol for the random variable
	 *            declaration.
	 * @param arity
	 *            the number of parameters that the random variable takes
	 * @param parametersAndRange
	 *            is arity > 0 must specify the sort for each parameter.
	 *            Optional append the sort for the range of the random variable
	 *            to the end of this list (will default to 'Boolean' if not
	 *            specified).
	 */
	public RandomVariableDeclaration(Expression name, Expression arity,
			Expression... parametersAndRange) {
		assertNameOk(name);
		assertArityOk(arity);
		assertParametersAndRangeOk(name, arity, parametersAndRange);

		this.name = name;
		this.arity = arity;
		this.intArity = arity.intValue();
		if (intArity == parametersAndRange.length) {
			for (int i = 0; i < parametersAndRange.length; i++) {
				parameters.add(parametersAndRange[i]);
			}
			// default range to boolean
			range = SortDeclaration.IN_BUILT_BOOLEAN.getName();
		} 
		else {
			for (int i = 0; i < parametersAndRange.length - 1; i++) {
				parameters.add(parametersAndRange[i]);
			}
			range = parametersAndRange[parametersAndRange.length - 1];
		}
	}

	/**
	 * 
	 * @return the unique identifying name for the random variable.
	 */
	public Expression getName() {
		return name;
	}

	/**
	 * 
	 * @return the arity of the number of parameters that the parametric random
	 *         variable declaration takes.
	 */
	public Expression getArity() {
		return arity;
	}

	/**
	 * 
	 * @return the actual value of the number of parameters that the parametric
	 *         random variable declaration takes.
	 */
	public int getArityValue() {
		return intArity;
	}

	/**
	 * 
	 * @return the sorts for the parameters of the random variable declaration.
	 */
	public List<Expression> getParameterSorts() {
		return Collections.unmodifiableList(parameters);
	}

	/**
	 * 
	 * @return the sort for the range that the random variable can take.
	 */
	public Expression getRangeSort() {
		return range;
	}

	/**
	 * 
	 * @return an expression representing the full random variable declaration.
	 */
	public Expression getRandomVariableDeclaration() {
		// Lazy initialize this attribute
		if (randomVariableDeclaration == null) {
			List<Expression> declarationArgs = new ArrayList<Expression>();
			declarationArgs.add(name);
			declarationArgs.add(arity);
			declarationArgs.addAll(parameters);
			declarationArgs.add(range);

			randomVariableDeclaration = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
					FUNCTOR_RANDOM_VARIABLE_DECLARATION,
					declarationArgs.toArray());
		}

		return randomVariableDeclaration;
	}

	//
	// STATIC UTILITY ROUTINES
	//

	/**
	 * Determine if an expressions is a legal random variable declaration.
	 * 
	 * @param expression
	 *            an expression to be checked for whether or not it is a legal
	 *            random variable declaration.
	 * @return true if a legal random variable declaration, false otherwise.
	 */
	public static boolean isRandomVariableDeclaration(Expression expression) {
		boolean isRandomVariableDeclaration = false;
		
		try {
			// Attempt to construct a RandomVariableDeclaration instance from the expression
			makeRandomVariableDeclaration(expression);
			isRandomVariableDeclaration = true;
		} catch (IllegalArgumentException iae) {
			isRandomVariableDeclaration = false;
		}
		
		return isRandomVariableDeclaration;
	}

	/**
	 * Make a random variable declaration object. Will default the arity of the
	 * random variable declaration to 0 and range to be of sort 'Boolean' if not
	 * specified in the expression passed in.
	 * 
	 * @param expression
	 *            an expression in the form of a random variable declaration.
	 * @return a RandomVariableDeclaration object corresonding to the expression
	 *         passed in.
	 */
	public static RandomVariableDeclaration makeRandomVariableDeclaration(
			Expression expression) {
		RandomVariableDeclaration declaration = null;

		if (Expressions.hasFunctor(expression, FUNCTOR_RANDOM_VARIABLE_DECLARATION)) {
			int numArgs = expression.numberOfArguments();
			if (numArgs > 0) {

				// Extract arguments
				Expression name = expression.get(0);
				Expression arity = Expressions.ZERO;
				if (numArgs >= 2) {
					arity = expression.get(1);
					
				}
				Expression[] parametersAndRange = new Expression[0];
				if (numArgs > 2) {
					parametersAndRange = new Expression[numArgs - 2];
					for (int i = 2; i < numArgs; i++) {
						parametersAndRange[i - 2] = expression.get(i);
					}
				} 
				else {
					parametersAndRange = new Expression[0];
				}
				
				declaration = new RandomVariableDeclaration(name, arity, parametersAndRange);
			}
		}
		
		if (declaration == null) {
			throw new IllegalArgumentException(
					"Not a legal definition of a random variable declartion:" + expression);
		}

		return declaration;
	}

	//
	// PRIVATE METHODS
	//
	private static void assertNameOk(Expression name) {
		boolean illegal = true;
		if (name.getSyntacticFormType().equals("Symbol")
			&& name.getValue() instanceof String) {
			illegal = false;
		}
		if (illegal) {
			throw new IllegalArgumentException(
					"name ["
							+ name
							+ "] is not of the correc type. must be a string valued symbol.");
		}
	}

	private static void assertArityOk(Expression arity) {
		boolean illegal = true;

		if (arity.getSyntacticFormType().equals("Symbol")) {
			Object value = arity.getValue();
			if (value instanceof Number) {
				int ivalue = ((Number) value).intValue();
				if (ivalue >= 0) {
					illegal = false;
				}
			}
		}

		if (illegal) {
			throw new IllegalArgumentException(
					"arity ["
							+ arity
							+ "] is not of the correct type. must be an integer valued symbol.");
		}
	}

	private static void assertParametersAndRangeOk(Expression name,
			Expression arity, Expression... parametersAndRange) {
		boolean illegal = true;

		int intArity = arity.intValue();
		if (intArity == parametersAndRange.length
				|| intArity == parametersAndRange.length - 1) {
			// Ensure are legal names for sorts and do not conflict with name of
			// the random variable declaration
			boolean sortNamesOk = true;
			for (int i = 0; i < parametersAndRange.length; i++) {
				assertNameOk(parametersAndRange[i]);
				if (name.equals(parametersAndRange[i])) {
					sortNamesOk = false;
					break;
				}
			}
			if (sortNamesOk) {
				illegal = false;
			}
		}

		if (illegal) {
			throw new IllegalArgumentException("Parameters and Range ["
					+ parametersAndRange.length
					+ "] do not match up with arity [" + arity + "]");
		}
	}
}
