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
package com.sri.ai.praise.core.representation.classbased.hogm.components;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;

/**
 * A parametric random variable declaration following {@link AbstractHOGMVariableDeclaration}.
 *
 * @author oreilly
 * 
 */
@Beta
public class HOGMRandomVariableDeclaration extends AbstractHOGMVariableDeclaration {

	//
	public static final String FUNCTOR_RANDOM_VARIABLE_DECLARATION = "randomVariable";

	/**
	 * Default constructor. Will default the arity of the random variable
	 * declaration to 0 and range to be of sort 'Boolean'.
	 * 
	 * @param name
	 *            a unique string valued symbol for the random variable declaration.
	 */
	public HOGMRandomVariableDeclaration(Expression name) {
		this(name, Expressions.ZERO, HOGMSortDeclaration.IN_BUILT_BOOLEAN.getName());
	}

	/**
	 * Constructor. Will default range to be of sort 'Boolean' if not explicitly
	 * added to end of list of parameters.
	 * 
	 * @param name
	 *            a unique string valued symbol for the random variable declaration.
	 * @param arity
	 *            the number of parameters that the random variable takes
	 * @param parametersAndRange
	 *            is arity > 0 must specify the sort for each parameter.
	 *            Optional append the sort for the range of the random variable
	 *            to the end of this list (will default to 'Boolean' if not
	 *            specified).
	 */
	public HOGMRandomVariableDeclaration(Expression name, Expression arity, Expression... parametersAndRange) {
		super(name, arity, parametersAndRange);
	}

	private static HOGMRandomVariableDeclaration make(Expression name, Expression arity, Expression[] parametersAndRange) {
		return new HOGMRandomVariableDeclaration(name, arity, parametersAndRange);
	}

	@Override
	protected String getFunctor() {
		return FUNCTOR_RANDOM_VARIABLE_DECLARATION;
	}
	
	//
	// STATIC UTILITY ROUTINES
	//

	/**
	 * Determine if an expression is a legal random variable declaration.
	 * 
	 * @param expression
	 *            an expression to be checked for whether or not it is a legal
	 *            variable declaration.
	 * @return true if a legal variable declaration, false otherwise.
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
	 * variable declaration to 0 and range to be of sort 'Boolean' if not
	 * specified in the expression passed in.
	 * 
	 * @param expression
	 *            an expression in the form of a variable declaration.
	 * @return a RandomVariableDeclaration object corresponding to the expression
	 *         passed in.
	 */
	public static HOGMRandomVariableDeclaration makeRandomVariableDeclaration(Expression expression) {
		HOGMRandomVariableDeclaration result = 
				(HOGMRandomVariableDeclaration) 
				AbstractHOGMVariableDeclaration.makeDeclaration(
						HOGMRandomVariableDeclaration::make,
						FUNCTOR_RANDOM_VARIABLE_DECLARATION, 
						expression);
		return result;
	}
}