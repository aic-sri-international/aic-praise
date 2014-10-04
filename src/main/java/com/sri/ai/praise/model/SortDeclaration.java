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

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;

/**
 * Sorts (from <a
 * href="http://en.wikipedia.org/wiki/Many-sorted_logic">many-sorted logic</a> )
 * allows us to to separate the objects in the universe of discourse into
 * separate partitions. The basic structure of a sort declaration is as follows:<br>
 * 
 * <pre>
 * // A sort declaration:
 * sort(name, size, {constant,...})
 * 
 * name: 
 * . mandatory: must be a unique string valued symbol within the model
 *   (e.g. cannot overlap with an individual constant in the type of
 *    a random variable).
 * 
 * size:
 * . optional: defaults to the string valued symbol "Unknown" to indicate unknown.
 *             if set, it must be an Integer symbol and its value must
 *             be >= the number of constants defined in the model
 *             as belonging to the sort.
 *  
 * {constant,...}:
 * . optional: defaults to an empty extensional uni-set.
 *             defines what constants used in the model declaration
 *             belong to this sort. the constants must be unique symbols
 *             and belonging to only a single sort within the context of
 *             a model.
 * 
 * </pre>
 * 
 * Note: There is one predefined in-built sort declaration: <br>
 * 
 * <pre>
 * 
 * // For boolean valued types (by default the range of random variables).
 * sort(Boolean, 2, {false, true})
 * </pre>
 * 
 * that does not need to be declared explicitly in a model declaration.
 * 
 * @author oreilly
 * 
 */
@Beta
public class SortDeclaration {
	/**
	 * Symbol used to indicate that the size of a sort is Unknown.
	 */
	public static final Expression UNKNOWN_SIZE = Expressions.makeSymbol("Unknown");

	/**
	 * Constant that can be used to name all sorts in a model if partitioning of
	 * the model is not desired.
	 */
	public static final Expression UNIVERSE_OF_DISCOURSE = Expressions.makeSymbol("Universe");

	/**
	 * An in-built sort representing Booleans {false, true}.
	 */
	public static final SortDeclaration IN_BUILT_BOOLEAN = new SortDeclaration(
			false, Expressions.makeSymbol("Boolean"), Expressions.makeSymbol(2),
			ExtensionalSet.makeUniSetExpression(Arrays.asList(new Expression[] {
					Expressions.FALSE, Expressions.TRUE })));
	public static final SortDeclaration[] IN_BUILT_SORTS = new SortDeclaration[] { IN_BUILT_BOOLEAN, };
	//
	public static final String FUNCTOR_SORT_DECLARATION = "sort";

	//
	private Expression name = null;
	private Expression size = null;
	private Expression constants = null;
	private Expression sortDeclaration = null;

	/**
	 * Default constructor. Will set the size of the sort to the string valued
	 * symbol "Unknown" and the known constants to the empty set {}.
	 * 
	 * @param name
	 *            the unique symbolic name for the sort.
	 */
	public SortDeclaration(Expression name) {
		this(name, UNKNOWN_SIZE);
	}

	/**
	 * Constructor. Will set the known constants to the empty set {}.
	 * 
	 * @param name
	 *            the unique symbolic name for the sort.
	 * @param size
	 *            an integer valued symbol >= # of known constants in the sort
	 *            if known. Otherwise a string valued symbol = "Unknown" if the
	 *            size of the sort is not known.
	 * 
	 */
	public SortDeclaration(Expression name, Expression size) {
		this(name, size, ExtensionalSet.makeEmptySet());
	}

	/**
	 * Constructor.
	 * 
	 * @param name
	 *            the unique symbolic name for the sort.
	 * @param size
	 *            an integer valued symbol >= # of known constants in the sort
	 *            if known. Otherwise a string valued symbol = "Unknown" if the
	 *            size of the sort is not known.
	 * @param constants
	 *            an extensional uni-set of symbols representing unique
	 *            constants in the model that belong to this sort.
	 * 
	 */
	public SortDeclaration(Expression name, Expression size,
			Expression constants) {
		this(true, name, size, constants);
	}
	
	/**
	 * Copy constructor, so that size information can be overridden.
	 * 
	 * @param toCopy
	 * @param knownTypeSize
	 * @param size
	 */
	public SortDeclaration(SortDeclaration toCopy, boolean knownTypeSize, Integer size) {
		this(false, toCopy.name, knownTypeSize ? Expressions.makeSymbol(size) : UNKNOWN_SIZE, toCopy.constants);		
	}

	/**
	 * 
	 * @return the unique identifying name for the sort.
	 */
	public Expression getName() {
		return name;
	}

	/**
	 * 
	 * @return the size of the sort. is an integer valued symbol or a string
	 *         valued symbol = "Unknown" to indicate unknown.
	 */
	public Expression getSize() {
		return size;
	}

	/**
	 * 
	 * @return an extensional uni-set of symbols representing unique constants
	 *         in the model.
	 */
	public Expression getConstants() {
		return constants;
	}

	/**
	 * Convenience routine to get the constants assigned to this sort from its
	 * constants expression.
	 * 
	 * @return the assigned constants to this sort.
	 */
	public List<Expression> getAssignedConstants() {
		List<Expression> result = ExtensionalSet.getElements(constants);

		return result;
	}

	/**
	 * 
	 * @return an expression representing the full sort declaration.
	 */
	public Expression getSortDeclaration() {
		// Lazy initialize this attribute
		if (sortDeclaration == null) {
			sortDeclaration = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_SORT_DECLARATION, name,
					size, constants);
		}

		return sortDeclaration;
	}

	//
	// STATIC UTILITY ROUTINES
	//

	/**
	 * Determine if an expression is a legal sort declaration or not.
	 * 
	 * @param expression
	 *            an expression to be checked for whether or not it is a legal
	 *            sort declaration expression.
	 * @return true if a legal sort declaration, false otherwise.
	 */
	public static boolean isSortDeclaration(Expression expression) {
		boolean isSortDeclaration = false;

		try {
			// Attempt to construct a SortDeclaration instance from the
			// expression
			makeSortDeclaration(expression);
			isSortDeclaration = true;
		} catch (IllegalArgumentException iae) {
			isSortDeclaration = false;
		}

		return isSortDeclaration;
	}

	/**
	 * Determine if an expression corresponds to an in-built sort declaration.
	 * 
	 * @param expression
	 *            an expression to be checked if it is an in-built sort
	 *            declaration.
	 * @return true if the expression corresponds to an in-built sort
	 *         declaration, false otherwise.
	 */
	public static boolean isInBuilt(Expression expression) {
		boolean isInBuilt = false;

		for (int i = 0; i < IN_BUILT_SORTS.length; i++) {
			if (IN_BUILT_SORTS[i].getSortDeclaration().equals(expression)) {
				isInBuilt = true;
				break;
			}
		}

		return isInBuilt;
	}

	/**
	 * Determine if is the name of an in-built sort.
	 * 
	 * @param name
	 *            the name to be checked for being an in-built or not.
	 * @return if the name corresponds to an in-built sort, false otherwise.
	 */
	public static boolean isNameOfInBuilt(Expression name) {
		boolean isInBuilt = false;

		for (int i = 0; i < IN_BUILT_SORTS.length; i++) {
			if (name.equals(IN_BUILT_SORTS[i].getName())) {
				isInBuilt = true;
				break;
			}
		}

		return isInBuilt;
	}

	/**
	 * Make a sort declaration object. Will set the size of the sort to the
	 * string valued symbol "Unknown" and the known constants to the empty set
	 * {} if not specified in the expression passed in.
	 * 
	 * @param expression
	 *            an expression in the form of a sort declaration definition.
	 * @return a SortDeclaration object corresponding to the expression passed
	 *         in.
	 * @throws IllegalArgumentException
	 *             if the expression is not a legal sort declaration.
	 */
	public static SortDeclaration makeSortDeclaration(Expression expression) {
		SortDeclaration declaration = null;

		// Check if an in-built first.
		for (int i = 0; i < IN_BUILT_SORTS.length; i++) {
			if (IN_BUILT_SORTS[i].getSortDeclaration().equals(expression)) {
				declaration = IN_BUILT_SORTS[i];
				break;
			}
		}

		// If not an in-built
		if (declaration == null
			&& Expressions.hasFunctor(expression, FUNCTOR_SORT_DECLARATION)) {
			
			int numArgs = expression.numberOfArguments();
			if (numArgs >= 1 && numArgs <= 3) {
				// Extract arguments
				Expression name = expression.get(0);
				Expression size = null;
				if (numArgs >= 2) {
					size = expression.get(1);
				} 
				else {
					size = UNKNOWN_SIZE;
				}
				Expression constants = null;
				if (numArgs >= 3) {
					constants = expression.get(2);
				} 
				else {
					constants = ExtensionalSet.makeEmptySetExpression();
				}

				declaration = new SortDeclaration(name, size, constants);
			}
		}

		if (declaration == null) {
			throw new IllegalArgumentException(
					"Not a legal definition of a sort declartion:" + expression);
		}

		return declaration;
	}

	//
	// PRIVATE METHODS
	//
	private SortDeclaration(boolean doAssertionChecks, Expression name,
			Expression size, Expression constants) {
		if (doAssertionChecks) {
			assertNameOk(name);
			assertSizeOk(size);
			assertConstantsOk(name, size, constants);
		}

		this.name = name;
		this.size = size;
		this.constants = constants;
	}

	private static void assertNameOk(Expression name) {
		boolean illegal = true;
		if (name.getSyntacticFormType().equals("Symbol")
			&& name.getValue() instanceof String
			// Ensure is not an in-built sort
			&& !isNameOfInBuilt(name)
			) {
			
			illegal = false;
		}
		if (illegal) {
			throw new IllegalArgumentException(
					"Model sort name '"
							+ name
							+ "' is not of the correct type. It must be a string-valued symbol.");
		}
	}

	private static void assertSizeOk(Expression size) {
		boolean illegal = true;
		if (size.equals(UNKNOWN_SIZE)) {
			illegal = false;
		} 
		else if (size.getSyntacticFormType().equals("Symbol")) {
			int ivalue = 0;
			try {
				ivalue = size.intValueExact();
				if (ivalue >= 0) {
					illegal = false;
				}
			}
			catch (ArithmeticException e) {
				// illegal remains 'true'
				illegal = true;
			}
			catch (Error e) { // intValueExact throws an Error if symbol is not a number
				// illegal remains 'true'
				illegal = true;
			}
		}
		if (illegal) {
			throw new IllegalArgumentException(
					"size ["
							+ size
							+ "] is not of the correct type. must be an integer valued symbol or a string valued symbol = 'Unknown'");
		}
	}

	private static void assertConstantsOk(Expression name, Expression size,
			Expression constants) {
		boolean illegal = true;

		if (Sets.isExtensionalUniSet(constants)) {
			boolean argsOk = true;
			Set<Expression> seen = new LinkedHashSet<Expression>();
			for (Expression arg : ExtensionalSet.getElements(constants)) {
				// Each constant must be a symbol.
				if (!(arg.getSyntacticFormType().equals("Symbol"))) {
					argsOk = false;
				}
				// Constants should be declared unique within the set expression
				if (seen.contains(arg)) {
					argsOk = false;
				}
				// Constant can't have same name as the sort its contained in
				if (name.equals(arg)) {
					argsOk = false;
				}

				if (!argsOk) {
					break;
				}

				seen.add(arg);
			}

			if (argsOk) {
				if (!UNKNOWN_SIZE.equals(size)) {
					if (seen.size() <= size.intValue()) {
						illegal = false;
					}
				} 
				else {
					illegal = false;
				}
			}
		}

		if (illegal) {
			throw new IllegalArgumentException(
					"constants ["
							+ constants
							+ "] is not of the correct type. must be an extensional uni-set of symbols representing unique constants in the model.");
		}
	}
}
