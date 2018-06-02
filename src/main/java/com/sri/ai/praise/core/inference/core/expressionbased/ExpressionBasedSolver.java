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
 * Neither the name of the aic-expresso nor the names of its
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
package com.sri.ai.praise.core.inference.core.expressionbased;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoSet;
import static com.sri.ai.util.Util.setDifference;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.MultiQuantifierEliminator;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.solver.DefaultMultiQuantifierEliminator;
import com.sri.ai.grinder.core.solver.SGVET;
import com.sri.ai.grinder.group.AssociativeCommutativeSemiRing;
import com.sri.ai.grinder.group.SumProduct;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.UniquelyNamedConstantIncludingBooleansAndNumbersPredicate;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.equality.EqualityTheory;
import com.sri.ai.grinder.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.theory.propositional.PropositionalTheory;
import com.sri.ai.praise.core.model.classbased.expressionbased.ExpressionBasedModel;

/**
 * A multiQuantifierEliminator for factor graphs using {@link SGVET}.
 * 
 * @author braz
 *
 */
public class ExpressionBasedSolver {

	private ExpressionBasedModel model;
	private Expression partitionFunction;
	private AssociativeCommutativeSemiRing semiRing;
	private MultiQuantifierEliminator multiQuantifierEliminator;
	private Theory theory;

	public Expression getPartitionFunction() {
		return partitionFunction;
	}

	public void interrupt() {
		multiQuantifierEliminator.interrupt();
	}
	
	/**
	 * Constructs a multiQuantifierEliminator for a factor graph .
	 * @param model 
	 *        the factors and their type information over which inference is to be performed.
	 * @param useFactorization indicates whether to use factorization (as in ExpressionVariable Elimination)
	 * @param optionalTheory the theory to be used; if null, a default one is used (as of May 2017, a compound theory with propositional, equalities on categorical types, difference arithmetic, and real linear arithmetic).
	 */
	public ExpressionBasedSolver(ExpressionBasedModel model, boolean useFactorization, Theory optionalTheory) {

		this.model = model;
		
		if (optionalTheory != null) {
			this.theory = optionalTheory;
		}
		else {
			this.theory =
					new CompoundTheory(
							new EqualityTheory(false, true),
							new DifferenceArithmeticTheory(false, true),
							new LinearRealArithmeticTheory(false, true),
							new PropositionalTheory());
		}
		
		if (useFactorization) {
			multiQuantifierEliminator = new SGVET();
		}
		else {
			multiQuantifierEliminator = new DefaultMultiQuantifierEliminator();
		}

		partitionFunction = null;
		semiRing = new SumProduct(); // for marginalization
	}

	private Context contextWithQuery = null;
	
	public Context getContextWithQuery() {
		if (contextWithQuery == null) {
			contextWithQuery = makeContext(model, theory, true);
		}
		return contextWithQuery;
	}

	private Context context = null;
	
	public Context getContext() {
		if (context == null) {
			context = makeContext(model, theory, false);
		}
		return context;
	}

	private static Context makeContext(ExpressionBasedModel model, Theory optionalTheory, boolean withQuery) {
		Map<String, String> mapFromSymbolNameToTypeName;
		Map<String, String> mapFromCategoricalTypeNameToSizeString;
		Collection<Type> additionalTypes;
		Predicate<Expression> isUniquelyNamedConstantPredicate;
		Theory theory;

		mapFromSymbolNameToTypeName = new LinkedHashMap<>(model.getMapFromRandomVariableNameToTypeName());
		mapFromSymbolNameToTypeName.putAll(model.getMapFromNonUniquelyNamedConstantNameToTypeName());
		mapFromSymbolNameToTypeName.putAll(model.getMapFromUniquelyNamedConstantNameToTypeName());

		mapFromCategoricalTypeNameToSizeString = new LinkedHashMap<>(model.getMapFromCategoricalTypeNameToSizeString());

		mapFromSymbolNameToTypeName.put("query", "Boolean"); // in case it was not there before -- it is ok to leave it there for other queries
		mapFromCategoricalTypeNameToSizeString.put("Boolean", "2"); // in case it was not there before

		Set<Expression> uniquelyNamedConstants = mapIntoSet(model.getMapFromUniquelyNamedConstantNameToTypeName().keySet(), Expressions::parse);
		isUniquelyNamedConstantPredicate = new UniquelyNamedConstantIncludingBooleansAndNumbersPredicate(uniquelyNamedConstants);

		if (optionalTheory != null) {
			theory = optionalTheory;
		}
		else {
			theory =
					new CompoundTheory(
							new EqualityTheory(false, true),
							new DifferenceArithmeticTheory(false, true),
							new LinearRealArithmeticTheory(false, true),
							new PropositionalTheory());
		}

		additionalTypes = new LinkedList<Type>(theory.getNativeTypes()); // add needed types that may not be the type of any variable
		additionalTypes.addAll(model.getAdditionalTypes());

		Context contextWithQuery = GrinderUtil.makeContext(mapFromSymbolNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes, isUniquelyNamedConstantPredicate, theory);
		
		return contextWithQuery;
	}
	
	/**
	 * Returns the marginal/posterior for the query expression;
	 * if the query expression is not a random variable,
	 * the result is expressed in terms of a symbol 'query'.
	 */
	public Expression solve(Expression queryExpression) {
		
		Expression factorGraphToUse;
		Expression queryVariable;
		List<Expression> queryVariables;
		List<Expression> indices; 
		boolean queryIsCompoundExpression;
		Context contextToBeUsed;
		if (model.getRandomVariables().contains(queryExpression)) {
			factorGraphToUse = Times.make(model.getFactors());
			queryIsCompoundExpression = false;
			queryVariable = queryExpression;
			queryVariables = list(queryVariable);
			indices = setDifference(model.getRandomVariables(), queryVariables);
			contextToBeUsed = getContext();
		}
		else {
			queryIsCompoundExpression = true;
			queryVariable = makeSymbol("query");
			queryVariables = list(queryVariable);
			// Add a query variable equivalent to query expression; this introduces no cycles and the model remains a Bayesian network
			factorGraphToUse = Times.make(list(Times.make(model.getFactors()), parse("if query <=> " + queryExpression + " then 1 else 0")));
			indices = model.getRandomVariables(); // 'query' is not in model's random variables 
			contextToBeUsed = getContextWithQuery();
		}

		// Solve the problem.
		Expression unnormalizedMarginal = multiQuantifierEliminator.extendContextAndSolve(semiRing, indices, factorGraphToUse, contextToBeUsed);

		Expression marginal;
		if (model.isKnownToBeBayesianNetwork()) {
			marginal = unnormalizedMarginal; // model was a Bayesian network, so marginal is equal to unnormalized marginal.
		}
		else {
			// We now marginalize on all variables. Since unnormalizedMarginal is the marginal on all variables but the query, we simply take that and marginalize on the query alone.
			if (partitionFunction == null) {
				partitionFunction = multiQuantifierEliminator.extendContextAndSolve(semiRing, queryVariables, unnormalizedMarginal, contextToBeUsed);
			}

     		// Bayes theorem: P(Q | E) = P(Q and E)/P(E)
			marginal = Division.make(unnormalizedMarginal, partitionFunction);
			// now we use the algorithm again for simplifying the above division; this is a lazy way of doing this, as it performs search on the query variable again -- we could instead write an ad hoc function to divide all numerical constants by the normalization constant, but the code would be uglier and the gain very small, since this is a search on a single variable anyway.
			marginal = contextToBeUsed.evaluate(marginal);
		}

		if (queryIsCompoundExpression) {
			// replace the query variable with the query expression
			marginal = marginal.replaceAllOccurrences(queryVariable, queryExpression, contextToBeUsed);
		}

		return marginal;
	}

	/**
	 * @param indices
	 * @param expression
	 * @return
	 */
	public Expression sum(List<Expression> indices, Expression expression) {
		Context context = getContext();
		Expression result = multiQuantifierEliminator.extendContextAndSolve(semiRing, indices, expression, context);
		return result;
	}

	/**
	 * Symbolically evaluates an expression into a solution (possibly nested if then else expression).
	 * @param expression
	 * @return
	 */
	public Expression evaluate(Expression expression) {
		Context context = getContext();
		Expression result = context.evaluate(expression);
		return result;
	}

	/**
	 * Simplifies an expression without requiring a context with all the type information (creating it from scratch);
	 * use {@link #simplify(Expression, Context)} instead for greater efficient if you already have such a context,
	 * or if you are invoking this method multiple times.
	 * @param expression
	 * @return
	 */
	public Expression simplify(Expression expression) {
		Context context = getContext();
		return simplify(expression, context);
	}

	public Expression simplify(Expression expression, Context context) {
		Expression result = getContext().getTheory().simplify(expression, context);
		return result;
	}
}
