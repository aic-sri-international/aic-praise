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
package com.sri.ai.praise.sgsolver.solver;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoSet;
import static com.sri.ai.util.Util.setDifference;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TypeContext;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.sgdpll.api.OldStyleQuantifierEliminator;
import com.sri.ai.grinder.sgdpll.api.SemiRingProblemType;
import com.sri.ai.grinder.sgdpll.api.Theory;
import com.sri.ai.grinder.sgdpll.core.solver.SGVET;
import com.sri.ai.grinder.sgdpll.interpreter.SGDPLLT;
import com.sri.ai.grinder.sgdpll.problemtype.SumProduct;
import com.sri.ai.grinder.sgdpll.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpll.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpll.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpll.theory.propositional.PropositionalTheory;
import com.sri.ai.util.Util;

/**
 * A solver for factor graphs using {@link SGVET}.
 * @author braz
 *
 */
public class InferenceForFactorGraphAndEvidence {

	private Expression factorGraph;
	private boolean isBayesianNetwork;
	private Expression evidence;
	private Expression evidenceProbability;
	private Map<String, String> mapFromRandomVariableNameToTypeName;
	private Map<String, String> mapFromSymbolNameToTypeName; // union of the two maps above
	private Map<String, String> mapFromCategoricalTypeNameToSizeString;
	private Collection<Type> additionalTypes;
	private Collection<Expression> allRandomVariables;
	private Predicate<Expression> isUniquelyNamedConstantPredicate;
	private Theory theory;
	private SemiRingProblemType problemType;
	private OldStyleQuantifierEliminator solver;

	public Expression getEvidenceProbability() {
		return evidenceProbability;
	}

	public Map<String, String> getMapFromRandomVariableNameToTypeName() {
		return mapFromRandomVariableNameToTypeName;
	}

	public void interrupt() {
		solver.interrupt();
	}
	
	/**
	 * Constructs a solver for a factor graph and an evidence expression.
	 * @param factorsAndTypes 
	 *        the factors and their type information over which inference is to be performed.
	 * @param isBayesianNetwork 
	 *        indicates if the factor graph is a bayesian network (each potential function in normalized for one of its variables, forms a DAG).
	 * @param evidence 
	 *        an Expression representing the evidence
	 * @param useFactorization indicates whether to use factorization (as in Variable Elimination)
	 * @param optionalTheory the theory to be used; if null, a default one is used (as of January 2016, a compound theory with propositional, equalities on categorical types and difference arithmetic).
	 */
	public InferenceForFactorGraphAndEvidence(
			FactorsAndTypes factorsAndTypes,
			boolean isBayesianNetwork,
			Expression evidence,
			boolean useFactorization,
			Theory optionalTheory) {

		this.factorGraph       = Times.make(factorsAndTypes.getFactors());
		this.isBayesianNetwork = isBayesianNetwork;
		this.evidence          = evidence;

		this.mapFromRandomVariableNameToTypeName = new LinkedHashMap<>(factorsAndTypes.getMapFromRandomVariableNameToTypeName());
		
		this.mapFromSymbolNameToTypeName = new LinkedHashMap<>(mapFromRandomVariableNameToTypeName);
		this.mapFromSymbolNameToTypeName.putAll(factorsAndTypes.getMapFromNonUniquelyNamedConstantNameToTypeName());
		this.mapFromSymbolNameToTypeName.putAll(factorsAndTypes.getMapFromUniquelyNamedConstantNameToTypeName());
		
		allRandomVariables = Util.mapIntoList(this.mapFromRandomVariableNameToTypeName.keySet(), Expressions::parse);
		                       
		this.mapFromCategoricalTypeNameToSizeString = new LinkedHashMap<>(factorsAndTypes.getMapFromCategoricalTypeNameToSizeString());

		Set<Expression> uniquelyNamedConstants = mapIntoSet(factorsAndTypes.getMapFromUniquelyNamedConstantNameToTypeName().keySet(), Expressions::parse);
		isUniquelyNamedConstantPredicate = e -> uniquelyNamedConstants.contains(e) || Expressions.isNumber(e) || Expressions.isBooleanSymbol(e);
		
		if (mapFromRandomVariableNameToTypeName.values().stream().anyMatch(type -> type.contains("->")) ||
			factorsAndTypes.getMapFromNonUniquelyNamedConstantNameToTypeName().values().stream().anyMatch(type -> type.contains("->"))) {
		}
		else {
		}

		problemType = new SumProduct(); // for marginalization

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
		
		this.additionalTypes = new LinkedList<Type>(theory.getNativeTypes()); // add needed types that may not be the type of any variable
		this.additionalTypes.addAll(factorsAndTypes.getAdditionalTypes());
		
		if (useFactorization) {
			solver = new SGVET(problemType, theory);
		}
		else {
			solver = new SGDPLLT(theory.getTopSimplifier(), problemType);
		}

		evidenceProbability = null;
	}
	
	public Theory getTheory() {
		return theory;
	}

	/**
	 * Returns the marginal/posterior for the query expression;
	 * if the query expression is not a random variable,
	 * the result is expressed in terms of a symbol 'query'.
	 */
	public Expression solve(Expression queryExpression) {
		
		Expression factorGraphWithEvidence = factorGraph;

		if (evidence != null) {
			// add evidence factor
			factorGraphWithEvidence = Times.make(list(factorGraphWithEvidence, IfThenElse.make(evidence, ONE, ZERO)));
		}

		boolean queryIsCompoundExpression;
		Expression queryVariable;
		Collection<Expression> queryVariables;
		Collection<Expression> indices; 
		if (allRandomVariables.contains(queryExpression)) {
			queryIsCompoundExpression = false;
			queryVariable = queryExpression;
			queryVariables = list(queryVariable);
			indices = setDifference(allRandomVariables, queryVariables);
		}
		else {
			queryIsCompoundExpression = true;
			queryVariable = makeSymbol("query");
			queryVariables = list(queryVariable);
			// Add a query variable equivalent to query expression; this introduces no cycles and the model remains a Bayesian network
			factorGraphWithEvidence = Times.make(list(factorGraphWithEvidence, parse("if query <=> " + queryExpression + " then 1 else 0")));
			indices = allRandomVariables; // 'query' is not in 'allRandomVariables' 
			mapFromSymbolNameToTypeName.put("query", "Boolean"); // in case it was not there before -- it is ok to leave it there for other queries
			mapFromCategoricalTypeNameToSizeString.put("Boolean", "2"); // in case it was not there before
		}
		
		// Solve the problem.
		Expression unnormalizedMarginal = sum(indices, factorGraphWithEvidence);
//		System.out.println("Unnormalized marginal: " + unnormalizedMarginal);

		Expression marginal;
		if (evidence == null && isBayesianNetwork) {
			marginal = unnormalizedMarginal; // model was a Bayesian network with no evidence, so marginal is equal to unnormalized marginal.
		}
		else {
			// We now marginalize on all variables. Since unnormalizedMarginal is the marginal on all variables but the query, we simply take that and marginalize on the query alone.
			if (evidenceProbability == null) {
				evidenceProbability = solver.solve(unnormalizedMarginal, queryVariables, mapFromSymbolNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes, isUniquelyNamedConstantPredicate, theory);
			}

			marginal = Division.make(unnormalizedMarginal, evidenceProbability); // Bayes theorem: P(Q | E) = P(Q and E)/P(E)
			// now we use the algorithm again for simplifying the above division; this is a lazy way of doing this, as it performs search on the query variable again -- we could instead write an ad hoc function to divide all numerical constants by the normalization constant, but the code would be uglier and the gain very small, since this is a search on a single variable anyway.
			marginal = evaluate(marginal);
		}

		if (queryIsCompoundExpression) {
			// replace the query variable with the query expression
			marginal = marginal.replaceAllOccurrences(queryVariable, queryExpression, new TypeContext());
		}

		return marginal;
	}

	/**
	 * @param indices
	 * @param expression
	 * @return
	 */
	public Expression sum(Collection<Expression> indices, Expression expression) {
		return solver.solve(expression, indices, mapFromSymbolNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes, isUniquelyNamedConstantPredicate, theory);
	}

	/**
	 * Symbolically evaluates an expression into a solution (possibly nested if then else expression).
	 * @param expression
	 * @return
	 */
	public Expression evaluate(Expression expression) {
		return solver.solve(expression, list(), mapFromSymbolNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes, isUniquelyNamedConstantPredicate, theory);
	}

	/**
	 * Simplifies an expression without requiring a context with all the type information (creating it from scratch);
	 * use {@link #simplify(Expression, Context)} instead for greater efficient if you already have such a context,
	 * or if you are invoking this method multiple times (you can make the context only once with {@link #makeContextWithTypeInformation()}.
	 * @param expression
	 * @return
	 */
	public Expression simplify(Expression expression) {
		Context context = makeContextWithTypeInformation();
		return simplify(expression, context);
	}

	/**
	 * Simplifies an expression given context with all the type information already built-in
	 * (one can be built with {@link #makeContextWithTypeInformation()}.
	 * @param expression
	 * @param context
	 * @return
	 */
	public Expression simplify(Expression expression, Context context) {
		Expression result = theory.simplify(expression, context);
		return result;
	}

	/**
	 * Makes context with all the type information on this inferencer.
	 * @return
	 */
	public Context makeContextWithTypeInformation() {
		return GrinderUtil.makeContext(mapFromSymbolNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes, isUniquelyNamedConstantPredicate, theory);
	}
}
