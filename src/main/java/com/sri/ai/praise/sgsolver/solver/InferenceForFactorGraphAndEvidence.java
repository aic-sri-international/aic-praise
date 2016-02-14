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
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.api.OldStyleQuantifierEliminator;
import com.sri.ai.grinder.sgdpll.api.SemiRingProblemType;
import com.sri.ai.grinder.sgdpll.core.DPLLUtil;
import com.sri.ai.grinder.sgdpll.core.solver.SGVET;
import com.sri.ai.grinder.sgdpll.interpreter.SGDPLLT;
import com.sri.ai.grinder.sgdpll.interpreter.SymbolicCommonInterpreterWithLiteralConditioning;
import com.sri.ai.grinder.sgdpll.problemtype.SumProduct;
import com.sri.ai.grinder.sgdpll.theory.compound.CompoundConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.equality.EqualityConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.inequality.InequalityConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.propositional.PropositionalConstraintTheory;
import com.sri.ai.util.Util;

/**
 * An example on how to use SGVE(T) to convert table representations to decision tree representations.
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
	private ConstraintTheory constraintTheory;
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
	 * @param optionalConstraintTheory the constraint theory to be used; if null, a default one is used (as of January 2016, a compound constraint theory with propositional, equalities on categorical types and inequalities on bounded integers).
	 * @return the marginal.
	 */
	public InferenceForFactorGraphAndEvidence(
			FactorsAndTypes factorsAndTypes,
			boolean isBayesianNetwork,
			Expression evidence,
			boolean useFactorization,
			ConstraintTheory optionalConstraintTheory) {

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

		if (optionalConstraintTheory != null) {
			this.constraintTheory = optionalConstraintTheory;
		}
		else {
			this.constraintTheory =
					new CompoundConstraintTheory(
							new EqualityConstraintTheory(false, true),
							new InequalityConstraintTheory(false, true),
							new PropositionalConstraintTheory());
		}
		
		this.additionalTypes = new LinkedList<Type>(constraintTheory.getNativeTypes()); // add needed types that may not be the type of any variable
		this.additionalTypes.addAll(factorsAndTypes.getAdditionalTypes());
		
		SymbolicCommonInterpreterWithLiteralConditioning simplifier = new SymbolicCommonInterpreterWithLiteralConditioning(constraintTheory);
		// TODO: since we are using the top simplifier of the simplifier above,
		// the "simplify under constraint" setting is irrelevant.
		// The whole functionality should be eliminated if it is not being used elsewhere.
		if (useFactorization) {
			solver = new SGVET(simplifier.getTopSimplifier(), problemType, constraintTheory);
		}
		else {
			solver = new SGDPLLT(simplifier.getTopSimplifier(), problemType, constraintTheory);
		}

		evidenceProbability = null;
	}
	
	public ConstraintTheory getConstraintTheory() {
		return constraintTheory;
	}

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
				evidenceProbability = solver.solve(unnormalizedMarginal, queryVariables, mapFromSymbolNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes, isUniquelyNamedConstantPredicate);

//				System.out.print("Normalization constant ");
//				if (evidence != null) {
//					System.out.print("(same as evidence probability P(" + evidence + ") ) ");
//				}
//				System.out.print("is " + evidenceProbability + "\n");
			}

			marginal = Division.make(unnormalizedMarginal, evidenceProbability); // Bayes theorem: P(Q | E) = P(Q and E)/P(E)
			// now we use the algorithm again for simplifying the above division; this is a lazy way of doing this, as it performs search on the query variable again -- we could instead write an ad hoc function to divide all numerical constants by the normalization constant, but the code would be uglier and the gain very small, since this is a search on a single variable anyway.
			marginal = evaluate(marginal);
		}

		if (queryIsCompoundExpression) {
			// replace the query variable with the query expression
			marginal = marginal.replaceAllOccurrences(queryVariable, queryExpression, new DefaultRewritingProcess());
		}

		return marginal;
	}

	/**
	 * @param indices
	 * @param expression
	 * @return
	 */
	public Expression sum(Collection<Expression> indices, Expression expression) {
		return solver.solve(expression, indices, mapFromSymbolNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes, isUniquelyNamedConstantPredicate);
	}

	/**
	 * Symbolically evaluates an expression into a solution (possibly nested if then else expression).
	 * @param expression
	 * @return
	 */
	public Expression evaluate(Expression expression) {
		return solver.solve(expression, list(), mapFromSymbolNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes, isUniquelyNamedConstantPredicate);
	}

	/**
	 * Simplifies an expression without requiring a process with all the type information (creating it from scratch);
	 * use {@link #simplify(Expression, RewritingProcess)} instead for greater efficient if you already have such a process,
	 * or if you are invoking this method multiple times (you can make the process only once with {@link #makeProcessWithTypeInformation()}.
	 * @param expression
	 * @return
	 */
	public Expression simplify(Expression expression) {
		RewritingProcess process = makeProcessWithTypeInformation();
		return simplify(expression, process);
	}

	/**
	 * Simplifies an expression given process with all the type information already built-in
	 * (one can be built with {@link #makeProcessWithTypeInformation()}.
	 * @param expression
	 * @param process
	 * @return
	 */
	public Expression simplify(Expression expression, RewritingProcess process) {
		Expression result = constraintTheory.simplify(expression, process);
		return result;
	}

	/**
	 * Makes rewriting process with all the type information on this inferencer.
	 * @return
	 */
	public RewritingProcess makeProcessWithTypeInformation() {
		return DPLLUtil.makeProcess(mapFromSymbolNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes, isUniquelyNamedConstantPredicate);
	}
}
