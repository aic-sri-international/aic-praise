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
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.setDifference;

import java.util.Collection;
import java.util.Map;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.core.PrologConstantPredicate;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.plaindpll.api.SemiRingProblemType;
import com.sri.ai.grinder.plaindpll.api.Solver;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.grinder.plaindpll.core.SGDPLLT;
import com.sri.ai.grinder.plaindpll.core.SGVET;
import com.sri.ai.grinder.plaindpll.problemtype.SumProduct;
import com.sri.ai.grinder.plaindpll.theory.AtomsOnConstraintTheoryWithEquality;
import com.sri.ai.grinder.plaindpll.theory.EqualityConstraintTheory;
import com.sri.ai.grinder.plaindpll.theory.term.SymbolTermTheory;
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
	private Map<String, String> mapFromTypeNameToSizeString;
	private Map<String, String> mapFromRandomVariableNameToTypeName;
	private Expression queryVariable;
	private Collection<Expression> allVariables;
	private Predicate<Expression> isUniquelyNamedConstantPredicate;
	private ConstraintTheory theory;
	private SemiRingProblemType problemType;
	private Solver solver;

	public Expression getEvidenceProbability() {
		return evidenceProbability;
	}

	public Map<String, String> getMapFromTypeNameToSizeString() {
		return mapFromTypeNameToSizeString;
	}

	public Map<String, String> getMapFromRandomVariableNameToTypeName() {
		return mapFromRandomVariableNameToTypeName;
	}

	public void interrupt() {
		solver.interrupt();
	}
	
	/**
	 * Constructs a solver for a factor graph and an evidence expression.
	 * @param useFactorization TODO
	 * @param mapFromTypeNameToSizeStringParam a map from type name strings to their size strings
	 * @param mapFromRandomVariableNameToTypeNameParam a map from random variable name strings to their type name strings
	 * @param allTheSameButQuery the {@link Result} object from a previous query for the same model and evidence, if available, or null.
	 * @param factorGraph an Expression representing the product of potential functions
	 * @param isBayesianNetwork indicates the factor graph is a bayesian network (each potential function in normalized for one of its variables, forms a DAG).
	 * @param evidence an Expression representing the evidence
	 * @return
	 */
	public InferenceForFactorGraphAndEvidence(
			Expression factorGraphParam, boolean isBayesianNetworkParam,
			Expression evidenceParam, boolean useFactorization, Map<String, String> mapFromTypeNameToSizeStringParam, Map<String, String> mapFromRandomVariableNameToTypeNameParam) {

		factorGraph = factorGraphParam;
		isBayesianNetwork = isBayesianNetworkParam;
		evidence = evidenceParam;
		mapFromTypeNameToSizeString = mapFromTypeNameToSizeStringParam;
		mapFromRandomVariableNameToTypeName = mapFromRandomVariableNameToTypeNameParam;
		mapFromRandomVariableNameToTypeName.put("query", "Boolean");
		queryVariable = parse("query");

		allVariables = Util.mapIntoList(mapFromRandomVariableNameToTypeName.keySet(), Expressions::parse);

		// We use the Prolog convention of small-letter initials for constants, but we need an exception for the random variables.
		Predicate<Expression> isPrologConstant = new PrologConstantPredicate();
		isUniquelyNamedConstantPredicate = e -> isPrologConstant.apply(e) && ! allVariables.contains(e);

		// The constraintTheory of atoms plus equality on function (relational) terms.
		theory = new AtomsOnConstraintTheoryWithEquality(new EqualityConstraintTheory(new SymbolTermTheory()));
		problemType = new SumProduct(); // for marginalization
		// The solver for the parameters above.
		if (useFactorization) {
			solver = new SGVET(theory, problemType);
			//((SGVET) solver).basicOutput = true;
		}
		else {
			solver = new SGDPLLT(theory, problemType);
		}

		evidenceProbability = null;
	}
	
	public ConstraintTheory getTheory() {
		return theory;
	}

	public Expression solve(Expression queryExpression) {
		Expression factorGraphWithEvidence = factorGraph;
		if (queryVariable != queryExpression) {
			// Add a query variable equivalent to query expression; this introduces no cycles and the model remains a Bayesian network
			factorGraphWithEvidence = Times.make(list(factorGraph, parse("if query <=> " + queryExpression + " then 1 else 0")));
		}

		if (evidence != null) {
			// add evidence factor
			factorGraphWithEvidence = Times.make(list(factorGraphWithEvidence, IfThenElse.make(evidence, ONE, ZERO)));
		}

		// We sum out all variables but the query
		Collection<Expression> indices = setDifference(allVariables, list(queryVariable)); 

		// Solve the problem.
		Expression unnormalizedMarginal = solver.solve(factorGraphWithEvidence, indices, mapFromRandomVariableNameToTypeName, mapFromTypeNameToSizeString, isUniquelyNamedConstantPredicate);
		System.out.println("Unnormalized marginal: " + unnormalizedMarginal);

		Expression marginal;
		if (evidence == null && isBayesianNetwork) {
			marginal = unnormalizedMarginal; // model was a Bayesian network with no evidence, so marginal is equal to unnormalized marginal.
		}
		else {
			// We now marginalize on all variables. Since unnormalizedMarginal is the marginal on all variables but the query, we simply take that and marginalize on the query alone.
			if (evidenceProbability == null) {
				evidenceProbability = solver.solve(unnormalizedMarginal, list(queryVariable), mapFromRandomVariableNameToTypeName, mapFromTypeNameToSizeString, isUniquelyNamedConstantPredicate);
				System.out.print("Normalization constant ");
				if (evidence != null) {
					System.out.print("(same as evidence probability P(" + evidence + ") ) ");
				}
				System.out.print("is " + evidenceProbability + "\n");
			}

			marginal = Division.make(unnormalizedMarginal, evidenceProbability); // Bayes theorem: P(Q | E) = P(Q and E)/P(E)
			// now we use the algorithm again for simplifying the above division; this is a lazy way of doing this, as it performs search on the query variable again -- we could instead write an ad hoc function to divide all numerical constants by the normalization constant, but the code would be uglier and the gain very small, since this is a search on a single variable anyway.
			marginal = evaluate(marginal);
		}

		// replace the query variable with the query expression
		marginal = marginal.replaceAllOccurrences(queryVariable, queryExpression, new DefaultRewritingProcess(null));

		return marginal;
	}

	/**
	 * Symbolically evaluates an expression.
	 * @param expression
	 * @return
	 */
	public Expression evaluate(Expression expression) {
		return solver.solve(expression, list(), mapFromRandomVariableNameToTypeName, mapFromTypeNameToSizeString, isUniquelyNamedConstantPredicate);
	}
}
