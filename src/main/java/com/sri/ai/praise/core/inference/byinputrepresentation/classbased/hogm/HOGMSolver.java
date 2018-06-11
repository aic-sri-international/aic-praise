/*
 * Copyright (c) 2015, SRI International
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
package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.time;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.solver.IntegrationRecording;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.ExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.exactbp.ExactBPExpressionBasedSolver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.HOGModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMSortDeclaration;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;

@Beta
public class HOGMSolver {
	
	// TODO: This class currently does too many things:
	// it does both solving and lots of error manipulation, handles multiple queries, and deals with both HOGModels and expression-based models.
	// It should instead: run another class for error checking of both model and queries, and run another class for processing an error-free query.
	
//	public static Class<? extends ExpressionBasedSolver> defaultSolverClass = EvaluationExpressionBasedSolver.class;
	public static Class<? extends ExpressionBasedSolver> defaultSolverClass = ExactBPExpressionBasedSolver.class;
	
	private HOGModel hogmModel = null;
	private List<HOGMProblemResult> results = new ArrayList<>();
	private List<HOGMProblemError> modelErrors = new ArrayList<>();
	private boolean canceled = false;
	private ExpressionBasedModel expressionBasedModel;
	private Class<? extends ExpressionBasedSolver> solverClass;
	private ExpressionBasedSolver solver = null;
	
	public HOGMSolver(String model, String query) {
		this(model, list(query), defaultSolverClass);
	}
	
	public HOGMSolver(String model, List<String> queries) {
		this(model, queries, defaultSolverClass);
	}

	public HOGMSolver(String model, String query, Class<? extends ExpressionBasedSolver> solverClass) {
		this(model, list(query), solverClass);
	}
	
	public HOGMSolver(String modelString, List<String> queries, Class<? extends ExpressionBasedSolver> solverClass) {
		this.solverClass = solverClass;
		initializeModel(modelString);
        processAllQueries(queries);
	}

	private void initializeModel(String modelString) {
		HOGMModelParsingWithErrorCollecting parsingWithErrorCollecting = new HOGMModelParsingWithErrorCollecting(modelString, modelErrors);
		this.hogmModel = parsingWithErrorCollecting.getModel();
		this.expressionBasedModel = hogmModel == null? null : new HOGMExpressionBasedModel(hogmModel);
	}

	private ExpressionBasedSolver getSolver() {
		if (solver == null) {
			makeSolver(solverClass);
		}
		return solver;
	}

	private void makeSolver(Class<? extends ExpressionBasedSolver> solverClass) throws Error {
		try {
			this.solver = solverClass.newInstance();
		}
		catch (Throwable throwable) {
			throw new Error("Could not instantiate " + solverClass);
		}
	}

	private void processAllQueries(List<String> queries) {
		for (String query : queries) {
        	processQuery(query);
        }
	}

	public List<HOGMProblemResult> getResults() {
        return results;
    }

	private void processQuery(String query) {
		HOGMQueryParsing queryParsing = new HOGMQueryParsing(query, hogmModel, modelErrors);
		if (queryParsing.succeeded()) {
			collectInferenceResult(query, queryParsing);
		}
		else {
			collectParsingErrorResult(queryParsing);
		}
	}

	private void collectInferenceResult(String query, HOGMQueryParsing queryParsing) {
		if (!canceled) {
			HOGMProblemResult inferenceResult = runInference(query, queryParsing.getQueryExpression());
			results.add(inferenceResult);
		}
	}

	private HOGMProblemResult runInference(String query, Expression queryExpression) {
		IntegrationRecording.startRecordingIntegrationsOverGroups();
		Pair<Expression, Long> inferenceResultAndTime = time(inference(queryExpression)); 			
		HOGMProblemResult inferenceResult = new HOGMProblemResult(query, queryExpression, hogmModel, inferenceResultAndTime);
		inferenceResult.recordNumberOfSummations();
		return inferenceResult;
	}

	private NullaryFunction<Expression> inference(Expression queryExpression) {
		NullaryFunction<Expression> inference = () -> getSolver().solve(queryExpression, expressionBasedModel);
		return inference;
	}

	private void collectParsingErrorResult(HOGMQueryParsing queryParsing) {
		results.add(queryParsing.getParsingErrorProblemResult());
	}

	public void cancelQuery() {
		canceled = true;
		if (solver != null) {
			solver.interrupt();
		}
	}

	public Expression simplifyAnswer(Expression answer, Expression forQuery) {
		Expression result  = answer;
		Context    context = getContext();
		if (HOGMSortDeclaration.IN_BUILT_BOOLEAN.getName().equals(GrinderUtil.getTypeExpressionOfExpression(forQuery, context))) {
			result = result.replaceAllOccurrences(forQuery, Expressions.TRUE, context);
			result = simplify(result);
			answer = Expressions.parse(result.toString()); // This ensures numeric values have the correct precision
		}
		return result;
	}
	
	public Context getContext() {
		return expressionBasedModel.getContext();
	}
	
	public Expression simplify(Expression expression) {
		return getContext().evaluate(expression);
	} 
}
