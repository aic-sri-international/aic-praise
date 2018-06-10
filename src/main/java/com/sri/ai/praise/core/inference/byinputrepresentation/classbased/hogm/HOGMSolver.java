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
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.solver.IntegrationRecording;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.ExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.exactbp.ExactBPExpressionBasedSolver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.HOGModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMSortDeclaration;
import com.sri.ai.praise.core.representation.classbased.hogm.parsing.HOGMParserWrapper;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;

@Beta
public class HOGMSolver {
	
	// TODO: This class currently does too many things:
	// it does both solving and lots of error manipulation, handles multiple queries, and deals with both HOGModels and expression-based models.
	// It should instead: run another class for error checking of both model and queries, and run another class for processing an error-free query.
	
//	public static Class<? extends ExpressionBasedSolver> defaultSolverClass = EvaluationExpressionBasedSolver.class;
	public static Class<? extends ExpressionBasedSolver> defaultSolverClass = ExactBPExpressionBasedSolver.class;
	
	private HOGMParserWrapper parser = new HOGMParserWrapper();
	private HOGModel hogmModel = null;
	private List<HOGMQueryResult> results = new ArrayList<>();
	private List<HOGMQueryError> errors = new ArrayList<>();
	private boolean canceled = false;
	private Theory optionalTheory = null;
	private ExpressionBasedModel expressionBasedModel;
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
		initializeModel(modelString);
		initializeSolver(solverClass);
        processAllQueries(queries);
	}

	private void initializeModel(String modelString) {
		HOGMModelParsingWithErrorCollecting parsingWithErrorCollecting = new HOGMModelParsingWithErrorCollecting(modelString, errors);
		this.hogmModel = parsingWithErrorCollecting.getModel();
		this.expressionBasedModel = hogmModel == null? null : new HOGMExpressionBasedModel(hogmModel);
	}

	private void initializeSolver(Class<? extends ExpressionBasedSolver> solverClass) {
		try {
			this.solver = solverClass.newInstance();
		}
		catch (Throwable throwable) {
			collectQueryError(throwable);
		}
	}

	private void processAllQueries(List<String> queries) {
		for (String query : queries) {
        	processQuery(query);
        }
	}

	public List<HOGMQueryResult> getResults() {
        return results;
    }

	public Theory getOptionalTheory() {
		return optionalTheory;
	}
	
	public void setOptionalTheory(Theory theory) {
		this.optionalTheory = theory;
	}
	
	private void processQuery(String query) {
		long queryProcessingStartingTime = System.currentTimeMillis();
		collectQueryResults(query, hogmModel);
		collectQueryResultBasedInErrors(query, hogmModel, queryProcessingStartingTime);
	}

	private void collectQueryResults(String query, HOGModel parsedModel) {
		
		collectErrorIfQueryIsEmpty(query);
		   		
		if (errors.size() == 0) {
			Expression queryExpression = parser.parseTerm(query, new HOGMParserErrorListener(HOGMQueryError.Scope.QUERY, errors));
			if (errors.size() == 0) {
				runInference(query, queryExpression, parsedModel);
			}
		}
	}

	private void collectErrorIfQueryIsEmpty(String query) {
		if (Util.isNullOrEmptyString(query)) {
			HOGMQueryError error = new HOGMQueryError(HOGMQueryError.Scope.QUERY, "Query not specified");
			errors.add(error);
		}
	}

	private void runInference(String query, Expression queryExpression, HOGModel parsedModel) {
		if (!canceled) {
			IntegrationRecording.startRecordingIntegrationsOverGroups();
			Pair<Expression, Long> inferenceResultAndTime = time(inference(queryExpression)); 			
			HOGMQueryResult queryResult = new HOGMQueryResult(query, queryExpression, parsedModel, inferenceResultAndTime);
			queryResult.recordNumberOfSummations();
			results.add(queryResult);
		}
	}

	private NullaryFunction<Expression> inference(Expression queryExpression) {
		NullaryFunction<Expression> inference = () -> solver.solve(queryExpression, expressionBasedModel);
		return inference;
	}

	private void collectQueryError(Throwable throwable) {
		HOGMQueryError error = new HOGMQueryError(throwable);
		errors.add(error);
	}

	private void collectQueryResultBasedInErrors(String query, HOGModel parsedModel, long queryProcessingStartingTime) {
		if (errors.size() > 0) {
			long queryProcessingEndingTime = System.currentTimeMillis();
			long time = queryProcessingEndingTime - queryProcessingStartingTime;
			HOGMQueryResult queryResult = new HOGMQueryResult(query, parsedModel, errors, time);
			results.add(queryResult);
			errors.clear();
		}
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
