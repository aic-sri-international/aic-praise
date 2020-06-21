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
package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.solver;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.RESULT;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;

import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.ExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.exactbp.ExactBPOnExpressionFactorsExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.answer.HOGMAnswerSimplifier;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.parsing.HOGMModelParsing;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.parsing.HOGMProblemError;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.HOGModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.other.integration.proceduralattachment.api.ProceduralAttachments;
import com.sri.ai.praise.other.integration.proceduralattachment.api.Procedure;
import com.sri.ai.praise.other.integration.proceduralattachment.core.DefaultProceduralAttachments;

@Beta
/**
 * Executes multiple queries for the same model, producing one {@link HOGMProblemResult} for each query.
 * The results may contain inference results, or collections of errors if there are problems with the model or query.
 * If there are errors in the model, they will be included in the {@link HOGMProblemResult} for each query.
 * <p>
 * This class uses a {@link ExpressionBasedSolver}, with the default being a {@link ExactBPOnExpressionFactorsExpressionBasedSolver}.
 * @author braz
 *
 */
public class HOGMMultiQueryProblemSolver {
	
	private String modelString;
	private List<String> queries;
	
	private HOGModel hogmModel = null;
	private ExpressionBasedModel expressionBasedModel;
	public ExpressionBasedSolver expressionBasedSolver;
	private HOGMSingleQueryProblemSolverThatUsesResultsOfPreviouslyDoneHOGModelParsing problemSolver;
	private List<HOGMProblemResult> results = null;
	private ProceduralAttachments proceduralAttachments = new DefaultProceduralAttachments();
	
	public HOGMMultiQueryProblemSolver(String model, String query) {
		this(model, list(query), null);
	}
	
	public HOGMMultiQueryProblemSolver(String model, List<String> queries) {
		this(model, queries, null);
	}

	public HOGMMultiQueryProblemSolver(String model, String query, ExpressionBasedSolver expressionBasedSolver) {
		this(model, list(query), expressionBasedSolver);
	}
	
	public HOGMMultiQueryProblemSolver(String model, List<String> queries, ExpressionBasedSolver expressionBasedSolver) {
		this.modelString = model;
		this.queries = queries;
		this.expressionBasedSolver = expressionBasedSolver == null? new ExactBPOnExpressionFactorsExpressionBasedSolver() : expressionBasedSolver;
	}
	
	public void setProceduralAttachments(ProceduralAttachments proceduralAttachments) {
		this.proceduralAttachments = proceduralAttachments;
	}

	public Map<String, Procedure> getProceduralAttachments() {
		return this.proceduralAttachments;
	}

	public List<? extends HOGMProblemResult> getResults() {
		if (results == null) {
			results = arrayList();
			List<? extends HOGMProblemError> modelErrors = initializeHOGMAndExpressionBasedModels(modelString);
	        processAllQueries(queries, modelErrors);
		}
        return results;
    }

	private List<? extends HOGMProblemError> initializeHOGMAndExpressionBasedModels(String modelString) {
		HOGMModelParsing parsingWithErrorCollecting = new HOGMModelParsing(modelString);
		if (parsingWithErrorCollecting.succeeded()) {
			hogmModel = parsingWithErrorCollecting.getModel();
			expressionBasedModel = new HOGMExpressionBasedModel(hogmModel);
			expressionBasedModel.setProceduralAttachments(proceduralAttachments);
		}
		else {
			hogmModel = null;
			expressionBasedModel = null;
		}
		return parsingWithErrorCollecting.getErrors();
	}

	/**
	 * Processes all queries, either solving them or producing error results in case of model or query errors.
	 * @param query
	 * @param modelErrors
	 */
	private void processAllQueries(List<String> queries, List<? extends HOGMProblemError> modelErrors) {
		for (String query : queries) {
        	processQuery(query, modelErrors);
        }
	}

	/**
	 * Processes query, either solving it or producing error results in case of model or query errors.
	 * @param query
	 * @param modelErrors
	 */
	private void processQuery(String query, List<? extends HOGMProblemError> modelErrors) {
		explanationBlock("Processing query ", query, code(() -> {
			HOGMSingleQueryProblemSolverThatUsesResultsOfPreviouslyDoneHOGModelParsing problemSolver = 
					new HOGMSingleQueryProblemSolverThatUsesResultsOfPreviouslyDoneHOGModelParsing(
							query, 
							expressionBasedSolver, 
							hogmModel, 
							expressionBasedModel, 
							modelErrors);
			List<HOGMProblemResult> queryResult = problemSolver.getResults();
			results.addAll(queryResult);
			return queryResult;
		}), "Query result is ", RESULT);
	}

	public void interrupt() {
		if (problemSolver != null) {
			problemSolver.interrupt();
		}
	}

	public Expression simplifyAnswer(Expression answer, Expression forQuery) {
		Expression result = HOGMAnswerSimplifier.simplifyAnswer(answer, forQuery, expressionBasedModel.getContext());
		return result;
	}
}
