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

import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.ExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.exactbp.ExactBPExpressionBasedSolver;
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
public class HOGMMultiQueryProblemSolver {
	
	public static Class<? extends ExpressionBasedSolver> defaultSolverClass = ExactBPExpressionBasedSolver.class;
	
	private HOGModel hogmModel = null;
	private ExpressionBasedModel expressionBasedModel;
	private Class<? extends ExpressionBasedSolver> solverClass;
	private HOGMSingleQueryProblemSolver problemSolver;
	private List<HOGMProblemError> modelErrors = new ArrayList<>();
	private List<HOGMProblemResult> results = new ArrayList<>();
	private ProceduralAttachments proceduralAttachments = new DefaultProceduralAttachments();
	
	public HOGMMultiQueryProblemSolver(String model, String query) {
		this(model, list(query), defaultSolverClass);
	}
	
	public HOGMMultiQueryProblemSolver(String model, List<String> queries) {
		this(model, queries, defaultSolverClass);
	}

	public HOGMMultiQueryProblemSolver(String model, String query, Class<? extends ExpressionBasedSolver> solverClass) {
		this(model, list(query), solverClass);
	}
	
	public HOGMMultiQueryProblemSolver(String modelString, List<String> queries, Class<? extends ExpressionBasedSolver> solverClass) {
		this.solverClass = solverClass;
		initializeModel(modelString);
        processAllQueries(queries);
	}
	
	public void setProceduralAttachments(ProceduralAttachments proceduralAttachments) {
		this.proceduralAttachments = proceduralAttachments;
	}

	public Map<String, Procedure> getProceduralAttachments() {
		return this.proceduralAttachments;
	}

	private void initializeModel(String modelString) {
		HOGMModelParsing parsingWithErrorCollecting = new HOGMModelParsing(modelString, modelErrors);
		this.hogmModel = parsingWithErrorCollecting.getModel();
		this.expressionBasedModel = hogmModel == null? null : new HOGMExpressionBasedModel(hogmModel);
		this.expressionBasedModel.setProceduralAttachments(proceduralAttachments);
	}

	private void processAllQueries(List<String> queries) {
		for (String query : queries) {
        	solveProblemWithQuery(query);
        }
	}

	private void solveProblemWithQuery(String query) {
		HOGMSingleQueryProblemSolver problemSolver = new HOGMSingleQueryProblemSolver(query, solverClass, hogmModel, expressionBasedModel, modelErrors);
		results.addAll(problemSolver.getResults());
	}

	public void interrupt() {
		if (problemSolver != null) {
			problemSolver.interrupt();
		}
	}

	public List<HOGMProblemResult> getResults() {
        return results;
    }

	public Expression simplifyAnswer(Expression answer, Expression forQuery) {
		Expression result = HOGMAnswerSimplifier.simplifyAnswer(answer, forQuery, expressionBasedModel.getContext());
		return result;
	}
}
