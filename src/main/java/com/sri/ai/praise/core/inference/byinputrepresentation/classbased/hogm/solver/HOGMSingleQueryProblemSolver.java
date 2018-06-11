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

import static com.sri.ai.util.Util.time;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.core.solver.IntegrationRecording;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.ExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.parsing.HOGMProblemError;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.parsing.HOGMQueryParsing;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.HOGModel;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;

@Beta
public class HOGMSingleQueryProblemSolver {
	
	private HOGModel hogmModel = null;
	private List<HOGMProblemResult> results = new ArrayList<>();
	private boolean canceled = false;
	private ExpressionBasedModel expressionBasedModel;
	private Class<? extends ExpressionBasedSolver> solverClass;

	public HOGMSingleQueryProblemSolver(String query, Class<? extends ExpressionBasedSolver> solverClass, HOGModel hogmModel, ExpressionBasedModel expressionBasedModel, List<HOGMProblemError> modelErrors) {
		this.solverClass = solverClass;
		this.hogmModel = hogmModel;
		this.expressionBasedModel = expressionBasedModel;
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
		return () -> getExpressionBasedSolver().solve(queryExpression, expressionBasedModel);
	}

	private void collectParsingErrorResult(HOGMQueryParsing queryParsing) {
		results.add(queryParsing.getParsingErrorProblemResult());
	}

	public void interrupt() {
		canceled = true;
		expressionBasedSolver.interrupt();
	}
	
	public List<HOGMProblemResult> getResults() {
        return results;
    }

	private ExpressionBasedSolver expressionBasedSolver = null;

	private ExpressionBasedSolver getExpressionBasedSolver() {
		if (expressionBasedSolver == null) {
			makeSolver(solverClass);
		}
		return expressionBasedSolver;
	}

	private void makeSolver(Class<? extends ExpressionBasedSolver> solverClass) throws Error {
		try {
			this.expressionBasedSolver = solverClass.newInstance();
		}
		catch (Throwable throwable) {
			throw new Error("Could not instantiate " + solverClass);
		}
	}
}
