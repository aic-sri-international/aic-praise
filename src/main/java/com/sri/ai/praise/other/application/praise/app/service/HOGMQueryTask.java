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
package com.sri.ai.praise.other.application.praise.app.service;

import static com.sri.ai.util.Util.map;

import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.solver.HOGMMultiQueryProblemSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.solver.HOGMProblemResult;
import com.sri.ai.praise.other.application.praise.app.PRAiSEController;
import com.sri.ai.praise.other.integration.proceduralattachment.api.ProceduralAttachments;
import com.sri.ai.praise.other.integration.proceduralattachment.api.Procedure;
import com.sri.ai.praise.other.integration.proceduralattachment.core.DefaultProceduralAttachments;

import javafx.concurrent.Task;

@Beta
public class HOGMQueryTask extends Task<HOGMProblemResult> {
	private String query;
	private String model;
	//
	private HOGMMultiQueryProblemSolver solver = null;
	
	public HOGMQueryTask(String query, String model) {
		this.query = query;
		this.model = model;
	}
	
	@Override
	public HOGMProblemResult call() {
		final AtomicReference<HOGMProblemResult> result = new AtomicReference<>();
		
		PRAiSEController.computeExpressionWithDesiredPrecision(() -> {
			solver = new HOGMMultiQueryProblemSolver(model, query);
			ProceduralAttachments proceduralAttachments = new DefaultProceduralAttachments(map("ultimateAnswer", (Procedure) p -> 42));
			solver.setProceduralAttachments(proceduralAttachments);
			List<? extends HOGMProblemResult> queryResults = solver.getResults();
			if (queryResults.size() == 1) {
				HOGMProblemResult queryResult = queryResults.get(0);
				if (queryResult.hasErrors()) {
					result.set(queryResult);
				}
				else {
					Expression answer = solver.simplifyAnswer(queryResult.getResult(), queryResult.getQueryExpression());					
					result.set(new HOGMProblemResult(queryResult.getQueryString(), queryResult.getQueryExpression(), queryResult.getParsedModel(), answer, queryResult.getMillisecondsToCompute()));
				}
			}		
		});		
 
        return result.get();
    }
	
	@Override
	protected void cancelled() {
		if (solver != null) {
			solver.interrupt();
		}
	}
}
