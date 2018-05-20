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
package com.sri.ai.praise.empiricalevaluation.solverevaluation;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.empiricalevaluation.Problem;
import com.sri.ai.praise.probabilisticsolver.api.Solver;
import com.sri.ai.praise.probabilisticsolver.core.SolverResult;

/**
 * The results of a {@link SolverEvaluation}.
 * 
 * @author oreilly
 * @author braz
 *
 */
public class SolverEvaluationResult {
	
	public Solver solver;
	public Problem problem;
	public Expression answer = null;
	public boolean failed = false;
	public long averageInferenceTimeInMilliseconds;
	public long averagelTranslationTimeInMilliseconds;
	public long sumOfTotalInferenceTimeInMilliseconds   = 0L;
	public long sumOfTotalTranslationTimeInMilliseconds = 0L;
	
	public SolverEvaluationResult(Solver solver, Problem problem) {
		this.solver = solver;
		this.problem = problem;
	}
	
	public void aggregateSingleRunSolverResult(SolverResult solverResult) {
		updateTime(solverResult);
		updateAnswer(solverResult);
	}

	private void updateTime(SolverResult solverResult) {
		sumOfTotalInferenceTimeInMilliseconds   += solverResult.getTotalInferenceTimeInMilliseconds();
		sumOfTotalTranslationTimeInMilliseconds += solverResult.getTotalTranslationTimeInMilliseconds();
	}
	
	private void updateAnswer(SolverResult solverResult) {
		if (solverResult.getProbabilityOfEvidence() == null) {
			failed = true;
		}
		else {
			answer = solverResult.getProbabilityOfEvidence();
		}
	}

	public void recordAverageTime(int numberOfRuns) {
		averageInferenceTimeInMilliseconds    = sumOfTotalInferenceTimeInMilliseconds / numberOfRuns;
		averagelTranslationTimeInMilliseconds = sumOfTotalTranslationTimeInMilliseconds / numberOfRuns;
	}
}