package com.sri.ai.praise.empiricalevaluation.core;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.empiricalevaluation.api.configuration.SolverEvaluationConfiguration;
import com.sri.ai.praise.probabilisticsolver.SolverResult;

public class SolverEvaluationResult {
	Expression answer = null;
	boolean failed = false;
	long averageInferenceTimeInMilliseconds;
	long averagelTranslationTimeInMilliseconds;
	long sumOfTotalInferenceTimeInMilliseconds   = 0L;
	long sumOfTotalTranslationTimeInMilliseconds = 0L;
	
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