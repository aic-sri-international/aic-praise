package com.sri.ai.praise.empiricalevaluation;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.probabilisticsolver.Solver;
import com.sri.ai.praise.probabilisticsolver.SolverResult;

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