package com.sri.ai.praise.empiricalevaluation.core;

import java.util.StringJoiner;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.probabilisticsolver.Solver;
import com.sri.ai.praise.probabilisticsolver.SolverResult;

public class SolverEvaluationResult {
	Solver solver;
	Evaluation.Problem problem;
	Expression answer = null;
	boolean failed = false;
	long averageInferenceTimeInMilliseconds;
	long averagelTranslationTimeInMilliseconds;
	long sumOfTotalInferenceTimeInMilliseconds   = 0L;
	long sumOfTotalTranslationTimeInMilliseconds = 0L;
	
	public SolverEvaluationResult(Solver solver, Evaluation.Problem problem) {
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
	
	public void addToCSVLine(StringJoiner csvLine) {
		csvLine.add(solver.getName());
		csvLine.add(failed ? "FAILED" : "" + answer);
		csvLine.add("" + averageInferenceTimeInMilliseconds);
		csvLine.add(Evaluation.toDurationString(averageInferenceTimeInMilliseconds));
		csvLine.add("" + averagelTranslationTimeInMilliseconds);
		csvLine.add(Evaluation.toDurationString(averagelTranslationTimeInMilliseconds));
	}

}