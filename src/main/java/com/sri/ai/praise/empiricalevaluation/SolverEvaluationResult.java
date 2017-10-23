package com.sri.ai.praise.empiricalevaluation;

import java.util.StringJoiner;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.probabilisticsolver.Solver;
import com.sri.ai.praise.probabilisticsolver.SolverResult;
import com.sri.ai.util.Util;

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
	
	public void addToCSVLine(StringJoiner csvLine) {
		csvLine.add(solver.getName());
		csvLine.add(failed ? "FAILED" : "" + answer);
		csvLine.add("" + averageInferenceTimeInMilliseconds);
		csvLine.add(Util.toHoursMinutesAndSecondsString(averageInferenceTimeInMilliseconds));
		csvLine.add("" + averagelTranslationTimeInMilliseconds);
		csvLine.add(Util.toHoursMinutesAndSecondsString(averagelTranslationTimeInMilliseconds));
	}

}