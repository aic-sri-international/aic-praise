package com.sri.ai.praise.empiricalevaluation.core;

import com.sri.ai.expresso.api.Expression;

public class SolverEvaluationRunResult {
	Expression answer = null;
	boolean failed = false;
	long averageInferenceTimeInMilliseconds;
	long averagelTranslationTimeInMilliseconds;
	long sumOfTotalInferenceTimeInMilliseconds   = 0L;
	long sumOfTotalTranslationTimeInMilliseconds = 0L;
}