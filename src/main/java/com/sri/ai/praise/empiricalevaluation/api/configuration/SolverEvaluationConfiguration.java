package com.sri.ai.praise.empiricalevaluation.api.configuration;

import java.io.File;

import com.sri.ai.praise.empiricalevaluation.core.Evaluation;

public interface SolverEvaluationConfiguration {

	public Evaluation.ProblemType getType();

	public File getWorkingDirectory();

	public int getNumberRunsToAverageOver();
}