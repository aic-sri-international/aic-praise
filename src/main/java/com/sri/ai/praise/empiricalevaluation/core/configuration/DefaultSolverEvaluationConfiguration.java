package com.sri.ai.praise.empiricalevaluation.core.configuration;

import java.io.File;

import com.sri.ai.praise.empiricalevaluation.api.configuration.SolverEvaluationConfiguration;
import com.sri.ai.praise.empiricalevaluation.core.Evaluation;

public class DefaultSolverEvaluationConfiguration implements SolverEvaluationConfiguration {
	
	private Evaluation.ProblemType type;
	private File workingDirectory;
	private int numberRunsToAverageOver;
	
	public DefaultSolverEvaluationConfiguration(Evaluation.ProblemType type, File workingDirectory, int numberRunsToAverageOver) {
		this.type                    = type;
		this.workingDirectory        = workingDirectory;
		this.numberRunsToAverageOver = numberRunsToAverageOver;
	}
	
	@Override
	public Evaluation.ProblemType getType() {
		return type;
	}
	
	@Override
	public File getWorkingDirectory() {
		return workingDirectory;
	}
	
	@Override
	public int getNumberRunsToAverageOver() {
		return numberRunsToAverageOver;
	}
}