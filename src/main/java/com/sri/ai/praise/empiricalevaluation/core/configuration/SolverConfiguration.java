package com.sri.ai.praise.empiricalevaluation.core.configuration;

import java.io.File;

import com.sri.ai.praise.empiricalevaluation.core.Evaluation;

public class SolverConfiguration {
	
	public Evaluation.ProblemType type;
	private File workingDirectory;
	private int numberRunsToAverageOver;
	
	public SolverConfiguration(Evaluation.ProblemType type, File workingDirectory, int numberRunsToAverageOver) {
		this.type                    = type;
		this.workingDirectory        = workingDirectory;
		this.numberRunsToAverageOver = numberRunsToAverageOver;
	}
	
	public Evaluation.ProblemType getType() {
		return type;
	}
	
	public File getWorkingDirectory() {
		return workingDirectory;
	}
	
	public int getNumberRunsToAverageOver() {
		return numberRunsToAverageOver;
	}
}