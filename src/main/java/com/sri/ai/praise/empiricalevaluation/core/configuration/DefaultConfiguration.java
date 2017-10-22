package com.sri.ai.praise.empiricalevaluation.core.configuration;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.empiricalevaluation.api.configuration.Configuration;
import com.sri.ai.praise.empiricalevaluation.core.Evaluation;

public class DefaultConfiguration implements Configuration {

	private Evaluation.ProblemType type = Evaluation.ProblemType.PR;

	private List<String> solverImplementationClassNames = new ArrayList<>(); // -s

	private PrintStream notificationOut = System.out; // -n
	private PrintStream resultOut = System.out; // -r

	private int totalCPURuntimeLimitSecondsPerSolveAttempt = 600; // -c
	private int totalMemoryLimitInMegabytesPerSolveAttempt = 2048; // -m
	private int numberOfRunsToAverageOver = 10; // -a

	private boolean doesNotCacheTranslations = false; // -t

	private File workingDirectory; // -w
	

	@Override
	public Evaluation.ProblemType getType() {
		return type;
	}

	@Override
	public void setType(Evaluation.ProblemType type) {
		this.type = type;
	}

	@Override
	public List<String> getSolverImplementationClassNames() {
		return solverImplementationClassNames;
	}

	public void setSolverImplementationClassNames(List<String> solverImplementationClassNames) {
		this.solverImplementationClassNames = solverImplementationClassNames;
	}

	
	@Override
	public PrintStream getNotificationOut() {
		return notificationOut;
	}

	public void setNotificationOut(PrintStream notificationOut) {
		this.notificationOut = notificationOut;
	}

	
	@Override
	public PrintStream getResultOut() {
		return resultOut;
	}

	public void setResultOut(PrintStream resultOut) {
		this.resultOut = resultOut;
	}

	
	@Override
	public int getTotalCPURuntimeLimitSecondsPerSolveAttempt() {
		return totalCPURuntimeLimitSecondsPerSolveAttempt;
	}

	public void setTotalCPURuntimeLimitSecondsPerSolveAttempt(int totalCPURuntimeLimitSecondsPerSolveAttempt) {
		this.totalCPURuntimeLimitSecondsPerSolveAttempt = totalCPURuntimeLimitSecondsPerSolveAttempt;
	}

	
	@Override
	public int getTotalMemoryLimitInMegabytesPerSolveAttempt() {
		return totalMemoryLimitInMegabytesPerSolveAttempt;
	}

	public void setTotalMemoryLimitInMegabytesPerSolveAttempt(int totalMemoryLimitInMegabytesPerSolveAttempt) {
		this.totalMemoryLimitInMegabytesPerSolveAttempt = totalMemoryLimitInMegabytesPerSolveAttempt;
	}

	
	@Override
	public int getNumberOfRunsToAverageOver() {
		return numberOfRunsToAverageOver;
	}

	public void setNumberOfRunsToAverageOver(int numberOfRunsToAverageOver) {
		this.numberOfRunsToAverageOver = numberOfRunsToAverageOver;
	}

	
	@Override
	public boolean doesNotCacheTranslations() {
		return doesNotCacheTranslations;
	}

	public void setDoesNotCacheTranslations(boolean doesNotCacheTranslations) {
		this.doesNotCacheTranslations = doesNotCacheTranslations;
	}

	
	@Override
	public File getWorkingDirectory() {
		return workingDirectory;
	}

	public void setWorkingDirectory(File workingDirectory) {
		this.workingDirectory = workingDirectory;
	}

	
	@Override
	public void close() throws IOException {
		notificationOut.flush();
		resultOut.flush();
		// Only close if not System.out
		if (notificationOut != System.out) {
			notificationOut.close();
		}
		if (resultOut != System.out) {
			resultOut.close();
		}
	}
}