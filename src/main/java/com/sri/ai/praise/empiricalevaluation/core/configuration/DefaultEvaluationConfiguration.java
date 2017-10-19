package com.sri.ai.praise.empiricalevaluation.core.configuration;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.empiricalevaluation.api.configuration.EvaluationConfiguration;

public class DefaultEvaluationConfiguration implements EvaluationConfiguration {
	// Optional
	//
	// Defaults to SGSolverEvaluator if not at least 1 specified
	public List<String> solverImplementationClassNames = new ArrayList<>(); // -s
	//
	public PrintStream notificationOut = System.out; // -n
	public PrintStream resultOut = System.out; // -r
	//
	public int totalCPURuntimeLimitSecondsPerSolveAttempt = 600; // -c
	public int totalMemoryLimitInMegabytesPerSolveAttempt = 2048; // -m
	public int numberOfRunsToAverageOver = 10; // -a
	//
	public boolean translateAlways = false; // -t

	// Required
	public File workingDirectory; // -w

	@Override
	public List<String> getSolverImplementationClassNames() {
		return solverImplementationClassNames;
	}

	@Override
	public PrintStream getNotificationOut() {
		return notificationOut;
	}

	@Override
	public PrintStream getResultOut() {
		return resultOut;
	}

	@Override
	public int getTotalCPURuntimeLimitSecondsPerSolveAttempt() {
		return totalCPURuntimeLimitSecondsPerSolveAttempt;
	}

	@Override
	public int getTotalMemoryLimitInMegabytesPerSolveAttempt() {
		return totalMemoryLimitInMegabytesPerSolveAttempt;
	}

	@Override
	public int getNumberOfRunsToAverageOver() {
		return numberOfRunsToAverageOver;
	}

	@Override
	public boolean isTranslateAlways() {
		return translateAlways;
	}

	@Override
	public File getWorkingDirectory() {
		return workingDirectory;
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