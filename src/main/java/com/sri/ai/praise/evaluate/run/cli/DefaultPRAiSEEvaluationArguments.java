package com.sri.ai.praise.evaluate.run.cli;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

public class DefaultPRAiSEEvaluationArguments implements PRAiSEEvaluationArguments {
	// Optional
	//
	// Defaults to SGSolverEvaluator if not at least 1 specified
	protected List<String> solverImplementationClassNames = new ArrayList<>(); // -s
	//
	protected PrintStream notificationOut = System.out; // -n
	protected PrintStream resultOut = System.out; // -r
	//
	protected int totalCPURuntimeLimitSecondsPerSolveAttempt = 600; // -c
	protected int totalMemoryLimitInMegabytesPerSolveAttempt = 2048; // -m
	protected int numberRunsToAverageOver = 10; // -a
	//
	protected boolean translateAlways = false; // -t

	// Required
	protected File workingDirectory; // -w

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
	public int getNumberRunsToAverageOver() {
		return numberRunsToAverageOver;
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