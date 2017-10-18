package com.sri.ai.praise.evaluate.run.cli;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.List;

public interface PRAiSEEvaluationArguments extends AutoCloseable {

	public List<String> getSolverImplementationClassNames();

	public PrintStream getNotificationOut();

	public PrintStream getResultOut();

	public int getTotalCPURuntimeLimitSecondsPerSolveAttempt();

	public int getTotalMemoryLimitInMegabytesPerSolveAttempt();

	public int getNumberRunsToAverageOver();

	public boolean isTranslateAlways();

	public File getWorkingDirectory();

	public void close() throws IOException;
}