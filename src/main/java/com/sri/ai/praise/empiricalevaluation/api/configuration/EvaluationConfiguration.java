package com.sri.ai.praise.empiricalevaluation.api.configuration;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.List;

public interface EvaluationConfiguration extends AutoCloseable {

	List<String> getSolverImplementationClassNames();

	PrintStream getNotificationOut();

	PrintStream getResultOut();

	int getTotalCPURuntimeLimitSecondsPerSolveAttempt();

	int getTotalMemoryLimitInMegabytesPerSolveAttempt();

	int getNumberOfRunsToAverageOver();

	boolean isTranslateAlways();

	File getWorkingDirectory();

	void close() throws IOException;
}