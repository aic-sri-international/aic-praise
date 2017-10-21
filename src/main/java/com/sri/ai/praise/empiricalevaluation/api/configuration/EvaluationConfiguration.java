package com.sri.ai.praise.empiricalevaluation.api.configuration;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.List;

public interface EvaluationConfiguration extends AutoCloseable {

	List<String> getSolverImplementationClassNames();
	void setSolverImplementationClassNames(List<String> solverImplementationClassNames);

	PrintStream getNotificationOut();
	void setNotificationOut(PrintStream notificationOut);

	PrintStream getResultOut();
	void setResultOut(PrintStream resultOut);

	int getTotalCPURuntimeLimitSecondsPerSolveAttempt();
	void setTotalCPURuntimeLimitSecondsPerSolveAttempt(int totalCPURuntimeLimitSecondsPerSolveAttempt);

	int getTotalMemoryLimitInMegabytesPerSolveAttempt();
	void setTotalMemoryLimitInMegabytesPerSolveAttempt(int totalMemoryLimitInMegabytesPerSolveAttempt);

	int getNumberOfRunsToAverageOver();
	void setNumberOfRunsToAverageOver(int numberOfRunsToAverageOver);

	boolean doesNotCacheTranslations();
	void setDoesNotCacheTranslations(boolean translatedAlways);

	File getWorkingDirectory();
	void setWorkingDirectory(File workingDirectory);

	void close() throws IOException;
}