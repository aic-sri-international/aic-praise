package com.sri.ai.praise.empiricalevaluation.api.configuration;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.List;

import com.sri.ai.praise.empiricalevaluation.core.Evaluation;

public interface Configuration extends AutoCloseable {

	Evaluation.ProblemType getType();
	void setType(Evaluation.ProblemType type);

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