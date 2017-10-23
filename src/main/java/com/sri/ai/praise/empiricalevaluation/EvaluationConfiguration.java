package com.sri.ai.praise.empiricalevaluation;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.model.common.io.PagedModelContainer;

public class EvaluationConfiguration implements AutoCloseable {

	private PagedModelContainer modelsContainer = null;
	
	private ProblemType type = ProblemType.PR;

	private List<String> solverImplementationClassNames = new ArrayList<>(); // -s

	private PrintStream notificationOut = System.out; // -n
	private PrintStream resultOut = System.out; // -r

	private int totalCPURuntimeLimitSecondsPerSolveAttempt = 600; // -c
	private int totalMemoryLimitInMegabytesPerSolveAttempt = 2048; // -m
	private int numberOfRunsToAverageOver = 10; // -a

	private boolean doesNotCacheTranslations = false; // -t

	private File workingDirectory; // -w
	

	public PagedModelContainer getModelsContainer() {
		return modelsContainer;
	}

	public void setModelsContainer(PagedModelContainer modelsContainer) {
		this.modelsContainer = modelsContainer;
	}

	public ProblemType getType() {
		return type;
	}

	public void setType(ProblemType type) {
		this.type = type;
	}

	public List<String> getSolverImplementationClassNames() {
		return solverImplementationClassNames;
	}

	public void setSolverImplementationClassNames(List<String> solverImplementationClassNames) {
		this.solverImplementationClassNames = solverImplementationClassNames;
	}

	
	public PrintStream getNotificationOut() {
		return notificationOut;
	}

	public void setNotificationOut(PrintStream notificationOut) {
		this.notificationOut = notificationOut;
	}

	
	public PrintStream getCSVOut() {
		return resultOut;
	}

	public void setCSVOut(PrintStream resultOut) {
		this.resultOut = resultOut;
	}

	
	public int getTotalCPURuntimeLimitSecondsPerSolveAttempt() {
		return totalCPURuntimeLimitSecondsPerSolveAttempt;
	}

	public void setTotalCPURuntimeLimitSecondsPerSolveAttempt(int totalCPURuntimeLimitSecondsPerSolveAttempt) {
		this.totalCPURuntimeLimitSecondsPerSolveAttempt = totalCPURuntimeLimitSecondsPerSolveAttempt;
	}

	
	public int getTotalMemoryLimitInMegabytesPerSolveAttempt() {
		return totalMemoryLimitInMegabytesPerSolveAttempt;
	}

	public void setTotalMemoryLimitInMegabytesPerSolveAttempt(int totalMemoryLimitInMegabytesPerSolveAttempt) {
		this.totalMemoryLimitInMegabytesPerSolveAttempt = totalMemoryLimitInMegabytesPerSolveAttempt;
	}

	
	public int getNumberOfRunsToAverageOver() {
		return numberOfRunsToAverageOver;
	}

	public void setNumberOfRunsToAverageOver(int numberOfRunsToAverageOver) {
		this.numberOfRunsToAverageOver = numberOfRunsToAverageOver;
	}

	
	public boolean doesNotCacheTranslations() {
		return doesNotCacheTranslations;
	}

	public void setDoesNotCacheTranslations(boolean doesNotCacheTranslations) {
		this.doesNotCacheTranslations = doesNotCacheTranslations;
	}

	
	public File getWorkingDirectory() {
		return workingDirectory;
	}

	public void setWorkingDirectory(File workingDirectory) {
		this.workingDirectory = workingDirectory;
	}

	
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