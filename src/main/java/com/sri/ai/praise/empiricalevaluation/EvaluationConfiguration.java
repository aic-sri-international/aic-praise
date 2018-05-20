/*
 * Copyright (c) 2015, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-praise nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.praise.empiricalevaluation;

import java.io.File;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.model.common.io.PagedModelContainer;

/**
 * Configuration information for an {@link Evaluation}.
 *
 * @author braz
 *
 */
public class EvaluationConfiguration {

	private PagedModelContainer modelsContainer = null;
	
	private ProblemType problemType = ProblemType.PR;

	private List<String> solverImplementationClassNames = new ArrayList<>();

	private PrintStream notificationOut = System.out;
	private PrintStream resultOut = System.out;

	private int totalCPURuntimeLimitSecondsPerSolveAttempt = 600;
	private int totalMemoryLimitInMegabytesPerSolveAttempt = 2048;
	private int numberOfRunsToAverageOver = 10;

	private boolean doesNotCacheTranslations = false;

	private File workingDirectory;
	

	public PagedModelContainer getModelsContainer() {
		return modelsContainer;
	}

	public void setModelsContainer(PagedModelContainer modelsContainer) {
		this.modelsContainer = modelsContainer;
	}

	public ProblemType getProblemType() {
		return problemType;
	}

	public void setProblemType(ProblemType type) {
		this.problemType = type;
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

	
	public PrintStream getResultOut() {
		return resultOut;
	}

	public void setResultOut(PrintStream resultOut) {
		this.resultOut = resultOut;
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
}