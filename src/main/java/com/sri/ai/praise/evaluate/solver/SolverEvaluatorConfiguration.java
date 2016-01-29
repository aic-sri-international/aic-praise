/*
 * Copyright (c) 2016, SRI International
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
package com.sri.ai.praise.evaluate.solver;

import java.io.File;
import java.util.Map;

/**
 * Configuration information for a solver evaluator.
 * 
 * @author oreilly
 */
public class SolverEvaluatorConfiguration {
	
	private String name;
	private String implementationClassName;
	private int totalCPURuntimeLimitSecondsPerSolveAttempt;
	private int totalMemoryLimitInMegabytesPerSolveAttempt;
	private boolean cacheTranslations;
	private Map<String, String> solverSpecificConfiguration;
	private File workingDirectory;
	
	public SolverEvaluatorConfiguration(String name, String implementationClassName, int totalCPURuntimeLimitSecondsPerSolveAttempt, int totalMemoryLimitInMegabytesPerSolveAttempt, boolean cacheTranslations, Map<String, String> solverSpecificConfiguration) {
		this.name                                       = name;
		this.implementationClassName                    = implementationClassName;
		this.totalCPURuntimeLimitSecondsPerSolveAttempt = totalCPURuntimeLimitSecondsPerSolveAttempt;
		this.totalMemoryLimitInMegabytesPerSolveAttempt = totalMemoryLimitInMegabytesPerSolveAttempt;
		this.cacheTranslations                          = cacheTranslations;
		this.solverSpecificConfiguration                = solverSpecificConfiguration;
	}

	public String getName() {
		return name;
	}

	public String getImplementationClassName() {
		return implementationClassName;
	}

	public int getTotalCPURuntimeLimitSecondsPerSolveAttempt() {
		return totalCPURuntimeLimitSecondsPerSolveAttempt;
	}

	public int getTotalMemoryLimitInMegabytesPerSolveAttempt() {
		return totalMemoryLimitInMegabytesPerSolveAttempt;
	}
	
	public boolean isCacheTranslations() {
		return cacheTranslations;
	}

	public Map<String, String> getSolverSpecificConfiguration() {
		return solverSpecificConfiguration;
	}
	
	public File getWorkingDirectory() {
		return workingDirectory;
	}
	
	public void setWorkingDirectory(File workingDirectory) {
		this.workingDirectory = workingDirectory;
	}
}
