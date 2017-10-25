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
package com.sri.ai.praise.empiricalevaluation.solverevaluation;

import com.sri.ai.praise.empiricalevaluation.Configuration;
import com.sri.ai.praise.empiricalevaluation.Problem;
import com.sri.ai.praise.empiricalevaluation.output.CSVWriter;
import com.sri.ai.praise.empiricalevaluation.output.Notifier;
import com.sri.ai.praise.probabilisticsolver.api.Solver;
import com.sri.ai.praise.probabilisticsolver.core.SolverConfiguration;
import com.sri.ai.praise.probabilisticsolver.core.SolverResult;
import com.sri.ai.util.Util;

/**
 * Class responsible for performing an evaluation of one or more solvers on a given problem set.
 * 
 * @author oreilly, braz
 *
 */
public class SolverEvaluation {	
	
	private Configuration configuration;
	public Solver solver;

	private Notifier notifier;
	private CSVWriter csvWriter;

	public SolverEvaluation(String solverImplementationClassName, Notifier notifier, CSVWriter csvWriter, Configuration configuration) {
		this.configuration = configuration;
		this.solver = makeSolverFromClassName(solverImplementationClassName, configuration);
		this.notifier = notifier;
		this.csvWriter = csvWriter;
	}

	private Solver makeSolverFromClassName(String solverImplementationClassName, Configuration configuration) {
		SolverConfiguration solverConfiguration = makeSolverConfiguration(solverImplementationClassName, configuration);
		Solver solver = makeSolverFromConfiguration(solverConfiguration);
		return solver;
	}
	
	private SolverConfiguration makeSolverConfiguration(String solverImplementationClassName, Configuration configuration) {
		SolverConfiguration solverConfiguration = 
				new SolverConfiguration(
						solverImplementationClassName,
						configuration.getTotalCPURuntimeLimitSecondsPerSolveAttempt(),
						configuration.getTotalMemoryLimitInMegabytesPerSolveAttempt(),
						!configuration.doesNotCacheTranslations(),
						configuration.getWorkingDirectory());
		return solverConfiguration;
	}

	private Solver makeSolverFromConfiguration(SolverConfiguration solverConfiguration) {
		Class<Solver> solverClass = getSolverImplementationClass(solverConfiguration);
		Solver solver = makeSolverInstance(solverConfiguration, solverClass);
		return solver;
	}

	private Class<Solver> getSolverImplementationClass(SolverConfiguration solverConfiguration) {
		String solverClassName = solverConfiguration.getImplementationClassName();
		Class<Solver> solverClass = getSolverClass(solverClassName);
		return solverClass;
	}

	private Solver makeSolverInstance(SolverConfiguration solverConfiguration, Class<Solver> solverClass) {
		Solver solver = newSolverInstance(solverClass);
		solver.setConfiguration(solverConfiguration);
		return solver;
	}

	public void performBurnIn(Problem problem) {
		SolverEvaluationResult solverEvaluationResult = getResultsFromAllRuns(problem);
		notifier.notifyAboutBurnIn(solver.getName(), solverEvaluationResult);
	}

	public void evaluate(Problem problem) {
		SolverEvaluationResult solverEvaluationResult = getResultsFromAllRuns(problem);
		csvWriter.addToQueryLine(solverEvaluationResult);
		notifier.notifyAboutSolverTime(solverEvaluationResult);
	}

	private SolverEvaluationResult getResultsFromAllRuns(Problem problem) {
		SolverEvaluationResult solverEvaluationResult = new SolverEvaluationResult(solver, problem);	
		for (int i = 0; i != configuration.getNumberOfRunsToAverageOver(); i++) {
			SolverResult solverResult = solve(problem);
			solverEvaluationResult.aggregateSingleRunSolverResult(solverResult);
		}
		solverEvaluationResult.recordAverageTime(configuration.getNumberOfRunsToAverageOver());
		return solverEvaluationResult;
	}

	/////////////// LOW-LEVEL METHODS
	
	private SolverResult solve(Problem problem) {
		try {
			SolverResult result = 
					solver.solve(
							problem.model.getName() + " - " + problem.query, 
							problem.model.getLanguage(), 
							problem.model.getModelString(), 
							problem.query);
			return result;
		} catch (Exception exception) {
			throw new IllegalArgumentException(exception);
		}
	}

	@SuppressWarnings("unchecked")
	private Class<Solver> getSolverClass(String solverClassName) {
		return (Class<Solver>) Util.getClassOrIllegalArgumentException(solverClassName);
	}

	private Solver newSolverInstance(Class<? extends Solver> classObject) {
		try {
			return classObject.newInstance();
		} catch (InstantiationException | IllegalAccessException exception) {
			throw new IllegalArgumentException(exception);
		}
	}
}
