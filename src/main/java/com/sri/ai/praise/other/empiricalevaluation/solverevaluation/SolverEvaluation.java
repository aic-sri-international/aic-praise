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
package com.sri.ai.praise.other.empiricalevaluation.solverevaluation;

import java.lang.reflect.InvocationTargetException;

import com.sri.ai.praise.core.inference.externalprocesssolver.api.ExternalProcessSolver;
import com.sri.ai.praise.core.inference.externalprocesssolver.core.ExternalProcessSolverConfiguration;
import com.sri.ai.praise.core.inference.externalprocesssolver.core.ExternalProcessSolverResult;
import com.sri.ai.praise.other.empiricalevaluation.EvaluationConfiguration;
import com.sri.ai.praise.other.empiricalevaluation.Problem;
import com.sri.ai.praise.other.empiricalevaluation.output.CSVWriter;
import com.sri.ai.praise.other.empiricalevaluation.output.Notifier;
import com.sri.ai.util.Util;

/**
 * The evaluation of a specific {@link ExternalProcessSolver}.
 * 
 * @author oreilly
 * @author braz
 *
 */
public class SolverEvaluation {	
	
	private EvaluationConfiguration configuration;
	public ExternalProcessSolver solver;

	private Notifier notifier;
	private CSVWriter csvWriter;

	public SolverEvaluation(String solverImplementationClassName, Notifier notifier, CSVWriter csvWriter, EvaluationConfiguration configuration) {
		this.configuration = configuration;
		this.solver = makeSolverFromClassName(solverImplementationClassName, configuration);
		this.notifier = notifier;
		this.csvWriter = csvWriter;
	}

	private ExternalProcessSolver makeSolverFromClassName(String solverImplementationClassName, EvaluationConfiguration configuration) {
		ExternalProcessSolverConfiguration solverConfiguration = makeSolverConfiguration(solverImplementationClassName, configuration);
		ExternalProcessSolver solver = makeSolverFromConfiguration(solverConfiguration);
		return solver;
	}
	
	private ExternalProcessSolverConfiguration makeSolverConfiguration(String solverImplementationClassName, EvaluationConfiguration configuration) {
		ExternalProcessSolverConfiguration solverConfiguration = 
				new ExternalProcessSolverConfiguration(
						solverImplementationClassName,
						configuration.getTotalCPURuntimeLimitSecondsPerSolveAttempt(),
						configuration.getTotalMemoryLimitInMegabytesPerSolveAttempt(),
						!configuration.doesNotCacheTranslations(),
						configuration.getWorkingDirectory());
		return solverConfiguration;
	}

	private ExternalProcessSolver makeSolverFromConfiguration(ExternalProcessSolverConfiguration solverConfiguration) {
		Class<ExternalProcessSolver> solverClass = getSolverImplementationClass(solverConfiguration);
		ExternalProcessSolver solver = makeSolverInstance(solverConfiguration, solverClass);
		return solver;
	}

	private Class<ExternalProcessSolver> getSolverImplementationClass(ExternalProcessSolverConfiguration solverConfiguration) {
		String solverClassName = solverConfiguration.getImplementationClassName();
		Class<ExternalProcessSolver> solverClass = getSolverClass(solverClassName);
		return solverClass;
	}

	private ExternalProcessSolver makeSolverInstance(ExternalProcessSolverConfiguration solverConfiguration, Class<ExternalProcessSolver> solverClass) {
		ExternalProcessSolver solver = newSolverInstance(solverClass);
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
			ExternalProcessSolverResult solverResult = solve(problem);
			solverEvaluationResult.aggregateSingleRunSolverResult(solverResult);
		}
		solverEvaluationResult.recordAverageTime(configuration.getNumberOfRunsToAverageOver());
		return solverEvaluationResult;
	}

	/////////////// LOW-LEVEL METHODS
	
	private ExternalProcessSolverResult solve(Problem problem) {
		try {
			ExternalProcessSolverResult result = 
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
	private Class<ExternalProcessSolver> getSolverClass(String solverClassName) {
		return (Class<ExternalProcessSolver>) Util.getClassOrIllegalArgumentException(solverClassName);
	}

	private ExternalProcessSolver newSolverInstance(Class<? extends ExternalProcessSolver> classObject) {
		try {
			return classObject.getDeclaredConstructor().newInstance();
		} catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException exception) {
			throw new IllegalArgumentException(exception);
		}
	}
}
