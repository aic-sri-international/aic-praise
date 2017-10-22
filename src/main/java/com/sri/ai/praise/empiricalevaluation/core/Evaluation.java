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
package com.sri.ai.praise.empiricalevaluation.core;

import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;

import java.io.PrintStream;
import java.util.List;
import java.util.StringJoiner;

import com.sri.ai.expresso.api.Type;
import com.sri.ai.praise.empiricalevaluation.api.configuration.Configuration;
import com.sri.ai.praise.model.common.io.ModelPage;
import com.sri.ai.praise.model.common.io.PagedModelContainer;
import com.sri.ai.praise.pimt.ExpressionFactorsAndTypes;
import com.sri.ai.praise.probabilisticsolver.Solver;
import com.sri.ai.praise.probabilisticsolver.SolverConfiguration;
import com.sri.ai.praise.probabilisticsolver.SolverResult;
import com.sri.ai.util.Util;

/**
 * Class responsible for performing an evaluation of one or more solvers on a given problem set.
 * 
 * @author oreilly
 *
 */
public class Evaluation {	
	
	private Configuration configuration;
	private List<SolverConfiguration> solverConfigurations;
	private PagedModelContainer modelsToEvaluateContainer;
	private List<Solver> solvers;

	String domainSizesOfCurrentModel;
	
	private PrintStream notificationOut;
	private PrintStream resultOut;

	public Evaluation(Configuration configuration, List<SolverConfiguration> solverConfigurations, PagedModelContainer modelsToEvaluateContainer, PrintStream notificationOut, PrintStream resultOut) {
		super();
		this.configuration = configuration;
		this.solverConfigurations = solverConfigurations;
		this.modelsToEvaluateContainer = modelsToEvaluateContainer;
		this.notificationOut = notificationOut;
		this.resultOut = resultOut;
	}
	
	public class Problem {
		
		public String query;
		public ModelPage model;
		public String name;
		
		public Problem(String query, ModelPage model) {
			super();
			this.query = query;
			this.model = model;
			this.name = modelsToEvaluateContainer.getName() + " - " + model.getName() + " : " + query;
		}
	}

	// Based on http://www.hlt.utdallas.edu/~vgogate/uai14-competition/information.html
	public enum ProblemType {
		PR,   // Computing the the partition function and probability of evidence
		MAR,  // Computing the marginal probability distribution over variable(s) given evidence
		MAP,  // Computing the most likely assignment to all variables given evidence (also known as MPE, Most Probable Explanation)
		MMAP, // Computing the most likely assignment to a subset of variables given evidence (Marginal MAP)
	};
	
	public void evaluate() {
	
		checkType(configuration.getType());
		
		long evaluationStart = System.currentTimeMillis();	
		initialize();
		evaluateAllModels();
		long evaluationEnd = System.currentTimeMillis();
		
		notifyAboutTotalEvaluationTime(evaluationStart, evaluationEnd);
	}

	private void initialize() {
		initializeSolvers();
		outputReportHeaderLine();
	}

	private void initializeSolvers() {
		makeSolvers();
		doInitialBurnInToEnsureOSCachingEtcOccurBeforeMeasuringPerformance();
	}

	private void makeSolvers() {
		solvers = mapIntoList(solverConfigurations, this::makeSolver);
	}

	private Solver makeSolver(SolverConfiguration solverConfiguration) {
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

	private void doInitialBurnInToEnsureOSCachingEtcOccurBeforeMeasuringPerformance() {
		Problem problem = makeBurnInProblem();
		notifyAboutBeginningOfBurnInForAllSolvers(modelsToEvaluateContainer, problem);
		for (Solver solver : solvers) {
			performBurnInForSolver(solver, problem);
		}
	}

	private Problem makeBurnInProblem() {
		ModelPage burnInModel = modelsToEvaluateContainer.getPages().get(0);
		String    burnInQuery = burnInModel.getDefaultQueriesToRun().get(0); 
		Problem problem = new Problem(burnInQuery, burnInModel);
		return problem;
	}

	private void performBurnInForSolver(Solver solver, Problem problem) {
		SolverEvaluationResult solverEvaluationResult = getResultsFromAllRunsForSolver(solver, problem);
		notifyAboutBurnIn(solver, solverEvaluationResult);
	}

	private void evaluateAllModels() {
		for (ModelPage model : modelsToEvaluateContainer.getPages()) {
			evaluateModel(model);
		}
	}

	private void evaluateModel(ModelPage model) {
		domainSizesOfCurrentModel = getDomainSizes(model.getModelString());
		for (String query : model.getDefaultQueriesToRun()) {
			Problem problem = new Problem(query, model);
			evaluateProblem(problem);
		}
	}

	private void evaluateProblem(Problem problem) {
		initializeQueryLine(problem);
		for (Solver solver : solvers) {
			evaluateSolver(solver, problem);
		}
		finalizeQueryLine();
	}

	private void evaluateSolver(Solver solver, Problem problem) {
		SolverEvaluationResult solverEvaluationResult = getResultsFromAllRunsForSolver(solver, problem);
		addToQueryLine(solverEvaluationResult);
		notifyAboutSolverTime(solverEvaluationResult);
	}

	private SolverEvaluationResult getResultsFromAllRunsForSolver(Solver solver, Problem problem) {
		SolverEvaluationResult solverEvaluationResult = new SolverEvaluationResult(solver, problem);	
		for (int i = 0; i != configuration.getNumberOfRunsToAverageOver(); i++) {
			SolverResult solverResult = solve(solver, problem);
			solverEvaluationResult.aggregateSingleRunSolverResult(solverResult);
		}
		solverEvaluationResult.recordAverageTime(configuration.getNumberOfRunsToAverageOver());
		return solverEvaluationResult;
	}

	
	
	
	/////////////// LOW-LEVEL METHODS
	
	
	private SolverResult solve(Solver solver, Problem problem) {
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

	private void checkType(ProblemType type) {
		myAssert(type == ProblemType.PR, () -> unsupported(type));
	}

	private String getDomainSizes(String model) {
		StringJoiner result = new StringJoiner(",");
		ExpressionFactorsAndTypes factorsAndTypes = new ExpressionFactorsAndTypes(model);
		for (Type type : factorsAndTypes.getAdditionalTypes()) {
			result.add(type.cardinality().intValueExact() + "");
		}
		return result.toString();
	}

	private String unsupported(ProblemType type) {
		return type + " is current unsupported by " + Evaluation.class;
	}

	//////// OUTPUT METHODS

	// Notifications
	
	private void notification(String notification) {
		notificationOut.println(notification);
	}

	private void notifyAboutSolverTime(SolverEvaluationResult solverEvaluationResult) {
		notification("Solver " + solverEvaluationResult.solver.getName() + " took an average inference time of " + toDurationString(solverEvaluationResult.averageInferenceTimeInMilliseconds) + " to solve " + solverEvaluationResult.problem.name);
	}

	private void notifyAboutTotalEvaluationTime(long evaluationStart, long evaluationEnd) {
		notification("Evaluation took " + toDurationString(evaluationEnd - evaluationStart) + " to run to completion.");
	}

	private void notifyAboutBeginningOfBurnInForAllSolvers(PagedModelContainer modelsToEvaluateContainer, Problem problem) {
		notification("Starting burn in for all solvers based on '" + modelsToEvaluateContainer.getName() + " - " + problem.model.getName() + " : " + problem.query + "'");
	}

	private void notifyAboutBurnIn(Solver solver, SolverEvaluationResult result) {
		notification("Burn in for " + solver.getName() + " complete. Average inference time = " + toDurationString(result.averageInferenceTimeInMilliseconds));
	}

	//// CSV - Header
	
	private void writeInitialHeader(StringJoiner csvLine) {
		csvLine.add("Problem");
		csvLine.add("Inference Type");
		csvLine.add("Domain Size(s)");
		csvLine.add("# runs values averaged over");
	}

	private void outputReportHeaderLine() {
		StringJoiner cvsHeaderLine = new StringJoiner(",");
		writeInitialHeader(cvsHeaderLine);
		for (Solver solver : solvers) {
			writeHeaderForSolver(solver, cvsHeaderLine);
		}
		notification("Starting to generate Evaluation Report");
		csvResultOutput(cvsHeaderLine.toString());
	}

	private void writeHeaderForSolver(Solver solver, StringJoiner cvsHeaderLine) {
		cvsHeaderLine.add("Solver");
		cvsHeaderLine.add("Result for " + solver.getName());
		cvsHeaderLine.add("Inference ms. for " + solver.getName());
		cvsHeaderLine.add("HH:MM:SS.");
		cvsHeaderLine.add("Translation ms. for " + solver.getName());
		cvsHeaderLine.add("HH:MM:SS.");
	}

	//// CSV - Query line
	
	StringJoiner queryCSVLine;
	
	private void initializeQueryLine(Problem problem) {
		notification("Starting to evaluate " + problem.name);
		queryCSVLine = new StringJoiner(",");
		queryCSVLine.add(problem.name);
		queryCSVLine.add(configuration.getType().name());
		queryCSVLine.add(domainSizesOfCurrentModel);
		queryCSVLine.add(" " + configuration.getNumberOfRunsToAverageOver());
	}

	private void addToQueryLine(SolverEvaluationResult solverEvaluationResult) {
		solverEvaluationResult.addToCSVLine(queryCSVLine);
	}

	private void finalizeQueryLine() {
		csvResultOutput(queryCSVLine.toString());
	}

	private void csvResultOutput(String csvLine) {
		resultOut.println(csvLine);
	}

	static String toDurationString(long duration) {
		long hours = 0L, minutes = 0L, seconds = 0L, milliseconds = 0L;
		long remainingDuration = duration;
		
		if (remainingDuration != 0) {
			hours    = remainingDuration / 3600000;
			remainingDuration = remainingDuration % 3600000; 
		}
		if (remainingDuration != 0) {
			minutes  = remainingDuration / 60000;
			remainingDuration = remainingDuration % 60000;
		}
		if (remainingDuration != 0) {
			seconds  = remainingDuration / 1000;
			remainingDuration = remainingDuration % 1000;
		}
		milliseconds = remainingDuration;
		
		return hours + "h" + minutes + "m" + seconds + "." + milliseconds + "s";
	}
}
