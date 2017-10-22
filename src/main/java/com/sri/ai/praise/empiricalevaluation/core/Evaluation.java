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
import com.sri.ai.praise.empiricalevaluation.api.configuration.SolverEvaluationConfiguration;
import com.sri.ai.praise.lang.ModelLanguage;
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
	
	private SolverEvaluationConfiguration solverEvaluationConfiguration;
	private List<SolverConfiguration> solverConfigurations;
	private PagedModelContainer modelsToEvaluateContainer;
	private List<Solver> solvers;
	
	private PrintStream notificationOut;
	private PrintStream resultOut;

	public Evaluation(SolverEvaluationConfiguration solverEvaluationConfiguration, List<SolverConfiguration> solverConfigurations, PagedModelContainer modelsToEvaluateContainer, PrintStream notificationOut, PrintStream resultOut) {
		super();
		this.solverEvaluationConfiguration = solverEvaluationConfiguration;
		this.solverConfigurations = solverConfigurations;
		this.modelsToEvaluateContainer = modelsToEvaluateContainer;
		this.notificationOut = notificationOut;
		this.resultOut = resultOut;

	}

	// Based on http://www.hlt.utdallas.edu/~vgogate/uai14-competition/information.html
	public enum ProblemType {
		PR,   // Computing the the partition function and probability of evidence
		MAR,  // Computing the marginal probability distribution over variable(s) given evidence
		MAP,  // Computing the most likely assignment to all variables given evidence (also known as MPE, Most Probable Explanation)
		MMAP, // Computing the most likely assignment to a subset of variables given evidence (Marginal MAP)
	};
	
	public void evaluate() {
	
		checkType(solverEvaluationConfiguration.getType());
		
		long evaluationStart = System.currentTimeMillis();	
		initializeSolversAndHeader();
		evaluateAllModels();
		long evaluationEnd = System.currentTimeMillis();
		
		notifyTotalEvaluationTime(evaluationStart, evaluationEnd);
	}

	private void initializeSolversAndHeader() {
		makeSolvers();
		doInitialBurnInToEnsureOSCachingEtcOccurBeforeMeasuringPerformance();
		outputReportHeaderLine();
	}

	private void makeSolvers() {
		solvers = mapIntoList(solverConfigurations, this::makeSolver);
	}

	private Solver makeSolver(SolverConfiguration solverConfiguration) {
		String solverClassName = solverConfiguration.getImplementationClassName();
		Class<Solver> solverClass = getSolverClass(solverClassName);
		Solver solver = newSolverInstance(solverClass);
		solver.setConfiguration(solverConfiguration);
		return solver;
	}

	private void doInitialBurnInToEnsureOSCachingEtcOccurBeforeMeasuringPerformance() {
		ModelPage burnInModel = modelsToEvaluateContainer.getPages().get(0);
		String    burnInQuery = burnInModel.getDefaultQueriesToRun().get(0);  
		notifyAboutBeginningOfBurnInForAllSolvers(modelsToEvaluateContainer, burnInModel, burnInQuery);
		for (Solver solver : solvers) {
			performBurnInForSolver(solver, burnInQuery, burnInModel);
		}
	}

	private void performBurnInForSolver(Solver solver, String query, ModelPage model) {
		SolverEvaluationResult solverEvaluationResult = runAllRunsForSolver(solver, query, model);
		notifyAboutBurnIn(solver, solverEvaluationResult);
	}

	private void evaluateAllModels() {
		for (ModelPage model : modelsToEvaluateContainer.getPages()) {
			evaluateModel(model);
		}
	}

	private void evaluateModel(ModelPage model) {
		String domainSizes = getDomainSizes(model.getModel());
		for (String query: model.getDefaultQueriesToRun()) {
			evaluateQuery(query, model, domainSizes);
		}
	}

	private String getDomainSizes(String model) {
		StringJoiner result = new StringJoiner(",");
		ExpressionFactorsAndTypes factorsAndTypes = new ExpressionFactorsAndTypes(model);
		for (Type type : factorsAndTypes.getAdditionalTypes()) {
			result.add(type.cardinality().intValueExact() + "");
		}
		return result.toString();
	}

	private void evaluateQuery(String query, ModelPage model, String domainSizes) {
		String problemName = modelsToEvaluateContainer.getName() + " - " + model.getName() + " : " + query;
		initializeQueryLine(problemName, domainSizes);
		for (Solver solver : solvers) {
			evaluateSolver(solver, model, query, problemName);
		}
		finalizeQueryLine();
	}

	private void evaluateSolver(Solver solver, ModelPage model, String query, String problemName) {
		SolverEvaluationResult solverEvaluationResult = runAllRunsForSolver(solver, query, model);
		outputSolverOutput(problemName, solver, solverEvaluationResult);
	}

	private SolverEvaluationResult runAllRunsForSolver(Solver solver, String query, ModelPage model) {
		SolverEvaluationResult solverEvaluationResult = new SolverEvaluationResult();	
		for (int i = 0; i < solverEvaluationConfiguration.getNumberRunsToAverageOver(); i++) {
			SolverResult solverResult = performOneRun(solver, model, query);
			solverEvaluationResult.aggregateSingleRunSolverResult(solverResult);
		}
		solverEvaluationResult.recordAverageTime(solverEvaluationConfiguration.getNumberRunsToAverageOver());
		return solverEvaluationResult;
	}

	private SolverResult performOneRun(Solver solver, ModelPage model, String query) {
		String queryName = model.getName() + " - " + query;
		SolverResult solverResult = solve(solver, queryName, model.getLanguage(), model.getModel(), query);
		return solverResult;
	}

	
	
	
	/////////////// LOW-LEVEL METHODS
	
	
	private SolverResult solve(Solver solver, String solveRequestId, ModelLanguage language, String model, String query) {
		try {
			SolverResult result = 
					solver.solve(
							solveRequestId, 
							language, 
							model, 
							query);
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

	private String unsupported(ProblemType type) {
		return type + " is current unsupported by " + Evaluation.class;
	}

	//////// OUTPUT METHODS
	
	StringJoiner csvLine;
	
	private void notification(String notification) {
		notificationOut.println(notification);
	}

	private void initializeQueryLine(String problemName, String domainSizes) {
		notification("Starting to evaluate " + problemName);
		csvLine = new StringJoiner(",");
		csvLine.add(problemName);
		csvLine.add(solverEvaluationConfiguration.getType().name());
		csvLine.add(domainSizes);
		csvLine.add(" " + solverEvaluationConfiguration.getNumberRunsToAverageOver());
	}

	private void finalizeQueryLine() {
		csvResultOutput(csvLine.toString());
	}

	private void csvResultOutput(String csvLine) {
		resultOut.println(csvLine);
	}

	private void notifyTotalEvaluationTime(long evaluationStart, long evaluationEnd) {
		notification("Evaluation took " + toDurationString(evaluationEnd - evaluationStart) + " to run to completion.");
	}

	private void notifyAboutBeginningOfBurnInForAllSolvers(PagedModelContainer modelsToEvaluateContainer, ModelPage burnInModel, String burnInQuery) {
		notification("Starting burn in for all solvers based on '" + modelsToEvaluateContainer.getName() + " - " + burnInModel.getName() + " : " + burnInQuery + "'");
	}

	private void notifyAboutBurnIn(Solver solver, SolverEvaluationResult result) {
		notification("Burn in for " + solver.getName() + " complete. Average inference time = " + toDurationString(result.averageInferenceTimeInMilliseconds));
	}

	private void writeInitialHeader(StringJoiner csvLine) {
		csvLine.add("Problem");
		csvLine.add("Inference Type");
		csvLine.add("Domain Size(s)");
		csvLine.add("# runs values averaged over");
	}

	private void outputReportHeaderLine() {
		StringJoiner csvLine = new StringJoiner(",");
		writeInitialHeader(csvLine);
		for (Solver solver : solvers) {
			writeHeaderForSolver(solver, csvLine);
		}
		notification("Starting to generate Evaluation Report");
		csvResultOutput(csvLine.toString());
	}

	private void writeHeaderForSolver(Solver solver, StringJoiner csvLine) {
		csvLine.add("Solver");
		csvLine.add("Result for " + solver.getName());
		csvLine.add("Inference ms. for " + solver.getName());
		csvLine.add("HH:MM:SS.");
		csvLine.add("Translation ms. for " + solver.getName());
		csvLine.add("HH:MM:SS.");
	}

	private void outputSolverOutput(String problemName, Solver solver, SolverEvaluationResult solverEvaluationResult) {
		solverEvaluationResult.output(problemName, solver, csvLine);
		notification("Solver " + solver.getName() + " took an average inference time of " + toDurationString(solverEvaluationResult.averageInferenceTimeInMilliseconds) + " to solve " + problemName);
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
