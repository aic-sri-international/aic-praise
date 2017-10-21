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

import java.io.File;
import java.util.List;
import java.util.StringJoiner;

import com.sri.ai.praise.empiricalevaluation.core.configuration.SolverConfiguration;
import com.sri.ai.praise.model.common.io.ModelPage;
import com.sri.ai.praise.model.common.io.PagedModelContainer;
import com.sri.ai.praise.pimt.ExpressionFactorsAndTypes;
import com.sri.ai.praise.probabilisticsolver.SolverEvaluator;
import com.sri.ai.praise.probabilisticsolver.SolverEvaluatorConfiguration;
import com.sri.ai.praise.probabilisticsolver.SolverEvaluatorProbabilityEvidenceResult;

/**
 * Class responsible for performing an evaluation of one or more solvers on a given problem set.
 * 
 * @author oreilly
 *
 */
public class Evaluation {	
	
	// NOTE: Based on http://www.hlt.utdallas.edu/~vgogate/uai14-competition/information.html
	public enum ProblemType {
		PR,   // Computing the the partition function and probability of evidence
		MAR,  // Computing the marginal probability distribution over variable(s) given evidence
		MAP,  // Computing the most likely assignment to all variables given evidence (also known as MPE, Most Probable Explanation)
		MMAP, // Computing the most likely assignment to a subset of variables given evidence (Marginal MAP)
	};
	
	public void evaluate(SolverConfiguration configuration, List<SolverEvaluatorConfiguration> solverConfigurations, PagedModelContainer modelsToEvaluateContainer, OutputListener outputListener) {
		// Note, varying domain sizes etc... is achieved by creating variants of a base model in the provided paged model container
		long evaluationStart = System.currentTimeMillis();	
		try {
			evaluateWithoutTiming(configuration, solverConfigurations, modelsToEvaluateContainer, outputListener);
		}
		catch (Exception exception) {
			outputListener.notificationException(exception);
		}
		long evaluationEnd = System.currentTimeMillis();
		outputListener.notification("Evaluation took " + toDurationString(evaluationEnd - evaluationStart) + " to run to completion.");
	}

	private void evaluateWithoutTiming(SolverConfiguration configuration, List<SolverEvaluatorConfiguration> solverConfigurations, PagedModelContainer modelsToEvaluateContainer, OutputListener outputListener) throws Exception {

		List<SolverEvaluator> solvers = setUp(configuration, solverConfigurations, modelsToEvaluateContainer, outputListener);
		evaluateAllModels(solvers, modelsToEvaluateContainer, configuration, outputListener);
	}

	private List<SolverEvaluator> setUp(SolverConfiguration configuration, List<SolverEvaluatorConfiguration> solverConfigurations, PagedModelContainer modelsToEvaluateContainer, OutputListener outputListener) throws Exception {
		
		List<SolverEvaluator> solvers = makeSolvers(solverConfigurations, configuration.getWorkingDirectory());
		
		doInitialBurnInToEnsureOSCachingEtcOccurBeforeMeasuringPerformance(configuration, solvers, modelsToEvaluateContainer, outputListener);
		
		outputReportHeaderLine(solvers, outputListener);
		
		return solvers;
	}

	private List<SolverEvaluator> makeSolvers(List<SolverEvaluatorConfiguration> solverConfigurations, File workingDirectory) {
		List<SolverEvaluator> result = mapIntoList(solverConfigurations, sc -> tryToMakeSolverFromSolverConfiguration(sc, workingDirectory));
		return result;
	}

	private SolverEvaluator tryToMakeSolverFromSolverConfiguration(SolverEvaluatorConfiguration solverConfiguration, File workingDirectory) {
		try {
			return makeSolverFromSolverConfiguration(solverConfiguration, workingDirectory);
		}
		catch (ClassNotFoundException exception) {
			throw makeSolverImplementationNotFoundException(solverConfiguration, exception);
		}
		catch (Throwable throwable) {
			throw makeUnexpectedSolverImplementationError(solverConfiguration, throwable);
		}
	}

	@SuppressWarnings("unchecked")
	private SolverEvaluator makeSolverFromSolverConfiguration(SolverEvaluatorConfiguration solverConfiguration, File workingDirectory) throws ClassNotFoundException, InstantiationException, IllegalAccessException {
		Class<? extends SolverEvaluator> solverImplementationClass = (Class<? extends SolverEvaluator>) Class.forName(solverConfiguration.getImplementationClassName());
		SolverEvaluator result = makeSolver(solverImplementationClass, workingDirectory, solverConfiguration);
		return result;
	}

	private SolverEvaluator makeSolver(Class<? extends SolverEvaluator> clazz, File workingDirectory, SolverEvaluatorConfiguration solverConfiguration) throws InstantiationException, IllegalAccessException {
		SolverEvaluator solver = clazz.newInstance();
		solverConfiguration.setWorkingDirectory(workingDirectory);
		solver.setConfiguration(solverConfiguration);
		return solver;
	}

	private void doInitialBurnInToEnsureOSCachingEtcOccurBeforeMeasuringPerformance(SolverConfiguration configuration, List<SolverEvaluator> solvers, PagedModelContainer modelsToEvaluateContainer, OutputListener outputListener) throws Exception {
		ModelPage burnInModel = modelsToEvaluateContainer.getPages().get(0);
		String    burnInQuery = burnInModel.getDefaultQueriesToRun().get(0);  
		notifyAboutBeginningOfBurnInForAllSolvers(modelsToEvaluateContainer, outputListener, burnInModel, burnInQuery);
		for (SolverEvaluator solver : solvers) {
			performBurnInForSolver(configuration, outputListener, burnInModel, burnInQuery, solver);
		}
	}

	private void notifyAboutBeginningOfBurnInForAllSolvers(PagedModelContainer modelsToEvaluateContainer, OutputListener outputListener, ModelPage burnInModel, String burnInQuery) {
		outputListener.notification("Starting burn in for all solvers based on '" + modelsToEvaluateContainer.getName() + " - " + burnInModel.getName() + " : " + burnInQuery + "'");
	}

	private void performBurnInForSolver(SolverConfiguration configuration, OutputListener outputListener, ModelPage burnInModel, String burnInQuery, SolverEvaluator solver) throws Exception {
		SolverEvaluationRunResult result = new SolverEvaluationRunResult();	
		for (int i = 0; i < configuration.getNumberRunsToAverageOver(); i++) {
			performOneRunOfBurnInForSolver(configuration, burnInModel, burnInQuery, solver, result);
		}
		recordAverageTime(configuration, result);
		notifyAboutOneRunOfBurnInResult(result, solver, outputListener);
	}

	private void performOneRunOfBurnInForSolver(SolverConfiguration configuration, ModelPage burnInModel, String burnInQuery, SolverEvaluator solver, SolverEvaluationRunResult result) throws Exception {
		if (configuration.type == ProblemType.PR) {
			performOneRunOfPRBurnIn(burnInModel, burnInQuery, solver, result);
		}
		else {
			throw new UnsupportedOperationException(configuration.type.name()+" type evaluations are currently not supported");
		}
	}

	private void performOneRunOfPRBurnIn(ModelPage burnInModel, String burnInQuery, SolverEvaluator solver, SolverEvaluationRunResult result) throws Exception {
		SolverEvaluatorProbabilityEvidenceResult burnInResult = getBurnInResult(burnInModel, burnInQuery, solver);
		updateTotalTime(result, burnInResult); 
		recordAnswerResult(burnInResult, result);
	}

	private void updateTotalTime(SolverEvaluationRunResult result, SolverEvaluatorProbabilityEvidenceResult burnInResult) {
		result.sumOfTotalInferenceTimeInMilliseconds   += burnInResult.getTotalInferenceTimeInMilliseconds();
		result.sumOfTotalTranslationTimeInMilliseconds += burnInResult.getTotalTranslationTimeInMilliseconds();
	}

	private SolverEvaluatorProbabilityEvidenceResult getBurnInResult(ModelPage burnInModel, String burnInQuery, SolverEvaluator solver) throws Exception {
		SolverEvaluatorProbabilityEvidenceResult result = 
				solver.solveProbabilityEvidence(
						burnInModel.getName() + " - " + burnInQuery, 
						burnInModel.getLanguage(), 
						burnInModel.getModel(), 
						burnInQuery);
		return result;
	}

	private void recordAnswerResult(SolverEvaluatorProbabilityEvidenceResult prResult, SolverEvaluationRunResult result) {
		if (prResult.getProbabilityOfEvidence() == null) {
			result.failed = true;
		}
		else {
			result.answer = prResult.getProbabilityOfEvidence();
		}
	}

	private void recordAverageTime(SolverConfiguration configuration, SolverEvaluationRunResult result) {
		result.averageInferenceTimeInMilliseconds    = result.sumOfTotalInferenceTimeInMilliseconds / configuration.getNumberRunsToAverageOver();
		result.averagelTranslationTimeInMilliseconds = result.sumOfTotalTranslationTimeInMilliseconds / configuration.getNumberRunsToAverageOver();
	}

	private void notifyAboutOneRunOfBurnInResult(SolverEvaluationRunResult result, SolverEvaluator solver, OutputListener outputListener) {
		outputListener.notification("Burn in for " + solver.getName() + " complete. Average inference time = " + toDurationString(result.averageInferenceTimeInMilliseconds));
	}

	private void outputReportHeaderLine(List<SolverEvaluator> solvers, OutputListener outputListener) {
		StringJoiner csvLine = new StringJoiner(",");
		writeInitialHeader(csvLine);
		for (SolverEvaluator solver : solvers) {
			writeHeaderForSolver(csvLine, solver);
		}
		outputListener.notification("Starting to generate Evaluation Report");
		outputListener.csvResultOutput(csvLine.toString());
	}

	private void writeInitialHeader(StringJoiner csvLine) {
		csvLine.add("Problem");
		csvLine.add("Inference Type");
		csvLine.add("Domain Size(s)");
		csvLine.add("# runs values averaged over");
	}

	private void writeHeaderForSolver(StringJoiner csvLine, SolverEvaluator solver) {
		csvLine.add("Solver");
		csvLine.add("Result for "+solver.getName());
		csvLine.add("Inference ms. for "+solver.getName());
		csvLine.add("HH:MM:SS.");
		csvLine.add("Translation ms. for "+solver.getName());
		csvLine.add("HH:MM:SS.");
	}

	private void evaluateAllModels(List<SolverEvaluator> solvers, PagedModelContainer modelsToEvaluateContainer, SolverConfiguration configuration, OutputListener outputListener) throws Exception {
		for (ModelPage model : modelsToEvaluateContainer.getPages()) {
			evaluateModel(model, modelsToEvaluateContainer, solvers, configuration, outputListener);
		}
	}

	private void evaluateModel(ModelPage model, PagedModelContainer modelsToEvaluateContainer, List<SolverEvaluator> solvers, SolverConfiguration configuration, OutputListener outputListener) throws Exception {
		String domainSizes = getDomainSizes(model.getModel());
		for (String query: model.getDefaultQueriesToRun()) {
			evaluateQuery(query, model, modelsToEvaluateContainer, solvers, domainSizes, configuration, outputListener);
		}
	}

	private void evaluateQuery(String query, ModelPage model, PagedModelContainer modelsToEvaluateContainer, List<SolverEvaluator> solvers, String domainSizes, SolverConfiguration configuration, OutputListener outputListener) throws Exception {
		String problemName = modelsToEvaluateContainer.getName()+" - "+model.getName() + " : " + query;
		StringJoiner csvLine = initializeQueryLine(domainSizes, configuration, outputListener, problemName);
		for (SolverEvaluator solver : solvers) {
			evaluateSolver(solver, query, model, configuration, csvLine, problemName, outputListener);
		}
		outputListener.csvResultOutput(csvLine.toString());
	}

	private StringJoiner initializeQueryLine(String domainSizes, SolverConfiguration configuration, OutputListener outputListener, String problemName) {
		outputListener.notification("Starting to evaluate " + problemName);
		StringJoiner csvLine;
		csvLine = new StringJoiner(",");
		csvLine.add(problemName);
		csvLine.add(configuration.type.name());
		csvLine.add(domainSizes);
		csvLine.add(" " + configuration.getNumberRunsToAverageOver());
		return csvLine;
	}

	private void evaluateSolver(SolverEvaluator solver, String query, ModelPage model, SolverConfiguration configuration, StringJoiner csvLine, String problemName, OutputListener outputListener) throws Exception {
		SolverEvaluationRunResult solverResult = new SolverEvaluationRunResult();	
		
		for (int i = 0; i < configuration.getNumberRunsToAverageOver(); i++) {
			tryToPerformOneRunForConfiguation(solver, query, model, configuration, solverResult);
		}
		recordAverageTime(configuration, solverResult);
		outputSolverOutput(problemName, solver, csvLine, solverResult, outputListener);
	}

	private void tryToPerformOneRunForConfiguation(SolverEvaluator solver, String query, ModelPage model, SolverConfiguration configuration, SolverEvaluationRunResult solverResult) throws Exception {
		if (configuration.type == ProblemType.PR) {
			performOneRun(solver, query, model, solverResult);
		}
		else {
			throw new UnsupportedOperationException(configuration.type.name() + " type evaluations are currently not supported");
		}
	}

	private void performOneRun(SolverEvaluator solver, String query, ModelPage model, SolverEvaluationRunResult solverResult) throws Exception {
		String queryName = model.getName() + " - " + query;
		SolverEvaluatorProbabilityEvidenceResult prResult = solver.solveProbabilityEvidence(queryName, model.getLanguage(), model.getModel(), query);
		solverResult.sumOfTotalInferenceTimeInMilliseconds   += prResult.getTotalInferenceTimeInMilliseconds();
		solverResult.sumOfTotalTranslationTimeInMilliseconds += prResult.getTotalTranslationTimeInMilliseconds(); 
		recordAnswerResult(prResult, solverResult);
	}

	private void outputSolverOutput(String problemName, SolverEvaluator solver, StringJoiner csvLine, SolverEvaluationRunResult solverResult, OutputListener outputListener) {
		csvLine.add(solver.getName());
		csvLine.add(solverResult.failed ? "FAILED" : ""+solverResult.answer);
		csvLine.add(""+solverResult.averageInferenceTimeInMilliseconds);
		csvLine.add(toDurationString(solverResult.averageInferenceTimeInMilliseconds));
		csvLine.add(""+solverResult.averagelTranslationTimeInMilliseconds);
		csvLine.add(toDurationString(solverResult.averagelTranslationTimeInMilliseconds));
		
		outputListener.notification("Solver "+solver.getName()+" took an average inference time of "+toDurationString(solverResult.averageInferenceTimeInMilliseconds)+" to solve "+problemName);
	}

	private String getDomainSizes(String model) {
		StringJoiner result = new StringJoiner(",");
		ExpressionFactorsAndTypes factorsAndTypes = new ExpressionFactorsAndTypes(model);
		for (com.sri.ai.expresso.api.Type type : factorsAndTypes.getAdditionalTypes()) {
			result.add("" + type.cardinality().intValueExact());
		}
		return result.toString();
	}
	
	private String toDurationString(long duration) {
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

	private IllegalStateException makeUnexpectedSolverImplementationError(SolverEvaluatorConfiguration solverConfiguration, Throwable throwable) {
		return new IllegalStateException("Unable to instantiate instance of " + solverConfiguration.getImplementationClassName(), throwable);
	}

	private IllegalArgumentException makeSolverImplementationNotFoundException(SolverEvaluatorConfiguration solverConfiguration, ClassNotFoundException exception) {
		return new IllegalArgumentException("Unable to find " + SolverEvaluator.class.getName() + " implementation class: " + solverConfiguration.getImplementationClassName(), exception);
	}
}
