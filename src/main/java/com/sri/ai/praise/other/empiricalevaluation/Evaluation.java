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
package com.sri.ai.praise.other.empiricalevaluation;

import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;

import java.util.Collection;
import java.util.List;

import com.sri.ai.expresso.api.Type;
import com.sri.ai.praise.core.model.impure.core.expressionbased.ExpressionBasedModel;
import com.sri.ai.praise.core.model.impure.core.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.model.impure.modelscontainer.ModelPage;
import com.sri.ai.praise.other.empiricalevaluation.output.CSVWriter;
import com.sri.ai.praise.other.empiricalevaluation.output.Notifier;
import com.sri.ai.praise.other.empiricalevaluation.solverevaluation.SolverEvaluation;
import com.sri.ai.util.Util;

/**
 * Evaluation of one or more solvers on a given problem set.
 * 
 * @author oreilly
 * @author braz
 *
 */
public class Evaluation {	
	
	private EvaluationConfiguration evaluationConfiguration;
	private List<SolverEvaluation> solverEvaluations;

	private Notifier notifier;
	private CSVWriter csvWriter;
	private String domainSizesOfCurrentModel;

	public Evaluation(EvaluationConfiguration evaluationConfiguration) {
		this.evaluationConfiguration = evaluationConfiguration;
		this.notifier = new Notifier(evaluationConfiguration.getNotificationOut());
		this.csvWriter = new CSVWriter(evaluationConfiguration);
		this.solverEvaluations = mapIntoList(evaluationConfiguration.getSolverImplementationClassNames(), n -> makeSolverEvaluation(n));
	}
	
	private SolverEvaluation makeSolverEvaluation(String solverImplementationClassName) {
		SolverEvaluation solverEvaluation = new SolverEvaluation(solverImplementationClassName, notifier, csvWriter, evaluationConfiguration);
		return solverEvaluation;
	}

	public void evaluate() {
	
		checkType(evaluationConfiguration.getProblemType());
		
		long evaluationStart = System.currentTimeMillis();	
		initialize();
		evaluateAllModels();
		long evaluationEnd = System.currentTimeMillis();
		
		notifier.notifyAboutTotalEvaluationTime(evaluationStart, evaluationEnd);
	}

	private void initialize() {
		doInitialBurnInToEnsureOSCachingEtcOccurBeforeMeasuringPerformance();
		csvWriter.outputReportHeaderLine(solverEvaluations);
	}

	private void doInitialBurnInToEnsureOSCachingEtcOccurBeforeMeasuringPerformance() {
		Problem problem = makeBurnInProblem();
		notifier.notifyAboutBeginningOfBurnInForAllSolvers(evaluationConfiguration.getModelsContainer(), problem);
		for (SolverEvaluation solverEvaluation : solverEvaluations) {
			solverEvaluation.performBurnIn(problem);
		}
	}

	private Problem makeBurnInProblem() {
		ModelPage burnInModel = evaluationConfiguration.getModelsContainer().getPages().get(0);
		String burnInQuery = burnInModel.getDefaultQueriesToRun().get(0); 
		Problem problem = new Problem(evaluationConfiguration.getProblemType(), burnInQuery, burnInModel);
		return problem;
	}

	private void evaluateAllModels() {
		notifier.notify("Starting to generate Evaluation Report");
		for (ModelPage model : evaluationConfiguration.getModelsContainer().getPages()) {
			evaluateModel(model);
		}
	}

	private void evaluateModel(ModelPage model) {
		domainSizesOfCurrentModel = getDomainSizes(model.getModelString());
		for (String query : model.getDefaultQueriesToRun()) {
			Problem problem = new Problem(evaluationConfiguration.getProblemType(), query, model);
			evaluateProblem(problem);
		}
	}

	private void evaluateProblem(Problem problem) {
		notifier.notify("Starting to evaluate " + problem.name);
		csvWriter.initializeQueryLine(problem, domainSizesOfCurrentModel);
		for (SolverEvaluation solverEvaluation : solverEvaluations) {
			solverEvaluation.evaluate(problem);
		}
		csvWriter.finalizeQueryLine();
	}

	/////////////// LOW-LEVEL METHODS
	
	private void checkType(ProblemType type) {
		myAssert(type == ProblemType.PR, () -> (type + " is current unsupported by " + Evaluation.class));
	}

	private String getDomainSizes(String model) {
		ExpressionBasedModel factorsAndTypes = new HOGMExpressionBasedModel(model);
		Collection<Type> types = factorsAndTypes.getAdditionalTypes();
		List<Integer> domainSizes = mapIntoList(types, t -> t.cardinality().intValueExact());
		String result = Util.join(domainSizes);
		return result;
	}
}
