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
package com.sri.ai.praise.evaluate.run;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.StringJoiner;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.evaluate.solver.SolverEvaluator;
import com.sri.ai.praise.evaluate.solver.SolverEvaluatorConfiguration;
import com.sri.ai.praise.evaluate.solver.SolverEvaluatorProbabilityEvidenceResult;
import com.sri.ai.praise.model.common.io.ModelPage;
import com.sri.ai.praise.model.common.io.PagedModelContainer;
import com.sri.ai.praise.sgsolver.solver.ExpressionFactorsAndTypes;

/**
 * Class responsible for running an evaluation of 1 or more solvers on a given problem set.
 * 
 * @author oreilly
 *
 */
public class Evaluation {	
	// NOTE: Based on http://www.hlt.utdallas.edu/~vgogate/uai14-competition/information.html
	public enum Type {
		PR,   // Computing the the partition function and probability of evidence
		MAR,  // Computing the marginal probability distribution over variable(s) given evidence
		MAP,  // Computing the most likely assignment to all variables given evidence (also known as MPE, Most Probable Explanation)
		MMAP, // Computing the most likely assignment to a subset of variables given evidence (Marginal MAP)
	};
	
	public static class Configuration {
		private Evaluation.Type type;
		private File workingDirectory;
		private int numberRunsToAverageOver;
		
		public Configuration(Evaluation.Type type, File workingDirectory, int numberRunsToAverageOver) {
			this.type                    = type;
			this.workingDirectory        = workingDirectory;
			this.numberRunsToAverageOver = numberRunsToAverageOver;
		}
		
		public Evaluation.Type getType() {
			return type;
		}
		
		public File getWorkingDirectory() {
			return workingDirectory;
		}
		
		public int getNumberRunsToAverageOver() {
			return numberRunsToAverageOver;
		}
	}
	
	public interface Listener {
		void notification(String notification);
		void notificationException(Exception ex);
		void csvResultOutput(String csvLine);
	}
	
	public void evaluate(Evaluation.Configuration configuration, PagedModelContainer modelsToEvaluateContainer, List<SolverEvaluatorConfiguration> solverConfigurations, Evaluation.Listener evaluationListener) {
		// Note, varying domain sizes etc... is achieved by creating variants of a base model in the provided paged model container
		long evaluationStart = System.currentTimeMillis();	
		try {
			List<SolverEvaluator> solvers = instantiateSolvers(solverConfigurations, configuration.getWorkingDirectory());
			
			// Do an initial burn in to ensure any OS caching etc... occurs so as to even out times across runs
			ModelPage burnInModel = modelsToEvaluateContainer.getPages().get(0);
			String    burnInQuery = burnInModel.getDefaultQueriesToRun().get(0);  
			evaluationListener.notification("Starting solver burn in based on '"+modelsToEvaluateContainer.getName()+" - "+burnInModel.getName() + " : " + burnInQuery+"'");
			for (SolverEvaluator solver : solvers) {
				SolverCallResult solverResult =  callSolver(configuration, solver, burnInModel, burnInQuery);
				evaluationListener.notification("Burn in for "+solver.getName()+" complete. Average inference time = "+toDurationString(solverResult.averageInferenceTimeInMilliseconds));
			}
			
			// Output the report header line
			StringJoiner csvLine = new StringJoiner(",");
			csvLine.add("Problem");
			csvLine.add("Inference Type");
			csvLine.add("Domain Size(s)");
			csvLine.add("# runs values averaged over");
			for (SolverEvaluator solver : solvers) {
				csvLine.add("Solver");
				csvLine.add("Result for "+solver.getName());
				csvLine.add("Inference ms. for "+solver.getName());
				csvLine.add("HH:MM:SS.");
				csvLine.add("Translation ms. for "+solver.getName());
				csvLine.add("HH:MM:SS.");
			}
			evaluationListener.notification("Starting to generate Evaluation Report");
			evaluationListener.csvResultOutput(csvLine.toString());
			
			// Now evaluate each of the model-query-solver combinations.
			for (ModelPage model : modelsToEvaluateContainer.getPages()) {
				String domainSizes = getDomainSizes(model.getModel());
				for (String query: model.getDefaultQueriesToRun()) {
					csvLine = new StringJoiner(",");
					String problemName = modelsToEvaluateContainer.getName()+" - "+model.getName() + " : " + query;
					evaluationListener.notification("Starting to evaluate "+problemName);
					csvLine.add(problemName);
					csvLine.add(configuration.type.name());
					csvLine.add(domainSizes);
					csvLine.add(""+configuration.getNumberRunsToAverageOver());
					for (SolverEvaluator solver : solvers) {
						SolverCallResult solverResult = callSolver(configuration, solver, model, query);
						csvLine.add(solver.getName());
						csvLine.add(solverResult.failed ? "FAILED" : ""+solverResult.answer);
						csvLine.add(""+solverResult.averageInferenceTimeInMilliseconds);
						csvLine.add(toDurationString(solverResult.averageInferenceTimeInMilliseconds));
						csvLine.add(""+solverResult.averagelTranslationTimeInMilliseconds);
						csvLine.add(toDurationString(solverResult.averagelTranslationTimeInMilliseconds));
						
						evaluationListener.notification("Solver "+solver.getName()+" took an average inference time of "+toDurationString(solverResult.averageInferenceTimeInMilliseconds)+" to solve "+problemName);
					}
					evaluationListener.csvResultOutput(csvLine.toString());
				}
			}
		}
		catch (Exception ex) {
			evaluationListener.notificationException(ex);
		}
		long evaluationEnd = System.currentTimeMillis();
		evaluationListener.notification("Evaluation took "+toDurationString(evaluationEnd - evaluationStart)+ " to run to completion.");
	}
	
	@SuppressWarnings("unchecked")
	private List<SolverEvaluator> instantiateSolvers(List<SolverEvaluatorConfiguration> solverConfigurations, File workingDirectory) {
		List<SolverEvaluator> result = new ArrayList<>(solverConfigurations.size());
		
		for (SolverEvaluatorConfiguration configuration : solverConfigurations) {
			Class<? extends SolverEvaluator> clazz;
			try {
				clazz = (Class<? extends SolverEvaluator>) Class.forName(configuration.getImplementationClassName());
			}
			catch (ClassNotFoundException cnfe) {
				throw new IllegalArgumentException("Unable to find "+SolverEvaluator.class.getName()+" implementation class: "+configuration.getImplementationClassName(), cnfe);
			}
			try {
				SolverEvaluator solver = clazz.newInstance();
				configuration.setWorkingDirectory(workingDirectory);
				solver.setConfiguration(configuration);
				result.add(solver);
			}
			catch (Throwable t) {
				throw new IllegalStateException("Unable to instantiate instance of "+clazz.getName(), t);
			}
		}
		
		return result;
	}
	
	private SolverCallResult callSolver(Evaluation.Configuration configuration, SolverEvaluator solver, ModelPage model, String query) 
		throws Exception {
		
		SolverCallResult result = new SolverCallResult();	
		
		long sumOfTotalInferenceTimeInMilliseconds   = 0L;
		long sumOfTotalTranslationTimeInMilliseconds = 0L;
		for (int i = 0; i < configuration.getNumberRunsToAverageOver(); i++) {
			if (configuration.type == Type.PR) {
				SolverEvaluatorProbabilityEvidenceResult prResult = solver.solveProbabilityEvidence(model.getName()+" - "+query, 
						model.getLanguage(), model.getModel(), query);
				sumOfTotalInferenceTimeInMilliseconds   += prResult.getTotalInferenceTimeInMilliseconds();
				sumOfTotalTranslationTimeInMilliseconds += prResult.getTotalTranslationTimeInMilliseconds(); 
				if (prResult.getProbabilityOfEvidence() == null) {
					result.failed = true;
				}
				else {
					result.answer = prResult.getProbabilityOfEvidence();
				}
			}
			else {
				throw new UnsupportedOperationException(configuration.type.name()+" type evaluations are currently not supported");
			}
		}
		result.averageInferenceTimeInMilliseconds    = sumOfTotalInferenceTimeInMilliseconds / configuration.getNumberRunsToAverageOver();
		result.averagelTranslationTimeInMilliseconds = sumOfTotalTranslationTimeInMilliseconds / configuration.getNumberRunsToAverageOver();
		
		return result;
	}

	
	private String getDomainSizes(String model) {
		StringJoiner result = new StringJoiner(",");
		
		ExpressionFactorsAndTypes factorsAndTypes = new ExpressionFactorsAndTypes(model);
// TODO - this only works with HOGM models and inequality integer types currently		
		for (com.sri.ai.expresso.api.Type type : factorsAndTypes.getAdditionalTypes()) {
			result.add(""+type.cardinality().intValueExact());
		}
		
		return result.toString();
	}
	
	private String toDurationString(long duration) {
		long hours = 0L, minutes = 0L, seconds = 0L, milliseconds = 0L;
		
		if (duration != 0) {
			hours    = duration / 3600000;
			duration = duration % 3600000; 
		}
		if (duration != 0) {
			minutes  = duration / 60000;
			duration = duration % 60000;
		}
		if (duration != 0) {
			seconds  = duration / 1000;
			duration = duration % 1000;
		}
		milliseconds = duration;
		
		return "" + hours + "h" + minutes + "m" + seconds + "." + milliseconds+"s";
	}
	
	class SolverCallResult {
		Expression answer = null;
		boolean failed = false;
		long averageInferenceTimeInMilliseconds;
		long averagelTranslationTimeInMilliseconds;
	}
}
