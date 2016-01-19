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
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.StringJoiner;

import com.sri.ai.praise.evaluate.solver.SolverEvaluator;
import com.sri.ai.praise.evaluate.solver.SolverEvaluatorConfiguration;
import com.sri.ai.praise.evaluate.solver.SolverEvaluatorProbabilityEvidenceResult;
import com.sri.ai.praise.model.common.io.ModelPage;
import com.sri.ai.praise.model.common.io.PagedModelContainer;

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
		
		public Configuration(Evaluation.Type type, File workingDirectory) {
			this.type             = type;
			this.workingDirectory = workingDirectory;
		}
		
		public Evaluation.Type getType() {
			return type;
		}
		
		public File getWorkingDirectory() {
			return workingDirectory;
		}
	}
	
	public interface Listener {
	}
	
	public void evaluate(Evaluation.Configuration configuration, PagedModelContainer modelsToEvaluateContainer, List<SolverEvaluatorConfiguration> solverConfigurations, Evaluation.Listener evaluationListener) {
		// Note, varying domain sizes etc... is achieved by creating variants of a base model in the provided paged model container
			
		try (PrintWriter resultOutput = new PrintWriter(new File(configuration.getWorkingDirectory(), "solver_results.csv"))) {
			List<SolverEvaluator> solvers = instantiateSolvers(solverConfigurations, configuration.getWorkingDirectory());
			
			StringJoiner csvLine = new StringJoiner(",");
			csvLine.add("Solver");
			csvLine.add("Problem");
			csvLine.add("Inference Type");
			csvLine.add("Result");
			csvLine.add("Inference ms.");
			csvLine.add("HH:MM:SS.");
			csvLine.add("Translation ms.");
			csvLine.add("HH:MM:SS.");
			resultOutput.println(csvLine);
			
			for (ModelPage model : modelsToEvaluateContainer.getPages()) {
				
				for (String query: model.getDefaultQueriesToRun()) {
					for (SolverEvaluator solver : solvers) {
						csvLine = new StringJoiner(",");
						csvLine.add(solver.getConfiguration().getName());
						csvLine.add(modelsToEvaluateContainer.getName()+" - "+model.getName() + " : " + query);
						if (configuration.type == Type.PR) {
							csvLine.add(Type.PR.name());
							SolverEvaluatorProbabilityEvidenceResult prResult = solver.solveProbabilityEvidence(model.getName()+" - "+query, 
									model.getLanguage(), model.getModel(), query);
							csvLine.add(prResult.getProbabilityOfEvidence() == null ? "FAILED" : ""+prResult.getProbabilityOfEvidence().doubleValue());
							csvLine.add(""+prResult.getTotalInferenceTimeInMilliseconds());
							csvLine.add(prResult.toTotalInferenceTimeInMillisecondsString());
							csvLine.add(""+prResult.getTotalTranslationTimeInMilliseconds());
							csvLine.add(prResult.toTotalTranslationTimeInMillisecondsString());
						}
						else {
							throw new UnsupportedOperationException(configuration.type.name()+" type evaluations are currently not supported");
						}
						resultOutput.println(csvLine);
						resultOutput.flush();
					}
				}
			}
		}
		catch (Exception ex) {
// TODO - handle properly
			ex.printStackTrace();
		}
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
}
