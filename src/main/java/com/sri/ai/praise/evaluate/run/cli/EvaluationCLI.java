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
package com.sri.ai.praise.evaluate.run.cli;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import com.sri.ai.praise.evaluate.run.Evaluation;
import com.sri.ai.praise.evaluate.solver.SolverEvaluatorConfiguration;
import com.sri.ai.praise.evaluate.solver.impl.sgsolver.SGSolverEvaluator;
import com.sri.ai.praise.evaluate.solver.impl.vec.VECSolverEvaluator;
import com.sri.ai.praise.model.common.io.PagedModelContainer;

/**
 * Command line interface for running evaluations.
 * 
 * @author oreilly
 *
 */
public class EvaluationCLI {
// TODO - consider using commons-configuration to evaluation input file reading, i.e. https://commons.apache.org/proper/commons-configuration/userguide_v1.10/user_guide.html
	
	public static void main(String[] args) throws Exception {
// TODO - implement based on args
		
		File rootDirectory       = new File("/mnt/hgfs/PPAML/evaluate");
		File inputDirectory      = new File(rootDirectory, "input");
		File outputDirectory     = new File(rootDirectory, "pi20160124");
		File modelsContainerFile = new File(inputDirectory,"randomModel-r3-n1-d2-iv1s1e10.praise");
		
		String evaluationName = "Evaluation randomModel-r3-n1-d2-iv1s1e10";
		int totalCPURuntimeLimitSecondsPerSolveAttempt = 600;
		int totalMemoryLimitInMegabytesPerSolveAttempt = 4096;
		int numberRunsToAverageOver                    = 10;
		
		Evaluation.Configuration           configuration        = new Evaluation.Configuration(Evaluation.Type.PR, outputDirectory, numberRunsToAverageOver);
		PagedModelContainer                modelsContainer      = new PagedModelContainer(evaluationName, PagedModelContainer.getModelPagesFromURI(modelsContainerFile.toURI()));
		List<SolverEvaluatorConfiguration> solverConfigurations = Arrays.asList(			
				new SolverEvaluatorConfiguration("SGSolver", SGSolverEvaluator.class.getName(), 
						totalCPURuntimeLimitSecondsPerSolveAttempt, totalMemoryLimitInMegabytesPerSolveAttempt, Collections.emptyMap()),
				new SolverEvaluatorConfiguration("VEC", VECSolverEvaluator.class.getName(), 
						totalCPURuntimeLimitSecondsPerSolveAttempt, totalMemoryLimitInMegabytesPerSolveAttempt, Collections.emptyMap())
				);
		
		Evaluation evaluation = new Evaluation();
		evaluation.evaluate(configuration, modelsContainer, solverConfigurations, new Evaluation.Listener() {
			public void info(String infoMessage) {
				System.out.println(infoMessage);
			}
		});
	}
}