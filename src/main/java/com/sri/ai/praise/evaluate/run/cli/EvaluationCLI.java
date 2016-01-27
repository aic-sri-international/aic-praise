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
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import com.sri.ai.praise.evaluate.run.Evaluation;
import com.sri.ai.praise.evaluate.solver.SolverEvaluatorConfiguration;
import com.sri.ai.praise.evaluate.solver.impl.sgsolver.SGSolverEvaluator;
import com.sri.ai.praise.evaluate.solver.impl.vec.VECSolverEvaluator;
import com.sri.ai.praise.model.common.io.PagedModelContainer;

import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;

/**
 * Command line interface for running evaluations.
 * 
 * @author oreilly
 *
 */
public class EvaluationCLI {
// TODO - consider using commons-configuration to evaluation input file reading, i.e. https://commons.apache.org/proper/commons-configuration/userguide_v1.10/user_guide.html
	
	static class EvaluationArgs implements AutoCloseable {
		// Optional
		PrintStream  notificationOut = System.out; // -n
		PrintStream  resultOut       = System.out; // -r 
		//
		int totalCPURuntimeLimitSecondsPerSolveAttempt = 600;  // -c
		int totalMemoryLimitInMegabytesPerSolveAttempt = 4096; // -m
		int numberRunsToAverageOver                    = 10;   // -a
				
		// Required
		File workingDirectory; // -w
		File praiseModelsFile; // -p
		
		@Override
		public void close() throws IOException {
			notificationOut.flush();
			resultOut.flush();
			// Only close if not System.out
			if (notificationOut != System.out) {				
				notificationOut.close();
			}
			if (resultOut != System.out) {
				resultOut.close();
			}
		}
	}
	
	public static void main(String[] args) throws Exception {		
		try (EvaluationArgs evaluationArgs = getArgs(args)) {			
			Evaluation.Configuration           configuration        = new Evaluation.Configuration(Evaluation.Type.PR, evaluationArgs.workingDirectory, evaluationArgs.numberRunsToAverageOver);
			PagedModelContainer                modelsContainer      = new PagedModelContainer(evaluationArgs.praiseModelsFile.getName(), PagedModelContainer.getModelPagesFromURI(evaluationArgs.praiseModelsFile.toURI()));
			List<SolverEvaluatorConfiguration> solverConfigurations = Arrays.asList(			
					new SolverEvaluatorConfiguration("SGSolver", SGSolverEvaluator.class.getName(), 
							evaluationArgs.totalCPURuntimeLimitSecondsPerSolveAttempt, 
							evaluationArgs.totalMemoryLimitInMegabytesPerSolveAttempt, Collections.emptyMap()),
					new SolverEvaluatorConfiguration("VEC", VECSolverEvaluator.class.getName(), 
							evaluationArgs.totalCPURuntimeLimitSecondsPerSolveAttempt, 
							evaluationArgs.totalMemoryLimitInMegabytesPerSolveAttempt, Collections.emptyMap())
					);
			
			Evaluation evaluation = new Evaluation();
			evaluation.evaluate(configuration, modelsContainer, solverConfigurations, new Evaluation.Listener() {
				public void notification(String notification) {
					evaluationArgs.notificationOut.println(notification);
				}
				
				public void notificationException(Exception ex) {
					ex.printStackTrace(evaluationArgs.notificationOut);
				}
				
				public void csvResultOutput(String csvLine) {
					evaluationArgs.resultOut.println(csvLine);
				}
			});
		}
	}
	
	//
	// PRIVATE
	private static EvaluationArgs getArgs(String[] args) throws UnsupportedEncodingException, FileNotFoundException, IOException {
		EvaluationArgs result = new EvaluationArgs();
		
		OptionParser parser = new OptionParser();
		// Optional
		OptionSpec<File> notificationFile = parser.accepts("n", "Notification output file name (default to stdout).").withRequiredArg().ofType(File.class);
		OptionSpec<File> resultFile       = parser.accepts("r", "Result output file name (defaults to stdout).").withRequiredArg().ofType(File.class);
		//
		OptionSpec<Integer> totalCPURuntimeLimitSecondsPerSolveAttempt = parser.accepts("c", "Total CPU runtime limit seconds per solver attempt (defaults to "+result.totalCPURuntimeLimitSecondsPerSolveAttempt+").").withRequiredArg().ofType(Integer.class);
		OptionSpec<Integer> totalMemoryLimitInMegabytesPerSolveAttempt = parser.accepts("m", "Total memory limit in MB per solver attempt (defaults to "+result.totalMemoryLimitInMegabytesPerSolveAttempt+").").withRequiredArg().ofType(Integer.class);
		OptionSpec<Integer> numberRunsToAverageOver                    = parser.accepts("a", "Number of runs to average each result over (defaults to "+result.numberRunsToAverageOver+").").withRequiredArg().ofType(Integer.class);

		// Required
		OptionSpec<File> praiseModelsFile  = parser.accepts("p", "The PRAiSE Models file used as input for the evaluations").withRequiredArg().required().ofType(File.class);
		OptionSpec<File> workingDirectory  = parser.accepts("w", "Solver Working Directory (temp directories and files will be created under here)").withRequiredArg().required().ofType(File.class);
		
		//
		parser.accepts("help").forHelp();
		
		OptionSet options = parser.parse(args);
		
		if (options.has("help")) {
			parser.printHelpOn(System.out);
			System.exit(0);
		}
		
		// 
		// Handle optional args
		if (options.has(notificationFile)) {
			result.notificationOut = new PrintStream(options.valueOf(notificationFile));
		}
		if (options.has(resultFile)) {
			result.resultOut = new PrintStream(options.valueOf(resultFile));
		}
		if (options.has(totalCPURuntimeLimitSecondsPerSolveAttempt)) {
			result.totalCPURuntimeLimitSecondsPerSolveAttempt = options.valueOf(totalCPURuntimeLimitSecondsPerSolveAttempt);
		}
		if (options.has(totalMemoryLimitInMegabytesPerSolveAttempt)) {
			result.totalMemoryLimitInMegabytesPerSolveAttempt = options.valueOf(totalMemoryLimitInMegabytesPerSolveAttempt);
		}
		if (options.has(numberRunsToAverageOver)) {
			result.numberRunsToAverageOver = options.valueOf(numberRunsToAverageOver);
		}
		
		result.praiseModelsFile = options.valueOf(praiseModelsFile);
		if (!result.praiseModelsFile.isFile()) {
			throw new IllegalArgumentException("Input PRAiSE models file does not exist: "+result.praiseModelsFile.getAbsolutePath());
		}
		result.workingDirectory = options.valueOf(workingDirectory);
		if (!result.workingDirectory.isDirectory()) {
			throw new IllegalArgumentException("Working directory does not exist: "+result.workingDirectory);
		}
		
		return result;
	}
}