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
package com.sri.ai.praise.empiricalevaluation.core.output;

import java.io.PrintStream;
import java.util.List;
import java.util.StringJoiner;

import com.sri.ai.praise.empiricalevaluation.core.Problem;
import com.sri.ai.praise.empiricalevaluation.core.SolverEvaluationResult;
import com.sri.ai.praise.probabilisticsolver.Solver;

/**
 * Class responsible for performing an evaluation of one or more solvers on a given problem set.
 * 
 * @author oreilly
 *
 */
public class CSVWriter {	
	
	private String problemTypeName;
	private int numberOfRunsToAverageOver;
	private PrintStream csvOut;

	public CSVWriter(String problemType, int numberOfRunsToAverageOver, PrintStream csvOut) {
		super();
		this.problemTypeName = problemType;
		this.numberOfRunsToAverageOver = numberOfRunsToAverageOver;
		this.csvOut = csvOut;
	}
	
	// Header methods
	
	public void writeInitialHeader(StringJoiner csvLine) {
		csvLine.add("Problem");
		csvLine.add("Inference Type");
		csvLine.add("Domain Size(s)");
		csvLine.add("# runs values averaged over");
	}

	public void outputReportHeaderLine(List<Solver> solvers) {
		StringJoiner cvsHeaderLine = new StringJoiner(",");
		writeInitialHeader(cvsHeaderLine);
		for (Solver solver : solvers) {
			writeHeaderForSolver(solver, cvsHeaderLine);
		}
		csvResultOutput(cvsHeaderLine.toString());
	}

	public void writeHeaderForSolver(Solver solver, StringJoiner cvsHeaderLine) {
		cvsHeaderLine.add("Solver");
		cvsHeaderLine.add("Result for " + solver.getName());
		cvsHeaderLine.add("Inference ms. for " + solver.getName());
		cvsHeaderLine.add("HH:MM:SS.");
		cvsHeaderLine.add("Translation ms. for " + solver.getName());
		cvsHeaderLine.add("HH:MM:SS.");
	}

	// Query line methods
	
	StringJoiner queryCSVLine;
	
	public void initializeQueryLine(Problem problem, String domainSizesOfCurrentModel) {
		queryCSVLine = new StringJoiner(",");
		queryCSVLine.add(problem.name);
		queryCSVLine.add(problemTypeName);
		queryCSVLine.add(domainSizesOfCurrentModel);
		queryCSVLine.add(Integer.toString(numberOfRunsToAverageOver));
	}

	public void addToQueryLine(SolverEvaluationResult solverEvaluationResult) {
		solverEvaluationResult.addToCSVLine(queryCSVLine);
	}

	public void finalizeQueryLine() {
		csvResultOutput(queryCSVLine.toString());
	}

	public void csvResultOutput(String csvLine) {
		csvOut.println(csvLine);
	}
}
