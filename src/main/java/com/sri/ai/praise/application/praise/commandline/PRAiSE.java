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
package com.sri.ai.praise.application.praise.commandline;

import static com.sri.ai.util.Util.toHoursMinutesAndSecondsString;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.inference.HOGMQueryError;
import com.sri.ai.praise.inference.HOGMQueryResult;
import com.sri.ai.praise.inference.HOGMQueryRunner;
import com.sri.ai.praise.model.common.io.ModelPage;
import com.sri.ai.praise.probabilisticsolver.core.praise.PRAiSESolver;

/**
 * Command line interface for running {@link PRAiSESolver}.
 * 
 * @author oreilly, braz
 *
 */
@Beta
public class PRAiSE {

	PRAiSECommandLineOptions options;
	
	public static void main(String[] args) {
		PRAiSE praise = new PRAiSE();
		praise.run(args);
	}

	public void run(String[] args) {
		try {
			parseArgumentsAndSolve(args);
		}
		catch (Exception exception) {
			showUnexpectedException(exception);
		}
		finally {
			runAtTheEnd();
		}
	}

	private void parseArgumentsAndSolve(String[] args) throws UnsupportedEncodingException, FileNotFoundException, IOException {
		options = new PRAiSECommandLineOptions(args);
		solve();
	}

	private void solve() {
		for (ModelPage modelPage : options.modelPages) {
			solveModel(modelPage);
		}
	}

	private void solveModel(ModelPage modelPage) {
		outputModel(modelPage);
		HOGMQueryRunner queryRunner = new  HOGMQueryRunner(modelPage.getModelString(), modelPage.getDefaultQueriesToRun());
		List<HOGMQueryResult> modelPageResults = queryRunner.query();
		outputModelResults(queryRunner, modelPageResults);
	}

	private void outputModelResults(HOGMQueryRunner queryRunner, List<HOGMQueryResult> modelPageResults) {
		modelPageResults.forEach(hogModelQueryResult -> output(queryRunner, hogModelQueryResult));
	}

	private void outputModel(ModelPage modelPage) {
		options.out.print("MODEL NAME = ");
		options.out.println(modelPage.getName());
		options.out.println("MODEL      = ");
		options.out.println(modelPage.getModelString());
	}

	private void output(HOGMQueryRunner queryRunner, HOGMQueryResult modelPageResult) {
		options.out.print("QUERY      = ");
		options.out.println(modelPageResult.getQueryString());
		options.out.print("RESULT     = ");
		options.out.println(queryRunner.simplifyAnswer(modelPageResult.getResult(), modelPageResult.getQueryExpression()));
		options.out.print("TOOK       = ");
		options.out.println(toHoursMinutesAndSecondsString(modelPageResult.getMillisecondsToCompute()) + "\n");
		modelPageResult.getErrors().forEach(error -> outputError(error));
	}

	private void outputError(HOGMQueryError error) {
		options.out.println("ERROR = " + error.getErrorMessage());
		if (error.getThrowable() != null) {
			options.out.println("THROWABLE = ");
			error.getThrowable().printStackTrace(options.out);
		}
	}

	private void showUnexpectedException(Exception exception) {
		System.err.println("Unexpected error:");
		exception.printStackTrace();
	}

	private void runAtTheEnd() {
		options.out.flush();
		if (options.out != System.out) {				
			options.out.close();
		}
	}
}