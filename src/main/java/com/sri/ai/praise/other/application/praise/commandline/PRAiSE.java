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
package com.sri.ai.praise.other.application.praise.commandline;

import static com.sri.ai.util.Util.toHoursMinutesAndSecondsString;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.grinder.core.solver.Integration;
import com.sri.ai.grinder.core.solver.IntegrationRecording;
import com.sri.ai.praise.core.inference.core.hogm.HOGMQueryError;
import com.sri.ai.praise.core.inference.core.hogm.HOGMQueryResult;
import com.sri.ai.praise.core.inference.core.hogm.HOGMSolver;
import com.sri.ai.praise.core.inference.core.praiseprocess.PRAiSESolver;
import com.sri.ai.praise.core.model.modelscontainer.ModelPage;

/**
 * Command line interface for running {@link PRAiSESolver}.
 *
 * @author oreilly, braz
 *
 */
@Beta
public class PRAiSE {

	public static final String RESULT_PREFIX_DEFINED_AS_CONSTANT_SO_DETECTORS_CAN_REFER_TO_IT = "Result: ";

	PRAiSECommandLineOptions options;

	public static void main(String[] args) {
		PRAiSE praise = new PRAiSE();
		praise.run(args);
	}

	public void run(String[] args) {
		ExpressoConfiguration.setDisplayNumericsExactlyForSymbols(false);
		ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInApproximateRepresentationOfNumericalSymbols(3);
		try {
			parseArguments(args);
			solveAllModels();
		}
		catch (Exception exception) {
			showUnexpectedException(exception);
		}
		finally {
			runAtTheEnd();
		}
	}

	private void parseArguments(String[] args) throws UnsupportedEncodingException, FileNotFoundException, IOException {
		options = new PRAiSECommandLineOptions(args);
	}

	private void solveAllModels() {
		for (ModelPage modelPage : options.modelPages) {
			solveModel(modelPage);
		}
	}

	private void solveModel(ModelPage modelPage) {
		outputModel(modelPage);
		startSummationCounting();
		HOGMSolver queryRunner = new HOGMSolver(modelPage.getModelString(), modelPage.getDefaultQueriesToRun());
		List<HOGMQueryResult> modelPageResults = queryRunner.getResults();
		outputModelResults(queryRunner, modelPageResults);
	}

	private void outputModel(ModelPage modelPage) {
		if (options.showModel) {
			options.out.print  ("FactorNetwork name: ");
			options.out.println(modelPage.getName());
			options.out.println("FactorNetwork     : ");
			options.out.println(modelPage.getModelString());
		}
	}

	private void startSummationCounting() {
		if (options.countSummations) {
			IntegrationRecording.startRecordingIntegrationsOverGroups();
			if (options.showSummations) {
				IntegrationRecording.turnStoringIntegrationsOverGroupsOn();
			}
		}
	}

	private void outputModelResults(HOGMSolver queryRunner, List<HOGMQueryResult> modelPageResults) {
		modelPageResults.forEach(hogModelQueryResult -> output(queryRunner, hogModelQueryResult));
	}

	private void output(HOGMSolver queryRunner, HOGMQueryResult modelQueryResult) {
		options.out.print("Query : ");
		options.out.println(modelQueryResult.getQueryString());
		options.out.print(RESULT_PREFIX_DEFINED_AS_CONSTANT_SO_DETECTORS_CAN_REFER_TO_IT);
		if (modelQueryResult.getResult() != null) {
			options.out.println(queryRunner.simplifyAnswer(modelQueryResult.getResult(), modelQueryResult.getQueryExpression()));
			options.out.print("Took  : ");
			options.out.println(toHoursMinutesAndSecondsString(modelQueryResult.getMillisecondsToCompute()));
			if (options.countSummations) {
				options.out.print("Took  : ");
				options.out.println(modelQueryResult.getNumberOfSummations() + " summations");
				if (options.showSummations) {
					for (Integration summation : modelQueryResult.getSummations()) {
						options.out.println(summation);
					}
				}
			}
		}
		modelQueryResult.getErrors().forEach(error -> outputError(error));
		options.out.println();
	}

	private void outputError(HOGMQueryError error) {
		options.out.println("ERROR = " + error.getErrorMessage());
		if (options.showDebugOutput && error.getThrowable() != null) {
			options.out.println("THROWABLE = ");
			error.getThrowable().printStackTrace(options.out);
		}
	}

	private void showUnexpectedException(Exception exception) {
		if (options != null && options.showDebugOutput) {
			System.err.println("Unexpected error:");
			exception.printStackTrace();
		}
		else {
			System.err.println(exception.getLocalizedMessage());
		}
	}

	private void runAtTheEnd() {
		if (options != null && options.out != null) {
			options.out.flush();
			if (options.out != System.out) {
				options.out.close();
			}
		}
	}
}
