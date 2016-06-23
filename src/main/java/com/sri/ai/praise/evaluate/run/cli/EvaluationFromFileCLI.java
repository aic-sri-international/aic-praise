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

import joptsimple.OptionSet;
import joptsimple.OptionSpec;

import com.sri.ai.praise.model.common.io.PagedModelContainer;


/**
 * Command line interface for running evaluations on a collection of models in a praise file.
 * 
 * @author oreilly
 *
 */
public class EvaluationFromFileCLI extends AbstractEvaluateCLI {	
	protected static class EvaluationArgsWithPraiseModelsFile extends EvaluationArgs {
		// Required
		File praiseModelsFile; // -p
	}

	protected static class OptionSpecsWithPraiseModelsFile extends OptionSpecs {
		OptionSpec<File> praiseModelsFile;
		
		public OptionSpecsWithPraiseModelsFile(EvaluationArgs evaluationArgs) {
			super(evaluationArgs);
			praiseModelsFile  = parser.accepts("p", "The PRAiSE Models file used as input for the evaluations").withRequiredArg().required().ofType(File.class);
		}
	}

	protected EvaluationArgs makeEvaluationArgs() {
		return new EvaluationArgsWithPraiseModelsFile(); 
	}
	
	protected OptionSpecs makeOptionSpecs(EvaluationArgs evaluationArgs) {
		return new OptionSpecsWithPraiseModelsFile(evaluationArgs);
	}
	
	protected void validateArguments(EvaluationArgs evaluationArgs, OptionSpecs optionSpecs, OptionSet options) throws IOException, FileNotFoundException {
		super.validateArguments(evaluationArgs, optionSpecs, options);
		OptionSpecsWithPraiseModelsFile optionSpecsWithPraiseModelsFile = (OptionSpecsWithPraiseModelsFile) optionSpecs;
		EvaluationArgsWithPraiseModelsFile evaluationArgsWithPraiseModelsFile = (EvaluationArgsWithPraiseModelsFile)evaluationArgs;
		evaluationArgsWithPraiseModelsFile.praiseModelsFile = options.valueOf(optionSpecsWithPraiseModelsFile.praiseModelsFile);
		if (!evaluationArgsWithPraiseModelsFile.praiseModelsFile.isFile()) {
			throw new IllegalArgumentException("Input PRAiSE models file does not exist: " + evaluationArgsWithPraiseModelsFile.praiseModelsFile.getAbsolutePath());
		}
	}

	protected PagedModelContainer makeModelsContainer(EvaluationArgs evaluationArgs) throws IOException {
		EvaluationArgsWithPraiseModelsFile evaluationArgsWithPraiseModelsFile = (EvaluationArgsWithPraiseModelsFile)evaluationArgs;
		return new PagedModelContainer(evaluationArgsWithPraiseModelsFile.praiseModelsFile.getName(), evaluationArgsWithPraiseModelsFile.praiseModelsFile.toURI());
	}

	public static void main(String[] args) throws Exception {
		EvaluationFromFileCLI evaluator = new EvaluationFromFileCLI();
		evaluator.run(args);
	}
}