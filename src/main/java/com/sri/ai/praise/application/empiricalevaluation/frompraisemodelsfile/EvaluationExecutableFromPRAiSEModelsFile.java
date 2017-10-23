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
package com.sri.ai.praise.application.empiricalevaluation.frompraisemodelsfile;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import joptsimple.OptionSet;
import joptsimple.OptionSpec;

import com.sri.ai.praise.application.empiricalevaluation.core.AbstractEvaluationExecutable;
import com.sri.ai.praise.application.empiricalevaluation.core.EvaluationExecutableCommandLineOptions;
import com.sri.ai.praise.model.common.io.PagedModelContainer;


/**
 * Command line interface for running evaluations on a collection of models in a praise file.
 * 
 * @author oreilly
 *
 */
public class EvaluationExecutableFromPRAiSEModelsFile extends AbstractEvaluationExecutable {	
	
	@Override
	protected EvaluationExecutableCommandLineOptions makeCommandLineOptions(String args[]) throws FileNotFoundException, IOException {
		return new CommandLineOptionsWithPRAiSEModelsFile(args);
	}
	
	protected PagedModelContainer makeModelsContainerFromCommandLineOptions() throws IOException {
		CommandLineOptionsWithPRAiSEModelsFile commandLineOptionsWithPRAiSEModelsFile = (CommandLineOptionsWithPRAiSEModelsFile) commandLineOptions;
		OptionSet optionSet = commandLineOptionsWithPRAiSEModelsFile.optionSet;
		OptionSpec<File> praiseModelsFileOptionSpec = commandLineOptionsWithPRAiSEModelsFile.praiseModelsFile;
		File praiseModelsFile = optionSet.valueOf(praiseModelsFileOptionSpec);
		if (!praiseModelsFile.isFile()) {
			throw new IllegalArgumentException("Input PRAiSE models file does not exist: " + praiseModelsFile.getAbsolutePath());
		}
		PagedModelContainer result = new PagedModelContainer(praiseModelsFile.getName(), praiseModelsFile.toURI());
		return result;
	}

	public static void main(String[] args) throws Exception {
		EvaluationExecutableFromPRAiSEModelsFile evaluator = new EvaluationExecutableFromPRAiSEModelsFile();
		evaluator.run(args);
	}
}