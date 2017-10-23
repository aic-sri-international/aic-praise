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
package com.sri.ai.praise.application.empiricalevaluation.core;

import java.io.FileNotFoundException;
import java.io.IOException;

import com.sri.ai.praise.empiricalevaluation.Evaluation;
import com.sri.ai.praise.empiricalevaluation.EvaluationConfiguration;
import com.sri.ai.praise.model.common.io.PagedModelContainer;

/**
 * Provides a static method for outputting evaluation results for given solvers,
 * models and configurations.
 * 
 * @author braz
 *
 */
public abstract class AbstractEvaluationExecutable {

	protected EvaluationExecutableCommandLineOptions commandLineOptions;
	
	/**
	 * A method making a {@link PagedModelContainer} from evaluation arguments.
	 * 
	 * @return
	 * @throws IOException
	 */
	abstract protected PagedModelContainer makeModelsContainerFromCommandLineOptions() throws IOException;

	/**
	 * Returns options specifications.
	 * 
	 * @return
	 * @throws IOException 
	 * @throws FileNotFoundException 
	 */
	protected EvaluationExecutableCommandLineOptions makeCommandLineOptions(String args[]) throws FileNotFoundException, IOException {
		return new EvaluationExecutableCommandLineOptions(args);
	}

	public void run(String[] args) throws Exception {
		commandLineOptions = makeCommandLineOptions(args);
		try (EvaluationConfiguration evaluationConfiguration = commandLineOptions.getEvaluationConfiguration()) {
			evaluate(evaluationConfiguration);
		}
	}

	private void evaluate(EvaluationConfiguration evaluationConfiguration) throws IOException {
		setModelsInEvaluationConfiguration(evaluationConfiguration);
		Evaluation evaluation = new Evaluation(evaluationConfiguration);
		evaluation.evaluate();
	}

	private void setModelsInEvaluationConfiguration(EvaluationConfiguration evaluationConfiguration) throws IOException {
		PagedModelContainer modelsContainer = makeModelsContainerFromCommandLineOptions();
		evaluationConfiguration.setModelsContainer(modelsContainer);
	}
}