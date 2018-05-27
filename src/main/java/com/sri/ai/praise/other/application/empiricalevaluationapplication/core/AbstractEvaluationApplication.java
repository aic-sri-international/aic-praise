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
package com.sri.ai.praise.other.application.empiricalevaluationapplication.core;

import java.io.FileNotFoundException;
import java.io.IOException;

import com.sri.ai.praise.core.model.impure.modelscontainer.PagedModelContainer;
import com.sri.ai.praise.other.empiricalevaluation.Evaluation;
import com.sri.ai.praise.other.empiricalevaluation.EvaluationConfiguration;

/**
 * An abstract class for Java applications 
 * that offers a static method {@link #run(String[]) taking command line arguments,
 * getting an empirical evaluation {@link EvaluationConfiguration}
 * and the containers of models to be evaluated from them,
 * and executes a {@link Evaluation} based on the {@link EvaluationConfiguration} and models in the container.
 * 
 * @author braz
 */
public abstract class AbstractEvaluationApplication {

	/**
	 * A method making a {@link PagedModelContainer} from evaluation arguments.
	 * @param evaluationConfiguration TODO
	 * 
	 * @return
	 * @throws IOException
	 */
	abstract protected PagedModelContainer getModelsContainer(EvaluationConfigurationFromCommandLineOptions evaluationConfiguration) throws IOException;

	/**
	 * Make a {@link EvaluationConfigurationFromCommandLineOptions} from command-line options.
	 * 
	 * @return the {@link EvaluationConfigurationFromCommandLineOptions} from command-line options.
	 * @throws IOException 
	 */
	protected EvaluationConfigurationFromCommandLineOptions makeEvaluationConfigurationFromCommandLineOptions(String args[]) throws FileNotFoundException, IOException {
		return new EvaluationConfigurationFromCommandLineOptions(args);
	}

	/**
	 * Executes an evaluation according to configuration provided in command line options;
	 * typically, extensions will have a static main method directly invoking this method.
	 * @param args arguments as provided in static main
	 * @throws Exception
	 */
	public void run(String[] args) throws Exception {
		try (EvaluationConfigurationFromCommandLineOptions evaluationConfiguration = makeEvaluationConfigurationFromCommandLineOptions(args)) {
			evaluate(evaluationConfiguration);
		}
	}

	private void evaluate(EvaluationConfigurationFromCommandLineOptions evaluationConfiguration) throws IOException {
		setModelsInEvaluationConfiguration(evaluationConfiguration);
		Evaluation evaluation = new Evaluation(evaluationConfiguration);
		evaluation.evaluate();
	}

	private void setModelsInEvaluationConfiguration(EvaluationConfigurationFromCommandLineOptions evaluationConfiguration) throws IOException {
		PagedModelContainer modelsContainer = getModelsContainer(evaluationConfiguration);
		evaluationConfiguration.setModelsContainer(modelsContainer);
	}
}