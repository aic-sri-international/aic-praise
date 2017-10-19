package com.sri.ai.praise.application.empiricalevaluation.options;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import com.sri.ai.praise.empiricalevaluation.core.configuration.DefaultEvaluationConfiguration;
import com.sri.ai.praise.empiricalevaluation.core.configuration.EvaluationConfigurationWithPraiseModelsFile;

import joptsimple.OptionSpec;

public class EvaluationArgumentsWithPRAiSEModelsFileFromCommandLineOptions extends EvaluationArgumentsFromCommandLineOptions {
	OptionSpec<File> praiseModelsFile;
	
	public EvaluationArgumentsWithPRAiSEModelsFileFromCommandLineOptions(String args[]) throws FileNotFoundException, IOException {
		super(args);
	}

	@Override
	protected DefaultEvaluationConfiguration makeInitialEvaluationArgs() {
		return new EvaluationConfigurationWithPraiseModelsFile(); 
	}

	@Override
	protected void setOptionSpecifications() {
		super.setOptionSpecifications();
		praiseModelsFile  = parser.accepts("p", "The PRAiSE Models file used as input for the evaluations").withRequiredArg().required().ofType(File.class);
	}

	@Override
	protected void setEvaluationArgsFromOptions() throws IOException, FileNotFoundException {
		super.setEvaluationArgsFromOptions();
		EvaluationConfigurationWithPraiseModelsFile evaluationArgsWithPraiseModelsFile = (EvaluationConfigurationWithPraiseModelsFile)evaluationArgs;
		evaluationArgsWithPraiseModelsFile.praiseModelsFile = options.valueOf(praiseModelsFile);
		if (!evaluationArgsWithPraiseModelsFile.praiseModelsFile.isFile()) {
			throw new IllegalArgumentException("Input PRAiSE models file does not exist: " + evaluationArgsWithPraiseModelsFile.praiseModelsFile.getAbsolutePath());
		}
	}

}