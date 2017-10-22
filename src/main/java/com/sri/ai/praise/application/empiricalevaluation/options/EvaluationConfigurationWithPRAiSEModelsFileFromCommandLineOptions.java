package com.sri.ai.praise.application.empiricalevaluation.options;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import com.sri.ai.praise.empiricalevaluation.core.configuration.DefaultConfiguration;
import com.sri.ai.praise.empiricalevaluation.core.configuration.ConfigurationWithPraiseModelsFile;

import joptsimple.OptionSpec;

public class EvaluationConfigurationWithPRAiSEModelsFileFromCommandLineOptions extends EvaluationConfigurationFromCommandLineOptions {
	OptionSpec<File> praiseModelsFile;
	
	public EvaluationConfigurationWithPRAiSEModelsFileFromCommandLineOptions(String args[]) throws FileNotFoundException, IOException {
		super(args);
	}

	@Override
	protected DefaultConfiguration makeInitialConfiguration() {
		return new ConfigurationWithPraiseModelsFile(); 
	}

	@Override
	protected void setOptionSpecifications() {
		super.setOptionSpecifications();
		praiseModelsFile  = parser.accepts("p", "The PRAiSE Models file (typically recorded by the PRAiSE demo environment) used as input for the evaluations").withRequiredArg().required().ofType(File.class);
	}

	@Override
	protected void setEvaluationArgsFromOptions() throws IOException, FileNotFoundException {
		super.setEvaluationArgsFromOptions();
		ConfigurationWithPraiseModelsFile evaluationArgsWithPraiseModelsFile = (ConfigurationWithPraiseModelsFile)configuration;
		evaluationArgsWithPraiseModelsFile.praiseModelsFile = options.valueOf(praiseModelsFile);
		if (!evaluationArgsWithPraiseModelsFile.praiseModelsFile.isFile()) {
			throw new IllegalArgumentException("Input PRAiSE models file does not exist: " + evaluationArgsWithPraiseModelsFile.praiseModelsFile.getAbsolutePath());
		}
	}
}