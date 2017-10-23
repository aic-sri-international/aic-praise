package com.sri.ai.praise.application.empiricalevaluation.frompraisemodelsfile;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import com.sri.ai.praise.application.empiricalevaluation.core.EvaluationExecutableCommandLineOptions;

import joptsimple.OptionSpec;

public class CommandLineOptionsWithPRAiSEModelsFile extends EvaluationExecutableCommandLineOptions {
	public OptionSpec<File> praiseModelsFile;
	
	public CommandLineOptionsWithPRAiSEModelsFile(String args[]) throws FileNotFoundException, IOException {
		super(args);
	}

	@Override
	protected void setOptionSpecificationsUsingDefaultEvaluationConfiguration() {
		super.setOptionSpecificationsUsingDefaultEvaluationConfiguration();
		praiseModelsFile = parser.accepts("p", "The PRAiSE Models file (typically recorded by the PRAiSE demo environment) used as input for the evaluations").withRequiredArg().required().ofType(File.class);
	}
}