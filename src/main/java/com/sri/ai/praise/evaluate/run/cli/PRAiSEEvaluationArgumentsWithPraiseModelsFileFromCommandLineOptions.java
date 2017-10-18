package com.sri.ai.praise.evaluate.run.cli;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import joptsimple.OptionSpec;

class PRAiSEEvaluationArgumentsWithPraiseModelsFileFromCommandLineOptions extends PRAiSEEvaluationArgumentsFromCommandLineOptions {
	OptionSpec<File> praiseModelsFile;
	
	public PRAiSEEvaluationArgumentsWithPraiseModelsFileFromCommandLineOptions(String args[]) throws FileNotFoundException, IOException {
		super(args);
	}

	@Override
	protected DefaultPRAiSEEvaluationArguments makeInitialEvaluationArgs() {
		return new EvaluationArgsWithPraiseModelsFile(); 
	}

	@Override
	protected void setOptionSpecifications() {
		super.setOptionSpecifications();
		praiseModelsFile  = parser.accepts("p", "The PRAiSE Models file used as input for the evaluations").withRequiredArg().required().ofType(File.class);
	}

	@Override
	protected void setEvaluationArgsFromOptions() throws IOException, FileNotFoundException {
		super.setEvaluationArgsFromOptions();
		EvaluationArgsWithPraiseModelsFile evaluationArgsWithPraiseModelsFile = (EvaluationArgsWithPraiseModelsFile)evaluationArgs;
		evaluationArgsWithPraiseModelsFile.praiseModelsFile = options.valueOf(praiseModelsFile);
		if (!evaluationArgsWithPraiseModelsFile.praiseModelsFile.isFile()) {
			throw new IllegalArgumentException("Input PRAiSE models file does not exist: " + evaluationArgsWithPraiseModelsFile.praiseModelsFile.getAbsolutePath());
		}
	}

}