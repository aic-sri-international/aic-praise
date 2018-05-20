package com.sri.ai.praise.application.empiricalevaluationapplication.fromfile;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import com.sri.ai.praise.application.empiricalevaluationapplication.core.EvaluationConfigurationFromCommandLineOptions;

import joptsimple.OptionSpec;

public class EvaluationConfigurationFromCommandLineOptionsAddingFileOption extends EvaluationConfigurationFromCommandLineOptions {
	
	public OptionSpec<File> file;
	
	public EvaluationConfigurationFromCommandLineOptionsAddingFileOption(String args[]) throws FileNotFoundException, IOException {
		super(args);
	}

	@Override
	protected void setOptionSpecificationsUsingDefaultEvaluationConfiguration() {
		super.setOptionSpecificationsUsingDefaultEvaluationConfiguration();
		file = parser.accepts("p", "The models file (typically recorded by the PRAiSE demo environment with a .praise extension) used as input for the evaluations").withRequiredArg().required().ofType(File.class);
	}
}