package com.sri.ai.praise.application.empiricalevaluation.options;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;

import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;

import com.sri.ai.praise.empiricalevaluation.core.configuration.DefaultEvaluationConfiguration;
import com.sri.ai.praise.probabilisticsolver.core.pimt.PIMTSolverEvaluator;

// TODO - consider using commons-configuration to evaluation input file
// reading, i.e:
// https://commons.apache.org/proper/commons-configuration/userguide_v1.10/user_guide.html
public class EvaluationArgumentsFromCommandLineOptions {

	DefaultEvaluationConfiguration evaluationArgs;

	OptionSet options;

	OptionParser parser;
	OptionSpec<String> solverImplementationClasses;
	OptionSpec<File> notificationFile;
	OptionSpec<File> resultFile;
	OptionSpec<Integer> totalCPURuntimeLimitSecondsPerSolveAttempt;
	OptionSpec<Integer> totalMemoryLimitInMegabytesPerSolveAttempt;
	OptionSpec<Integer> numberRunsToAverageOver;
	OptionSpec<File> workingDirectory;

	public EvaluationArgumentsFromCommandLineOptions(String args[]) throws FileNotFoundException, IOException {
		evaluationArgs = makeInitialEvaluationArgs();
		setOptionSpecifications();
		options = parser.parse(args);
		setEvaluationArgsFromOptions();
	}

	protected DefaultEvaluationConfiguration makeInitialEvaluationArgs() {
		return new DefaultEvaluationConfiguration();
	}

	protected void setOptionSpecifications() {
		parser = new OptionParser();
		// Optional
		solverImplementationClasses = parser.accepts("s", "Solver implementation class name.").withRequiredArg()
				.ofType(String.class);
		//
		notificationFile = parser.accepts("n", "Notification output file name (default to stdout).")
				.withRequiredArg().ofType(File.class);
		resultFile = parser.accepts("r", "Result output file name (defaults to stdout).").withRequiredArg()
				.ofType(File.class);
		//
		totalCPURuntimeLimitSecondsPerSolveAttempt = parser
				.accepts("c",
						"Total CPU runtime limit seconds per solver attempt (defaults to "
								+ evaluationArgs.totalCPURuntimeLimitSecondsPerSolveAttempt + ").")
				.withRequiredArg().ofType(Integer.class);
		totalMemoryLimitInMegabytesPerSolveAttempt = parser
				.accepts("m",
						"Total memory limit in MB per solver attempt (defaults to "
								+ evaluationArgs.totalMemoryLimitInMegabytesPerSolveAttempt + ").")
				.withRequiredArg().ofType(Integer.class);
		numberRunsToAverageOver = parser
				.accepts("a",
						"Number of runs to average each result over (defaults to "
								+ evaluationArgs.numberOfRunsToAverageOver + ").")
				.withRequiredArg().ofType(Integer.class);
		parser.accepts("t",
				"Translate models always, instead of caching them between runs (default behavior is caching)");

		// Required
		workingDirectory = parser
				.accepts("w", "Solver Working Directory (temp directories and files will be created under here)")
				.withRequiredArg().required().ofType(File.class);

		//
		parser.accepts("help", "For help on command line arguments").forHelp();
	}
	
	protected void setEvaluationArgsFromOptions()
			throws IOException, FileNotFoundException {
		if (options.has("help")) {
			parser.printHelpOn(System.out);
			System.exit(0);
		}

		//
		// Handle optional args
		if (options.has(solverImplementationClasses)) {
			evaluationArgs.solverImplementationClassNames
					.addAll(options.valuesOf(solverImplementationClasses));
		} else {
			evaluationArgs.solverImplementationClassNames.add(PIMTSolverEvaluator.class.getName());
		}
		if (options.has(notificationFile)) {
			evaluationArgs.notificationOut = new PrintStream(options.valueOf(notificationFile));
		}
		if (options.has(resultFile)) {
			evaluationArgs.resultOut = new PrintStream(options.valueOf(resultFile));
		}
		if (options.has(totalCPURuntimeLimitSecondsPerSolveAttempt)) {
			evaluationArgs.totalCPURuntimeLimitSecondsPerSolveAttempt = options
					.valueOf(totalCPURuntimeLimitSecondsPerSolveAttempt);
		}
		if (options.has(totalMemoryLimitInMegabytesPerSolveAttempt)) {
			evaluationArgs.totalMemoryLimitInMegabytesPerSolveAttempt = options
					.valueOf(totalMemoryLimitInMegabytesPerSolveAttempt);
		}
		if (options.has(numberRunsToAverageOver)) {
			evaluationArgs.numberOfRunsToAverageOver = options.valueOf(numberRunsToAverageOver);
		}
		if (options.has("t")) {
			evaluationArgs.translateAlways = true;
		}

		evaluationArgs.workingDirectory = options.valueOf(workingDirectory);
		if (!evaluationArgs.workingDirectory.isDirectory()) {
			throw new IllegalArgumentException("Working directory does not exist: " + evaluationArgs.workingDirectory);
		}
	}

	public DefaultEvaluationConfiguration getEvaluationArgs() {
		return evaluationArgs;
	}
}