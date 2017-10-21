package com.sri.ai.praise.application.empiricalevaluation.options;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.util.LinkedList;
import java.util.List;

import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;

import com.sri.ai.praise.empiricalevaluation.core.configuration.DefaultSetOfSolversEvaluationConfiguration;
import com.sri.ai.praise.probabilisticsolver.core.pimt.PIMTSolver;

// TODO - consider using commons-configuration to evaluation input file
// reading, i.e:
// https://commons.apache.org/proper/commons-configuration/userguide_v1.10/user_guide.html
public class EvaluationConfigurationFromCommandLineOptions {

	DefaultSetOfSolversEvaluationConfiguration evaluationArgs;

	OptionSet options;

	OptionParser parser;
	OptionSpec<String> solverImplementationClasses;
	OptionSpec<File> notificationFile;
	OptionSpec<File> resultFile;
	OptionSpec<Integer> totalCPURuntimeLimitSecondsPerSolveAttempt;
	OptionSpec<Integer> totalMemoryLimitInMegabytesPerSolveAttempt;
	OptionSpec<Integer> numberRunsToAverageOver;
	OptionSpec<File> workingDirectory;

	public EvaluationConfigurationFromCommandLineOptions(String args[]) throws FileNotFoundException, IOException {
		evaluationArgs = makeInitialEvaluationArgs();
		setOptionSpecifications();
		options = parser.parse(args);
		setEvaluationArgsFromOptions();
	}

	protected DefaultSetOfSolversEvaluationConfiguration makeInitialEvaluationArgs() {
		return new DefaultSetOfSolversEvaluationConfiguration();
	}

	protected void setOptionSpecifications() {
		
		parser = new OptionParser();

		solverImplementationClasses = parser.accepts("s", "Solver implementation class name.").withRequiredArg()
				.ofType(String.class);

		notificationFile = parser.accepts("n", "Notification output file name (default to stdout).")
				.withRequiredArg().ofType(File.class);
		resultFile = parser.accepts("r", "Result output file name (defaults to stdout).").withRequiredArg()
				.ofType(File.class);

		totalCPURuntimeLimitSecondsPerSolveAttempt = parser
				.accepts("c",
						"Total CPU runtime limit seconds per solver attempt (defaults to "
								+ evaluationArgs.getTotalCPURuntimeLimitSecondsPerSolveAttempt() + ").")
				.withRequiredArg().ofType(Integer.class);
		totalMemoryLimitInMegabytesPerSolveAttempt = parser
				.accepts("m",
						"Total memory limit in MB per solver attempt (defaults to "
								+ evaluationArgs.getTotalMemoryLimitInMegabytesPerSolveAttempt() + ").")
				.withRequiredArg().ofType(Integer.class);
		numberRunsToAverageOver = parser
				.accepts("a",
						"Number of runs to average each result over (defaults to "
								+ evaluationArgs.getNumberOfRunsToAverageOver() + ").")
				.withRequiredArg().ofType(Integer.class);
		parser.accepts("t",
				"Translate models always, instead of caching them between runs (default behavior is caching)");

		workingDirectory = parser
				.accepts("w", "Solver Working Directory (temp directories and files will be created under here)")
				.withRequiredArg().required().ofType(File.class);

		parser.accepts("help", "For help on command line arguments").forHelp();
	}
	
	protected void setEvaluationArgsFromOptions() throws IOException, FileNotFoundException {
		
		if (options.has("help")) {
			parser.printHelpOn(System.out);
			System.exit(0);
		}

		
		List<String> currentSolverImplementationClasses = new LinkedList<>(evaluationArgs.getSolverImplementationClassNames());
		if (options.has(solverImplementationClasses)) {
			currentSolverImplementationClasses.addAll(options.valuesOf(solverImplementationClasses));
		} else {
			currentSolverImplementationClasses.add(PIMTSolver.class.getName());
		}
		evaluationArgs.setSolverImplementationClassNames(currentSolverImplementationClasses);
		
		
		if (options.has(notificationFile)) {
			evaluationArgs.setNotificationOut(new PrintStream(options.valueOf(notificationFile)));
		}
		
		if (options.has(resultFile)) {
			evaluationArgs.setResultOut(new PrintStream(options.valueOf(resultFile)));
		}
		
		if (options.has(totalCPURuntimeLimitSecondsPerSolveAttempt)) {
			evaluationArgs.setTotalCPURuntimeLimitSecondsPerSolveAttempt(options.valueOf(totalCPURuntimeLimitSecondsPerSolveAttempt));
		}
		
		if (options.has(totalMemoryLimitInMegabytesPerSolveAttempt)) {
			evaluationArgs.setTotalMemoryLimitInMegabytesPerSolveAttempt(options.valueOf(totalMemoryLimitInMegabytesPerSolveAttempt));
		}
		
		if (options.has(numberRunsToAverageOver)) {
			evaluationArgs.setNumberOfRunsToAverageOver(options.valueOf(numberRunsToAverageOver));
		}
		
		if (options.has("t")) {
			evaluationArgs.setDoesNotCacheTranslations(true);
		}

		File workingDirectoryFile = options.valueOf(workingDirectory);
		if (workingDirectoryFile.isDirectory()) {
			evaluationArgs.setWorkingDirectory(workingDirectoryFile);
		}
		else {
			throw new IllegalArgumentException("Working directory does not exist: " + workingDirectoryFile);
		}
	}

	public DefaultSetOfSolversEvaluationConfiguration getEvaluationArgs() {
		return evaluationArgs;
	}
}