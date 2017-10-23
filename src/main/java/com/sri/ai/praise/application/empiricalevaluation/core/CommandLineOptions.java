package com.sri.ai.praise.application.empiricalevaluation.core;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.util.LinkedList;
import java.util.List;

import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;

import com.sri.ai.praise.empiricalevaluation.Configuration;
import com.sri.ai.praise.probabilisticsolver.core.pimt.PIMTSolver;

// TODO - consider using commons-configuration to evaluation input file
// reading, i.e:
// https://commons.apache.org/proper/commons-configuration/userguide_v1.10/user_guide.html
public class CommandLineOptions {

	public Configuration evaluationConfiguration;

	public OptionSet optionSet;

	public OptionParser parser;
	public OptionSpec<String> solverImplementationClasses;
	public OptionSpec<File> notificationFile;
	public OptionSpec<File> resultFile;
	public OptionSpec<Integer> totalCPURuntimeLimitSecondsPerSolveAttempt;
	public OptionSpec<Integer> totalMemoryLimitInMegabytesPerSolveAttempt;
	public OptionSpec<Integer> numberRunsToAverageOver;
	public OptionSpec<File> workingDirectory;

	public CommandLineOptions(String args[]) throws FileNotFoundException, IOException {
		evaluationConfiguration = new Configuration();
		setOptionsSetUsingDefaultEvaluationConfiguration(args);
		overrideEvaluationConfigurationsFromOptionSet();
	}

	private void setOptionsSetUsingDefaultEvaluationConfiguration(String[] args) {
		setOptionSpecificationsUsingDefaultEvaluationConfiguration();
		optionSet = parser.parse(args);
	}

	protected void setOptionSpecificationsUsingDefaultEvaluationConfiguration() {
		
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
								+ evaluationConfiguration.getTotalCPURuntimeLimitSecondsPerSolveAttempt() + ").")
				.withRequiredArg().ofType(Integer.class);
		totalMemoryLimitInMegabytesPerSolveAttempt = parser
				.accepts("m",
						"Total memory limit in MB per solver attempt (defaults to "
								+ evaluationConfiguration.getTotalMemoryLimitInMegabytesPerSolveAttempt() + ").")
				.withRequiredArg().ofType(Integer.class);
		numberRunsToAverageOver = parser
				.accepts("a",
						"Number of runs to average each result over (defaults to "
								+ evaluationConfiguration.getNumberOfRunsToAverageOver() + ").")
				.withRequiredArg().ofType(Integer.class);
		parser.accepts("t",
				"Translate models always, instead of caching them between runs (default behavior is caching)");

		workingDirectory = parser
				.accepts("w", "Solver Working Directory (temp directories and files will be created under here)")
				.withRequiredArg().required().ofType(File.class);

		parser.accepts("help", "For help on command line arguments").forHelp();
	}
	
	protected void overrideEvaluationConfigurationsFromOptionSet() throws IOException, FileNotFoundException {
		
		if (optionSet.has("help")) {
			parser.printHelpOn(System.out);
			System.exit(0);
		}

		
		List<String> currentSolverImplementationClasses = new LinkedList<>(evaluationConfiguration.getSolverImplementationClassNames());
		if (optionSet.has(solverImplementationClasses)) {
			currentSolverImplementationClasses.addAll(optionSet.valuesOf(solverImplementationClasses));
		} else {
			currentSolverImplementationClasses.add(PIMTSolver.class.getName());
		}
		evaluationConfiguration.setSolverImplementationClassNames(currentSolverImplementationClasses);
		
		
		if (optionSet.has(notificationFile)) {
			evaluationConfiguration.setNotificationOut(new PrintStream(optionSet.valueOf(notificationFile)));
		}
		
		if (optionSet.has(resultFile)) {
			evaluationConfiguration.setCSVOut(new PrintStream(optionSet.valueOf(resultFile)));
		}
		
		if (optionSet.has(totalCPURuntimeLimitSecondsPerSolveAttempt)) {
			evaluationConfiguration.setTotalCPURuntimeLimitSecondsPerSolveAttempt(optionSet.valueOf(totalCPURuntimeLimitSecondsPerSolveAttempt));
		}
		
		if (optionSet.has(totalMemoryLimitInMegabytesPerSolveAttempt)) {
			evaluationConfiguration.setTotalMemoryLimitInMegabytesPerSolveAttempt(optionSet.valueOf(totalMemoryLimitInMegabytesPerSolveAttempt));
		}
		
		if (optionSet.has(numberRunsToAverageOver)) {
			evaluationConfiguration.setNumberOfRunsToAverageOver(optionSet.valueOf(numberRunsToAverageOver));
		}
		
		if (optionSet.has("t")) {
			evaluationConfiguration.setDoesNotCacheTranslations(true);
		}

		File workingDirectoryFile = optionSet.valueOf(workingDirectory);
		if (workingDirectoryFile.isDirectory()) {
			evaluationConfiguration.setWorkingDirectory(workingDirectoryFile);
		}
		else {
			throw new IllegalArgumentException("Working directory does not exist: " + workingDirectoryFile);
		}
	}

	public Configuration getEvaluationConfiguration() {
		return evaluationConfiguration;
	}
}