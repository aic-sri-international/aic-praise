package com.sri.ai.praise.evaluate.run.cli;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;

import com.sri.ai.praise.evaluate.run.Evaluation;
import com.sri.ai.praise.evaluate.solver.SolverEvaluatorConfiguration;
import com.sri.ai.praise.evaluate.solver.impl.sgsolver.SGSolverEvaluator;
import com.sri.ai.praise.evaluate.solver.impl.vec.VECSolverEvaluator;
import com.sri.ai.praise.model.common.io.PagedModelContainer;

/**
 * Provides a static method for outputting evaluation results for given solvers, models and configurations.
 * @author braz
 *
 */
public abstract class AbstractEvaluateCLI {

	protected static class EvaluationArgs implements AutoCloseable {
		// Optional
		PrintStream  notificationOut = System.out; // -n
		PrintStream  resultOut       = System.out; // -r 
		//
		int totalCPURuntimeLimitSecondsPerSolveAttempt = 600;  // -c
		int totalMemoryLimitInMegabytesPerSolveAttempt = 2048; // -m
		int numberRunsToAverageOver                    = 10;   // -a
		//
		boolean translateAlways = false; // -t

		// Required
		File workingDirectory; // -w

		@Override
		public void close() throws IOException {
			notificationOut.flush();
			resultOut.flush();
			// Only close if not System.out
			if (notificationOut != System.out) {				
				notificationOut.close();
			}
			if (resultOut != System.out) {
				resultOut.close();
			}
		}
	}

	protected static class OptionSpecs {
		OptionParser parser;
		OptionSpec<File> notificationFile;
		OptionSpec<File> resultFile;
		OptionSpec<Integer> totalCPURuntimeLimitSecondsPerSolveAttempt;
		OptionSpec<Integer> totalMemoryLimitInMegabytesPerSolveAttempt;
		OptionSpec<Integer> numberRunsToAverageOver;
		OptionSpec<File> workingDirectory;
		
		public OptionSpecs(EvaluationArgs evaluationArgs) {
			parser = new OptionParser();
			// Optional
			notificationFile = parser.accepts("n", "Notification output file name (default to stdout).").withRequiredArg().ofType(File.class);
			resultFile       = parser.accepts("r", "Result output file name (defaults to stdout).").withRequiredArg().ofType(File.class);
			//
			totalCPURuntimeLimitSecondsPerSolveAttempt = parser.accepts("c", "Total CPU runtime limit seconds per solver attempt (defaults to "+evaluationArgs.totalCPURuntimeLimitSecondsPerSolveAttempt+").").withRequiredArg().ofType(Integer.class);
			totalMemoryLimitInMegabytesPerSolveAttempt = parser.accepts("m", "Total memory limit in MB per solver attempt (defaults to "+evaluationArgs.totalMemoryLimitInMegabytesPerSolveAttempt+").").withRequiredArg().ofType(Integer.class);
			numberRunsToAverageOver                    = parser.accepts("a", "Number of runs to average each result over (defaults to "+evaluationArgs.numberRunsToAverageOver+").").withRequiredArg().ofType(Integer.class);
			parser.accepts("t", "Translate models always, instead of caching them between runs (default behavior is caching)");
			
			// Required
			workingDirectory  = parser.accepts("w", "Solver Working Directory (temp directories and files will be created under here)").withRequiredArg().required().ofType(File.class);
			
			//
			parser.accepts("help", "For help on command line arguments").forHelp();
		}
	}
	
	/**
	 * Returns evaluation args object to be used by (possibly extending) class.
	 * @return
	 */
	protected EvaluationArgs makeEvaluationArgs() {
		return new EvaluationArgs();
	}
	
	/**
	 * Returns options specifications.
	 * @return
	 */
	protected OptionSpecs makeOptionSpecs(EvaluationArgs evaluationArgs) {
		return new OptionSpecs(evaluationArgs);
	}

	/**
	 * Validates arguments
	 * @param evaluationArgs
	 * @param optionSpecs
	 * @param options
	 * @throws IOException
	 * @throws FileNotFoundException
	 */
	protected void validateArguments(EvaluationArgs evaluationArgs, OptionSpecs optionSpecs, OptionSet options) throws IOException, FileNotFoundException {
		if (options.has("help")) {
			optionSpecs.parser.printHelpOn(System.out);
			System.exit(0);
		}
		
		// 
		// Handle optional args
		if (options.has(optionSpecs.notificationFile)) {
			evaluationArgs.notificationOut = new PrintStream(options.valueOf(optionSpecs.notificationFile));
		}
		if (options.has(optionSpecs.resultFile)) {
			evaluationArgs.resultOut = new PrintStream(options.valueOf(optionSpecs.resultFile));
		}
		if (options.has(optionSpecs.totalCPURuntimeLimitSecondsPerSolveAttempt)) {
			evaluationArgs.totalCPURuntimeLimitSecondsPerSolveAttempt = options.valueOf(optionSpecs.totalCPURuntimeLimitSecondsPerSolveAttempt);
		}
		if (options.has(optionSpecs.totalMemoryLimitInMegabytesPerSolveAttempt)) {
			evaluationArgs.totalMemoryLimitInMegabytesPerSolveAttempt = options.valueOf(optionSpecs.totalMemoryLimitInMegabytesPerSolveAttempt);
		}
		if (options.has(optionSpecs.numberRunsToAverageOver)) {
			evaluationArgs.numberRunsToAverageOver = options.valueOf(optionSpecs.numberRunsToAverageOver);
		}
		if (options.has("t")) {
			evaluationArgs.translateAlways = true;
		}
		
		evaluationArgs.workingDirectory = options.valueOf(optionSpecs.workingDirectory);
		if (!evaluationArgs.workingDirectory.isDirectory()) {
			throw new IllegalArgumentException("Working directory does not exist: "+evaluationArgs.workingDirectory);
		}
	}

	/**
	 * Evaluates given solvers on given models according to given evaluation configurations and streams for notifications and results.
	 * @param configuration
	 * @param modelsContainer
	 * @param solverConfigurations
	 * @param notificationOut
	 * @param resultOut
	 */
	public static void evaluate(Evaluation.Configuration configuration, PagedModelContainer modelsContainer, List<SolverEvaluatorConfiguration> solverConfigurations, PrintStream notificationOut, PrintStream resultOut) {
		
		Evaluation evaluation = new Evaluation();
		evaluation.evaluate(configuration, modelsContainer, solverConfigurations, new Evaluation.Listener() {
			@Override
			public void notification(String notification) {
				notificationOut.println(notification);
			}
			
			@Override
			public void notificationException(Exception ex) {
				ex.printStackTrace(notificationOut);
			}
			
			@Override
			public void csvResultOutput(String csvLine) {
				resultOut.println(csvLine);
			}
		});
	}

	public void run(String[] args) throws Exception {
		try (EvaluationArgs evaluationArgs = getArgs(args)) {			
			PagedModelContainer                modelsContainer      = makeModelsContainer(evaluationArgs);
			Evaluation.Configuration           configuration        = new Evaluation.Configuration(Evaluation.Type.PR, evaluationArgs.workingDirectory, evaluationArgs.numberRunsToAverageOver);
			List<SolverEvaluatorConfiguration> solverConfigurations = Arrays.asList(			
					new SolverEvaluatorConfiguration("SGSolver", SGSolverEvaluator.class.getName(), 
							evaluationArgs.totalCPURuntimeLimitSecondsPerSolveAttempt, 
							evaluationArgs.totalMemoryLimitInMegabytesPerSolveAttempt,
							!evaluationArgs.translateAlways, Collections.emptyMap())
					,
					new SolverEvaluatorConfiguration("VEC", VECSolverEvaluator.class.getName(), 
							evaluationArgs.totalCPURuntimeLimitSecondsPerSolveAttempt, 
							evaluationArgs.totalMemoryLimitInMegabytesPerSolveAttempt,
							!evaluationArgs.translateAlways, Collections.emptyMap())
					);
			
			PrintStream notificationOut = evaluationArgs.notificationOut;
			PrintStream resultOut = evaluationArgs.resultOut;
	
			evaluate(configuration, modelsContainer, solverConfigurations, notificationOut, resultOut);
		}
	}

	/**
	 * A method making a {@link PagedModelContainer} from evaluation arguments.
	 * @param evaluationArgs
	 * @return
	 * @throws IOException
	 */
	abstract protected PagedModelContainer makeModelsContainer(EvaluationArgs evaluationArgs) throws IOException;

	private EvaluationArgs getArgs(String[] args) throws UnsupportedEncodingException, FileNotFoundException, IOException {
		EvaluationArgs evaluationArgs = makeEvaluationArgs();
		OptionSpecs optionSpecs = makeOptionSpecs(evaluationArgs);
		
		OptionSet options = optionSpecs.parser.parse(args);
		
		validateArguments(evaluationArgs, optionSpecs, options);
		
		return evaluationArgs;
	}
}