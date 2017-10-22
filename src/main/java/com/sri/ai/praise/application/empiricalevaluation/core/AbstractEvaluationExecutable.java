package com.sri.ai.praise.application.empiricalevaluation.core;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.application.empiricalevaluation.options.EvaluationConfigurationFromCommandLineOptions;
import com.sri.ai.praise.empiricalevaluation.api.configuration.Configuration;
import com.sri.ai.praise.empiricalevaluation.core.Evaluation;
import com.sri.ai.praise.empiricalevaluation.core.configuration.DefaultConfiguration;
import com.sri.ai.praise.model.common.io.PagedModelContainer;
import com.sri.ai.praise.probabilisticsolver.SolverConfiguration;

/**
 * Provides a static method for outputting evaluation results for given solvers,
 * models and configurations.
 * 
 * @author braz
 *
 */
public abstract class AbstractEvaluationExecutable {

	/**
	 * A method making a {@link PagedModelContainer} from evaluation arguments.
	 * 
	 * @param evaluationArgs
	 * @return
	 * @throws IOException
	 */
	abstract protected PagedModelContainer makeModelsContainer(Configuration evaluationArgs) throws IOException;

	/**
	 * Returns options specifications.
	 * 
	 * @return
	 * @throws IOException 
	 * @throws FileNotFoundException 
	 */
	protected EvaluationConfigurationFromCommandLineOptions makeEvaluationArgumentsFromCommandLineOptions(String args[]) throws FileNotFoundException, IOException {
		return new EvaluationConfigurationFromCommandLineOptions(args);
	}

	public void run(String[] args) throws Exception {
		
		try (Configuration configuration = getConfiguration(args)) {

			PagedModelContainer modelsContainer = makeModelsContainer(configuration);
			
			List<SolverConfiguration> solverConfigurations = makeSolverConfigurations(configuration);
	
			PrintStream notificationOut = configuration.getNotificationOut();
			PrintStream resultOut = configuration.getResultOut();
	
			evaluate(configuration, modelsContainer, solverConfigurations, notificationOut, resultOut);
		}
	}

	/**
	 * Evaluates given solvers on given models according to given evaluation
	 * configurations and streams for notifications and results.
	 * 
	 * @param configuration
	 * @param modelsContainer
	 * @param solverConfigurations
	 * @param notificationOut
	 * @param resultOut
	 */
	private static void evaluate(
			Configuration configuration, 
			PagedModelContainer modelsContainer,
			List<SolverConfiguration> solverConfigurations, 
			PrintStream notificationOut,
			PrintStream resultOut) {
	
		Evaluation evaluation = new Evaluation(configuration, solverConfigurations, modelsContainer, notificationOut, resultOut);
		evaluation.evaluate();
	}

	private List<SolverConfiguration> makeSolverConfigurations(Configuration configuration) {
		List<SolverConfiguration> solverConfigurations = new ArrayList<>();
		for (String solverImplementationClassName : configuration.getSolverImplementationClassNames()) {
			SolverConfiguration solverConfiguration = makeSolverConfiguration(solverImplementationClassName, configuration);
			solverConfigurations.add(solverConfiguration);
		}
		return solverConfigurations;
	}

	private SolverConfiguration makeSolverConfiguration(String solverImplementationClassName, Configuration configuration) {
		SolverConfiguration solverConfiguration = 
				new SolverConfiguration(
						solverImplementationClassName,
						configuration.getTotalCPURuntimeLimitSecondsPerSolveAttempt(),
						configuration.getTotalMemoryLimitInMegabytesPerSolveAttempt(),
						!configuration.doesNotCacheTranslations(),
						configuration.getWorkingDirectory());
		return solverConfiguration;
	}

	private Configuration getConfiguration(String[] args)
			throws UnsupportedEncodingException, FileNotFoundException, IOException {

		EvaluationConfigurationFromCommandLineOptions optionSpecs = makeEvaluationArgumentsFromCommandLineOptions(args);
		return optionSpecs.getConfiguration();
	}
}