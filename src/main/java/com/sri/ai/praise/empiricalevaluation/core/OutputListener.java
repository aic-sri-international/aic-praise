package com.sri.ai.praise.empiricalevaluation.core;

import java.io.PrintStream;
import java.util.List;
import java.util.StringJoiner;

import com.sri.ai.praise.empiricalevaluation.api.configuration.SolverEvaluationConfiguration;
import com.sri.ai.praise.model.common.io.ModelPage;
import com.sri.ai.praise.model.common.io.PagedModelContainer;
import com.sri.ai.praise.probabilisticsolver.Solver;

public class OutputListener {

	private PrintStream notificationOut;
	private PrintStream resultOut;
	
	public OutputListener(PrintStream notificationOut, PrintStream resultOut) {
		super();
		this.notificationOut = notificationOut;
		this.resultOut = resultOut;
	}

	public void notification(String notification) {
		notificationOut.println(notification);
	}

	public void notificationException(Exception exception) {
		exception.printStackTrace(notificationOut);
	}

	public StringJoiner initializeQueryLine(String domainSizes, SolverEvaluationConfiguration configuration, String problemName) {
		notification("Starting to evaluate " + problemName);
		StringJoiner csvLine;
		csvLine = new StringJoiner(",");
		csvLine.add(problemName);
		csvLine.add(configuration.getType().name());
		csvLine.add(domainSizes);
		csvLine.add(" " + configuration.getNumberRunsToAverageOver());
		return csvLine;
	}

	public void csvResultOutput(String csvLine) {
		resultOut.println(csvLine);
	}

	public void notifyTotalEvaluationTime(long evaluationStart, long evaluationEnd) {
		notification("Evaluation took " + toDurationString(evaluationEnd - evaluationStart) + " to run to completion.");
	}

	public void notifyAboutBeginningOfBurnInForAllSolvers(PagedModelContainer modelsToEvaluateContainer, ModelPage burnInModel, String burnInQuery) {
		notification("Starting burn in for all solvers based on '" + modelsToEvaluateContainer.getName() + " - " + burnInModel.getName() + " : " + burnInQuery + "'");
	}

	public void notifyAboutOneRunOfBurnInResult(Solver solver, SolverEvaluationResult result) {
		notification("Burn in for " + solver.getName() + " complete. Average inference time = " + toDurationString(result.averageInferenceTimeInMilliseconds));
	}

	public void writeInitialHeader(StringJoiner csvLine) {
		csvLine.add("Problem");
		csvLine.add("Inference Type");
		csvLine.add("Domain Size(s)");
		csvLine.add("# runs values averaged over");
	}

	public void outputReportHeaderLine(List<Solver> solvers) {
		StringJoiner csvLine = new StringJoiner(",");
		writeInitialHeader(csvLine);
		for (Solver solver : solvers) {
			writeHeaderForSolver(csvLine, solver);
		}
		notification("Starting to generate Evaluation Report");
		csvResultOutput(csvLine.toString());
	}

	public void writeHeaderForSolver(StringJoiner csvLine, Solver solver) {
		csvLine.add("Solver");
		csvLine.add("Result for " + solver.getName());
		csvLine.add("Inference ms. for " + solver.getName());
		csvLine.add("HH:MM:SS.");
		csvLine.add("Translation ms. for " + solver.getName());
		csvLine.add("HH:MM:SS.");
	}

	public void outputSolverOutput(String problemName, Solver solver, StringJoiner csvLine, SolverEvaluationResult solverResult) {
		csvLine.add(solver.getName());
		csvLine.add(solverResult.failed ? "FAILED" : "" + solverResult.answer);
		csvLine.add("" + solverResult.averageInferenceTimeInMilliseconds);
		csvLine.add(toDurationString(solverResult.averageInferenceTimeInMilliseconds));
		csvLine.add("" + solverResult.averagelTranslationTimeInMilliseconds);
		csvLine.add(toDurationString(solverResult.averagelTranslationTimeInMilliseconds));
		
		notification("Solver " + solver.getName() + " took an average inference time of " + toDurationString(solverResult.averageInferenceTimeInMilliseconds) + " to solve " + problemName);
	}

	private String toDurationString(long duration) {
		long hours = 0L, minutes = 0L, seconds = 0L, milliseconds = 0L;
		long remainingDuration = duration;
		
		if (remainingDuration != 0) {
			hours    = remainingDuration / 3600000;
			remainingDuration = remainingDuration % 3600000; 
		}
		if (remainingDuration != 0) {
			minutes  = remainingDuration / 60000;
			remainingDuration = remainingDuration % 60000;
		}
		if (remainingDuration != 0) {
			seconds  = remainingDuration / 1000;
			remainingDuration = remainingDuration % 1000;
		}
		milliseconds = remainingDuration;
		
		return hours + "h" + minutes + "m" + seconds + "." + milliseconds + "s";
	}

}