/*
 * Copyright (c) 2015, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-praise nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.praise.application.praise.commandline;

import static com.sri.ai.praise.model.common.io.PagedModelContainer.getModelPagesFromURI;
import static com.sri.ai.util.Util.getFirstNonNullResultOrNull;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.thereExists;
import static com.sri.ai.util.Util.toHoursMinutesAndSecondsString;
import static java.util.stream.Collectors.joining;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import joptsimple.OptionException;
import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;

import com.google.common.annotations.Beta;
import com.google.common.base.Charsets;
import com.google.common.base.Predicate;
import com.sri.ai.praise.inference.HOGMQueryResult;
import com.sri.ai.praise.inference.HOGMQueryRunner;
import com.sri.ai.praise.lang.ModelLanguage;
import com.sri.ai.praise.model.common.io.ModelPage;
import com.sri.ai.praise.model.common.io.PagedModelContainer;
import com.sri.ai.praise.probabilisticsolver.core.praise.PRAiSESolver;

/**
 * Command line interface for running {@link PRAiSESolver}.
 * 
 * @author oreilly
 *
 */
@Beta
public class PRAiSE {
	
	private static final String LOWER_CASE_PRAISE_FILE_DEFAULT_EXTENSION = PagedModelContainer.DEFAULT_CONTAINER_FILE_EXTENSION.toLowerCase();
	
	public static final Charset FILE_CHARSET  = Charsets.UTF_8;
	
	public static final String  RESULT_PREFIX = "RESULT     = ";
	
	public static final ModelLanguage DEFAULT_LANGUAGE = ModelLanguage.HOGMv1;
	
	
	List<String> errors   = new ArrayList<>();
	List<String> warnings = new ArrayList<>();

	public static void main (String[] args) {
		PRAiSE praise = new PRAiSE();
		praise.run(args);
	}

	public void run (String[] args) {
		try (PRAiSEArguments arguments = makePRAiSEArguments(args)) {
			
			List<ModelPage> hogModelsToQuery = getHOGModelsToQuery(arguments);
		
			for (ModelPage hogModelToQuery : hogModelsToQuery) {
				arguments.out.print("MODEL NAME = ");
				arguments.out.println(hogModelToQuery.getName());
				arguments.out.println("MODEL      = ");
				arguments.out.println(hogModelToQuery.getModelString());
				HOGMQueryRunner queryRunner = new  HOGMQueryRunner(hogModelToQuery.getModelString(), hogModelToQuery.getDefaultQueriesToRun());
				List<HOGMQueryResult> hogModelQueryResults = queryRunner.query();
				hogModelQueryResults.forEach(hogModelQueryResult -> {
					arguments.out.print("QUERY      = ");
					arguments.out.println(hogModelQueryResult.getQueryString());
					arguments.out.print(RESULT_PREFIX);
					arguments.out.println(queryRunner.simplifyAnswer(hogModelQueryResult.getResult(), hogModelQueryResult.getQueryExpression()));
					arguments.out.print("TOOK       = ");
					arguments.out.println(toHoursMinutesAndSecondsString(hogModelQueryResult.getMillisecondsToCompute()) + "\n");
					if (hogModelQueryResult.isErrors()) {
						hogModelQueryResult.getErrors().forEach(error -> {
							arguments.out.println("ERROR ="+error.getErrorMessage());
							if (error.getThrowable() != null) {
								arguments.out.println("THROWABLE =");
								error.getThrowable().printStackTrace(arguments.out);
							}
						});
					}
				});
			}
		}
		catch (Exception ex) {
			System.err.println("Error calling SGSolver");
			ex.printStackTrace();
		}
	}
	
	public PRAiSEArguments makePRAiSEArguments(String[] args) throws UnsupportedEncodingException, FileNotFoundException, IOException {
		
		PRAiSEArguments result = new PRAiSEArguments();
		
		OptionParser parser = new OptionParser();		

		OptionSpec<String> language   = parser.accepts("language", "input model language (code), allowed values are "+getLegalModelLanguageCodesDescription()).withRequiredArg().ofType(String.class);
		OptionSpec<String> query      = parser.accepts("query", "query to run over all input models").withRequiredArg().ofType(String.class);
		OptionSpec<File>   outputFile = parser.accepts("output",   "output file name (defaults to stdout).").withRequiredArg().ofType(File.class);
		
		OptionSpec<Void> help = parser.accepts("help", "command line options help").forHelp();

		String usage = 
				"java " + PRAiSE.class.getName() + " [--help] [--language language_code] [--query global_query_string] [--output output_file_name] inputModelFile ..."
				+ "\n\n"
				+ "This command reads a set of models from input files and executes a set of queries on each of them.\n\n"
				+ "The models are obtained in the following manner:\n"
				+ "- input files may be one or more; they can be .praise files (saved from the PRAiSE editor and solver) or plain text files.\n"
				+ "- each .praise input file contains possibly multiple pages, each with a model and a set of queries for it (see PRAiSE editor and solver).\n"
				+ "- multiple plain text input files are combined into a single model (not combined with .praise models).\n\n"
				+ "The queries are obtained in the following manner:\n"
				+ "- each page in each .praise file contains a list of queries for its specific model\n"
				+ "- queries specified with --query option will apply to all models from all .praise files and to the model from combined plain text input files.\n\n"
				+ "Evidence can be encoded as deterministic statements (see examples in PRAiSE editor and solver).\n"
						;
		
		setupParameters(args, result, parser, language, query, outputFile, help, usage, errors, warnings);
		
		outputWarningsAndErrors(parser, usage, errors, warnings);
		
		return result;
	}

	private void setupParameters(String[] args, PRAiSEArguments result, OptionParser parser, OptionSpec<String> language, OptionSpec<String> query, OptionSpec<File> outputFile, OptionSpec<Void> help, String usage, List<String> errors, List<String> warnings) throws IOException, FileNotFoundException, UnsupportedEncodingException {
		try {
			OptionSet options = parser.parse(args);
			
			showHelpMessageAndExitIfRequested(help, options, usage, parser);
			
			collectInputFiles(options, errors, result);
			
			setGlobalQueries(query, options, result);
			
			setInputLanguage(result, language, errors, options);
			
			setupOutputFile(options, outputFile, errors, warnings, result);
		}
		catch (OptionException oe) {
			errors.add(oe.getMessage());
		}
	}

	private static void showHelpMessageAndExitIfRequested(OptionSpec<Void> help, OptionSet options, String usage, OptionParser parser) throws IOException {
		if (options.has(help)) {
			System.out.println(usage);
			parser.printHelpOn(System.out);
			System.exit(0);
		}
	}

	private static void collectInputFiles(OptionSet options, List<String> errors, PRAiSEArguments result) {
		for (Object inputFileName : options.nonOptionArguments()) {
			collectInputFileWithName(inputFileName, errors, result);
		}
		checkIfThereAreInputFiles(errors, result);
	}

	private static void collectInputFileWithName(Object inputFileName, List<String> errors, PRAiSEArguments result) {
		File inputFile = new File(inputFileName.toString());
		if (inputFile.exists()) {
			result.inputFiles.add(inputFile);
		}
		else {
			errors.add("Input file [" + inputFileName + "] is not a file or does not exist.");
		}
	}

	private static void checkIfThereAreInputFiles(List<String> errors, PRAiSEArguments result) {
		if (result.inputFiles.size() == 0) {
			errors.add("No input files specified");
		}
	}

	private static void setGlobalQueries(OptionSpec<String> query, OptionSet options, PRAiSEArguments result) {
		if (options.has(query)) {
			for (String commandLineQuery : options.valuesOf(query)) {
				result.globalQueries.add(commandLineQuery);
			}
		}
	}

	private static void setInputLanguage(PRAiSEArguments result, OptionSpec<String> language, List<String> errors, OptionSet options) {
		if (options.has(language)) {
			setOptionsProvidedLanguage(language, options, errors, result);
		}
		else {
			guessLanguageModel(result, errors);
		}
	}

	private static void setOptionsProvidedLanguage(OptionSpec<String> language, OptionSet options, List<String> errors, PRAiSEArguments result) {
		String languageCode = options.valueOf(language);
		result.inputLanguage = findLanguageModel(languageCode);
		if (result.inputLanguage == null) {
			errors.add("Input language code " + languageCode + " is not legal. Legal values are " + getLegalModelLanguageCodesDescription() + ".");
		}
	}

	private static void guessLanguageModel(PRAiSEArguments result, List<String> errors) {
		result.inputLanguage = guessLanguageModel(result.inputFiles);
		if (result.inputLanguage == null) {
			errors.add("Unable to guess the input language from the given input file name extensions. Legal input language codes are "+getLegalModelLanguageCodesDescription()+".");
		}
	}

	private static void setupOutputFile(OptionSet options, OptionSpec<File> outputFile, List<String> errors, List<String> warnings, PRAiSEArguments result) throws FileNotFoundException, UnsupportedEncodingException {
		if (options.has(outputFile)) {
			File outFile = collectOutputFile(outputFile, options, errors, warnings);
			openOutfileFileIfThereAreNoErrorsSoFar(errors, result, outFile);
		}
	}

	private static File collectOutputFile(OptionSpec<File> outputFile, OptionSet options, List<String> errors, List<String> warnings) {
		File outFile = options.valueOf(outputFile);
		if (outFile.exists()) {
			if (outFile.isDirectory()) {
				errors.add("Output file specified [" + outFile.getAbsolutePath() + "] is a directory, must be a legal file name.");
			}
			else {
				warnings.add("Warning output file [" + outFile.getAbsolutePath() + "] already exists, will be overwritten.");
			}
		}
		return outFile;
	}

	private static void openOutfileFileIfThereAreNoErrorsSoFar(List<String> errors, PRAiSEArguments result, File outFile) throws FileNotFoundException, UnsupportedEncodingException {
		if (errors.size() == 0) {
			result.out = new PrintStream(outFile, FILE_CHARSET.name());
		}
	}
	
	private void outputWarningsAndErrors(OptionParser parser, String usage, List<String> errors, List<String> warnings) throws IOException {
		
		if (warnings.size() > 0) {
			warnings.forEach(warning -> System.err.println("WARNING: "+ warning));
		}
		
		if (errors.size() > 0) {
			errors.forEach(error -> System.err.println("ERROR: "+ error));
			System.err.println(usage);
			parser.printHelpOn(System.err);
			System.exit(1);
		}
	}

	private static List<ModelPage> getHOGModelsToQuery(PRAiSEArguments solverArguments) throws IOException {
		
		List<ModelPage> result = new ArrayList<>();
		
		List<File> nonContainerFiles = 
				collectModelPagesFromContainerFilesAndNonContainerFiles(
						solverArguments, solverArguments.globalQueries, result);
		
		addModelPageFromNonContainerFilesIfAny(
				nonContainerFiles, solverArguments.inputLanguage, solverArguments.globalQueries, result);
		
		return result;
	}

	private static List<File> collectModelPagesFromContainerFilesAndNonContainerFiles(
			PRAiSEArguments solverArguments, List<String> globalQueries, List<ModelPage> result) {
		
		List<File> nonContainerFiles = new ArrayList<>();
		for (File inputFile : solverArguments.inputFiles) {
			collectModelPageFromContainerFileOrCollectNonContainerFile(inputFile, globalQueries, result, nonContainerFiles);
		}
		return nonContainerFiles;
	}

	private static void collectModelPageFromContainerFileOrCollectNonContainerFile(File inputFile, List<String> globalQueries, List<ModelPage> result, List<File> nonContainerFiles) {
		if (isContainerFile(inputFile)) {
			addModelPagesFromFileWithAddedGlobalQueries(inputFile, globalQueries, result);
		}
		else {
			nonContainerFiles.add(inputFile);
		}
	}

	private static void addModelPagesFromFileWithAddedGlobalQueries(File inputFile, List<String> globalQueries, List<ModelPage> result) {
		List<ModelPage> modelPagesFromFile = getModelPagesFromURI(inputFile.toURI());
		mapIntoList(modelPagesFromFile, m -> m.makeCopyWithExtraQueries(globalQueries), result);
	}

	private static boolean isContainerFile(File inputFile) {
		return inputFile.getName().endsWith(PagedModelContainer.DEFAULT_CONTAINER_FILE_EXTENSION);
	}

	private static void addModelPageFromNonContainerFilesIfAny(List<File> nonContainerFiles, ModelLanguage inputLanguage, List<String> globalQueries, List<ModelPage> result) {
		if (nonContainerFiles.size() > 0) {
			ModelPage modelPage = makeModelPageFromNonContainerFiles(nonContainerFiles, inputLanguage, globalQueries);
			result.add(modelPage);
		}
	}

	private static ModelPage makeModelPageFromNonContainerFiles(List<File> nonContainerFiles, ModelLanguage inputLanguage, List<String> globalQueries) {
		String unionModel = 
				nonContainerFiles
				.stream()
				.map(file -> getFileContent(file))
				.collect(joining("\n"));
		ModelPage modelPage = makeModelPage(unionModel, inputLanguage, globalQueries);
		return modelPage;
	}

	private static ModelPage makeModelPage(String unionModel, ModelLanguage inputLanguage, List<String> globalQueries) {
		return new ModelPage(inputLanguage, "Model from concatenation of non-container input files", unionModel, globalQueries);
	}

	private static String getFileContent(File file) {
		try {
			String fileContents = Files.readAllLines(file.toPath()).stream().collect(Collectors.joining("\n"));
			return fileContents;
		}
		catch (IOException ioe) {
			throw new RuntimeException(ioe);
		}
	}
	
	private static String getLegalModelLanguageCodesDescription() {
		String result = join(mapIntoList(ModelLanguage.values(), ModelLanguage::getCode));
		return result;
	}
	
	private static ModelLanguage findLanguageModel(String languageCode) {
		List<ModelLanguage> modelLanguages = Arrays.asList(ModelLanguage.values());
		ModelLanguage result = getFirstSatisfyingPredicateOrNull(modelLanguages, ml -> isLanguageCodeForModelLanguage(languageCode, ml));
		return result;
	}

	private static boolean isLanguageCodeForModelLanguage(String languageCode, ModelLanguage modeLanguage) {
		boolean result = modeLanguage.getCode().toLowerCase().equals(languageCode.toLowerCase());
		return result;
	}
	
	private static ModelLanguage guessLanguageModel(List<File> inputFiles) {
		
		ModelLanguage result = getLanguageMatchingExtensionOfSomeFile(inputFiles);
		
		if (result == null) {
			result = getLanguageFromInputFileWithDefaultPRAiSEFileExtension(inputFiles);
		}
		
		if (result == null) {
			result = DEFAULT_LANGUAGE;
		}
		
		return result;
	}

	private static ModelLanguage getLanguageMatchingExtensionOfSomeFile(List<File> inputFiles) {
		List<ModelLanguage> modelLanguages = Arrays.asList(ModelLanguage.values());
		ModelLanguage result = getFirstSatisfyingPredicateOrNull(modelLanguages, isExtensionForSomeOfTheFiles(inputFiles));
		return result;
	}

	private static Predicate<ModelLanguage> isExtensionForSomeOfTheFiles(List<File> inputFiles) {
		return ml -> thereExists(inputFiles, hasExtension(ml));
	}

	private static Predicate<File> hasExtension(ModelLanguage ml) {
		return inputFile -> lowerCaseName(inputFile).endsWith(lowerCaseFileExtension(ml));
	}

	private static String lowerCaseName(File inputFile) {
		String result = inputFile.getName().toLowerCase();
		return result;
	}

	private static String lowerCaseFileExtension(ModelLanguage ml) {
		String result = ml.getDefaultFileExtension().toLowerCase();
		return result;
	}

	private static ModelLanguage getLanguageFromInputFileWithDefaultPRAiSEFileExtension(List<File> inputFiles) {
		ModelLanguage result;
		result = inputFiles.stream()
					.filter(fileHasPRAiSEFileDefaultExtension)
					.map(fromPRAiSEInputFileToLanguage)
					.findFirst().orElse(null);
		return result;
	}

	private static java.util.function.Predicate<? super File> fileHasPRAiSEFileDefaultExtension = 
			inputFile -> lowerCaseName(inputFile).endsWith(LOWER_CASE_PRAISE_FILE_DEFAULT_EXTENSION);
			
	private static Function<? super File, ? extends ModelLanguage> fromPRAiSEInputFileToLanguage = 
			praiseInputFile -> getLanguageFromFirstModel(praiseInputFile);

	private static ModelLanguage getLanguageFromFirstModel(File praiseInputFile) {
		List<ModelPage> models = getModelPagesFromURI(praiseInputFile.toURI());
		ModelLanguage praiseInputFileLanguage = getFirstNonNullResultOrNull(models, m -> m.getLanguage());
		return praiseInputFileLanguage;
	}
}
