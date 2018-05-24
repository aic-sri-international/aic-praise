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

import static com.sri.ai.praise.modelscontainer.PagedModelContainer.getModelPagesFromURI;
import static com.sri.ai.util.Util.collect;
import static com.sri.ai.util.Util.getFirstNonNullResultOrNull;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.thereExists;
import static java.util.stream.Collectors.joining;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;

import joptsimple.OptionException;
import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;

import com.google.common.annotations.Beta;
import com.google.common.base.Charsets;
import com.google.common.base.Predicate;
import com.sri.ai.praise.lang.ModelLanguage;
import com.sri.ai.praise.modelscontainer.ModelPage;
import com.sri.ai.praise.modelscontainer.PagedModelContainer;
import com.sri.ai.praise.probabilisticsolver.core.praise.PRAiSESolver;
import com.sri.ai.util.Util;

/**
 * Command line interface for running {@link PRAiSESolver}.
 *
 * @author oreilly, braz
 *
 */
@Beta
public class PRAiSECommandLineOptions {

	private static final String LOWER_CASE_PRAISE_FILE_DEFAULT_EXTENSION = PagedModelContainer.DEFAULT_CONTAINER_FILE_EXTENSION.toLowerCase();
	private static final Charset FILE_CHARSET  = Charsets.UTF_8;
	private static final ModelLanguage DEFAULT_LANGUAGE = ModelLanguage.HOGMv1;

	public List<ModelPage> modelPages;
	public PrintStream   out = System.out;        // --output   (optional - 0 or 1)
	public boolean showModel = false;
	public boolean countSummations = false;
	public boolean showSummations = false;
	public boolean showDebugOutput = false;

	private List<File>    inputFiles      = new ArrayList<>(); // non option arguments (at least 1 required)
	private ModelLanguage inputLanguage   = null;              // --language (optional - 0 or 1)
	private List<String>  globalQueries   = new ArrayList<>(); // --query    (optional - 0 or more)
	private List<File> nonContainerFiles = new ArrayList<>();

	private OptionParser parser;
	private OptionSet options;
	private OptionSpec<String>  languageOptionSpec;
	private OptionSpec<String>  queryOptionSpec;
	private OptionSpec<File>    outputFileOptionSpec;
	private OptionSpec<Void>    debugOptionSpec;
	private OptionSpec<Void>    helpOptionSpec;
	private String usage;

	private List<String> errors   = new ArrayList<>();
	private List<String> warnings = new ArrayList<>();

	public PRAiSECommandLineOptions(String[] args) throws UnsupportedEncodingException, FileNotFoundException, IOException {

		parser = new OptionParser();

		languageOptionSpec   = parser.accepts("language",   "input model language (code), allowed values are " + getLegalModelLanguageCodesDescription()).withRequiredArg().ofType(String.class);
		queryOptionSpec      = parser.accepts("query",      "query to run over all input models").withRequiredArg().ofType(String.class);
							   parser.accepts("model",      "show solved model in output");
		                       parser.accepts("count",      "inform how many summations have been performed for each query");
		                       parser.accepts("summations", "shows number of summations and integrations performed, if they are being counted (with --count)");
		outputFileOptionSpec = parser.accepts("output",     "output file name (defaults to stdout).").withRequiredArg().ofType(File.class);

		debugOptionSpec = parser.accepts("debug", "Output detailed error messaages with stack traces when available");
		helpOptionSpec = parser.accepts("help", "command line options help").forHelp();

		usage =
				"java " + PRAiSECommandLineOptions.class.getName() + " [--help] [--language language_code] [--query global_query_string] [--output output_file_name] [--debug] inputModelFile ..."
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

		setupParameters(args);

		outputWarningsAndErrors();
	}

	private void setupParameters(String[] args) throws IOException, FileNotFoundException, UnsupportedEncodingException {
		try {
			parseArguments(args);
			setDebugOutput();
			setRecordingOfSummations();
			showHelpMessageAndExitIfRequested();
			setGlobalQueries();
			collectInputFiles();
			setInputLanguage();
			setOutputFile();
			collectModelPages();
		}
		catch (OptionException optionException) {
			errors.add(optionException.getMessage());
		}
	}

	private void parseArguments(String[] args) {
		options = parser.parse(args);
	}

	private void setDebugOutput() {
		showDebugOutput = options.has(debugOptionSpec);
	}

	private void setRecordingOfSummations() {
		showModel = options.has("model");
		countSummations = options.has("count");
		showSummations = options.has("summations");
		if (showSummations && !countSummations) {
			errors.add("Cannot show summations (option --summations) if not counting them (option --count)");
		}
	}

	private void showHelpMessageAndExitIfRequested() throws IOException {
		if (options.has(helpOptionSpec)) {
			System.out.println(usage);
			parser.printHelpOn(System.out);
			System.exit(0);
		}
	}

	private void collectInputFiles() {
		for (Object nonOptionArgument : options.nonOptionArguments()) {
			String inputFileName = (String) nonOptionArgument;
			collectInputFileWithName(inputFileName);
		}
		checkIfThereAreInputFiles();
	}

	private void collectInputFileWithName(String inputFileName) {
		File inputFile = new File(inputFileName.toString());
		if (inputFile.exists()) {
			inputFiles.add(inputFile);
		}
		else {
			errors.add("Input file [" + inputFileName + "] is not a file or does not exist.");
		}
	}

	private void checkIfThereAreInputFiles() {
		if (inputFiles.size() == 0) {
			errors.add("No input files specified");
		}
	}

	private void setGlobalQueries() {
		if (options.has(queryOptionSpec)) {
			for (String commandLineQuery : options.valuesOf(queryOptionSpec)) {
				globalQueries.add(commandLineQuery);
			}
		}
	}

	private void setInputLanguage() {
		if (options.has(languageOptionSpec)) {
			setOptionsProvidedLanguage();
		}
		else {
			guessLanguageModelAndLogError();
		}
	}

	private void setOptionsProvidedLanguage() {
		String languageCode = options.valueOf(languageOptionSpec);
		inputLanguage = findLanguageModel(languageCode);
		if (inputLanguage == null) {
			errors.add("Input language code " + languageCode + " is not legal. Legal values are " + getLegalModelLanguageCodesDescription() + ".");
		}
	}

	private void guessLanguageModelAndLogError() {
		inputLanguage = guessLanguageModel();
		if (inputLanguage == null) {
			errors.add("Unable to guess the input language from the given input file name extensions. Legal input language codes are " + getLegalModelLanguageCodesDescription() + ".");
		}
	}

	private void setOutputFile() throws FileNotFoundException, UnsupportedEncodingException {
		if (options.has(outputFileOptionSpec)) {
			File outFile = collectOutputFile();
			openOutputFileIfThereAreNoErrorsSoFar(outFile);
		}
	}

	private File collectOutputFile() {
		File outFile = options.valueOf(outputFileOptionSpec);
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

	private void openOutputFileIfThereAreNoErrorsSoFar(File outFile) throws FileNotFoundException, UnsupportedEncodingException {
		if (errors.size() == 0) {
			out = new PrintStream(outFile, FILE_CHARSET.name());
		}
	}

	private void outputWarningsAndErrors() throws IOException {

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

	private void collectModelPages() throws IOException {
		modelPages = new ArrayList<>();
		collectModelPagesFromContainerFiles();
		collectModelPagesFromNonContainerFiles();
	}

	private void collectModelPagesFromContainerFiles() {
		for (File inputFile : inputFiles) {
			if (isContainerFile(inputFile)) {
				collectModelPagesFromFileWithAddedGlobalQueries(inputFile);
			}
		}
	}

	private void collectModelPagesFromNonContainerFiles() {
		collectNonContainerFiles();
		collectModelPageFromNonContainerFilesIfAny();
	}

	private void collectNonContainerFiles() {
		collect(inputFiles, f -> !isContainerFile(f), nonContainerFiles);
	}

	private void collectModelPagesFromFileWithAddedGlobalQueries(File containerFile) {
		List<ModelPage> modelPagesFromFile = getModelPagesFromURI(containerFile.toURI());
		mapIntoList(modelPagesFromFile, m -> m.makeCopyWithExtraQueries(globalQueries), modelPages);
	}

	private static boolean isContainerFile(File inputFile) {
		return inputFile.getName().endsWith(PagedModelContainer.DEFAULT_CONTAINER_FILE_EXTENSION);
	}

	private void collectModelPageFromNonContainerFilesIfAny() {
		if (nonContainerFiles.size() > 0) {
			ModelPage modelPage = makeModelPageFromNonContainerFiles(nonContainerFiles);
			modelPages.add(modelPage);
		}
	}

	private ModelPage makeModelPageFromNonContainerFiles(List<File> nonContainerFiles) {
		String unionModel =
				nonContainerFiles
				.stream()
				.map(file -> Util.getFileContent(file))
				.collect(joining("\n"));
		ModelPage modelPage = makeModelPage(unionModel);
		return modelPage;
	}

	private ModelPage makeModelPage(String unionModel) {
		return new ModelPage(inputLanguage, "FactorNetwork from concatenation of non-container input files", unionModel, globalQueries);
	}

	private static String getLegalModelLanguageCodesDescription() {
		String result = join(mapIntoList(ModelLanguage.values(), ModelLanguage::getCode));
		return result;
	}

	private static ModelLanguage findLanguageModel(String languageCode) {
		List<ModelLanguage> modelLanguages = Arrays.asList(ModelLanguage.values());
		ModelLanguage result = getFirstSatisfyingPredicateOrNull(modelLanguages, hasLanguageCode(languageCode));
		return result;
	}

	private static Predicate<ModelLanguage> hasLanguageCode(String languageCode) {
		return ml -> isLanguageCodeForModelLanguage(languageCode, ml);
	}

	private static boolean isLanguageCodeForModelLanguage(String languageCode, ModelLanguage modelLanguage) {
		boolean result = modelLanguage.getCode().toLowerCase().equals(languageCode.toLowerCase());
		return result;
	}

	private ModelLanguage guessLanguageModel() {
		ModelLanguage result =
				getFirstNonNullResultOrNull(
						() -> getLanguageMatchingExtensionOfSomeInputFile(),
						() -> getLanguageFromInputFilesWithDefaultPRAiSEFileExtension(),
						() -> DEFAULT_LANGUAGE);
		return result;
	}

	private ModelLanguage getLanguageMatchingExtensionOfSomeInputFile() {
		List<ModelLanguage> modelLanguages = Arrays.asList(ModelLanguage.values());
		ModelLanguage result = getFirstSatisfyingPredicateOrNull(modelLanguages, isExtensionForSomeOfTheInputFiles());
		return result;
	}

	private Predicate<ModelLanguage> isExtensionForSomeOfTheInputFiles() {
		return ml -> thereExists(inputFiles, hasExtension(ml));
	}

	private static Predicate<File> hasExtension(ModelLanguage modelLanguage) {
		return file -> lowerCaseName(file).endsWith(lowerCaseFileExtension(modelLanguage));
	}

	private static String lowerCaseName(File file) {
		String result = file.getName().toLowerCase();
		return result;
	}

	private static String lowerCaseFileExtension(ModelLanguage modelLanguage) {
		String result = modelLanguage.getDefaultFileExtension().toLowerCase();
		return result;
	}

	private ModelLanguage getLanguageFromInputFilesWithDefaultPRAiSEFileExtension() {
		ModelLanguage result;
		result = inputFiles.stream()
					.filter(fileHasPRAiSEFileDefaultExtension)
					.map(fromPRAiSEFileToLanguage)
					.findFirst().orElse(null);
		return result;
	}

	private static java.util.function.Predicate<? super File> fileHasPRAiSEFileDefaultExtension =
			file -> lowerCaseName(file).endsWith(LOWER_CASE_PRAISE_FILE_DEFAULT_EXTENSION);

	private static Function<? super File, ? extends ModelLanguage> fromPRAiSEFileToLanguage =
			praiseFile -> getLanguageFromPRAiSEFile(praiseFile);

	private static ModelLanguage getLanguageFromPRAiSEFile(File praiseFile) {
		List<ModelPage> modelPages = getModelPagesFromURI(praiseFile.toURI());
		ModelLanguage modelLanguage = Util.getFirstNonNullResultOrNull(modelPages, m -> m.getLanguage());
		return modelLanguage;
	}
}
