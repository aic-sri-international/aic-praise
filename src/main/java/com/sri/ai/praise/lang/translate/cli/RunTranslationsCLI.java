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
package com.sri.ai.praise.lang.translate.cli;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Stopwatch;
import com.sri.ai.praise.lang.ModelLanguage;
import com.sri.ai.praise.lang.translate.Translator;
import com.sri.ai.praise.lang.translate.TranslatorFactory;

/**
 * Command Line Interface (CLI) for for performing a set of translation.
 * 
 * @author oreilly
 *
 */
@Beta
public class RunTranslationsCLI {
	
	static class TranslationArgs {
		File             rootModelOutputDirectory;
		List<Translator> translators = new ArrayList<>();
	}

	/**
	 * Translate Probabilistic Models based on given command line arguments.
	 * 
	 * @param args
	 *        pass '--help' to see description of expected program arguments.
	 */
	public static void main(String[] args) {
		TranslationArgs translationArgs = getArgs(args);

		for (Translator translator : translationArgs.translators) {
			File sourceDirectory = new File(translationArgs.rootModelOutputDirectory, translator.getSource().getCode());
			if (!sourceDirectory.isDirectory()) {
				throw new IllegalArgumentException("Source Directory "+sourceDirectory+" does not exist");
			}
			File targetDirectory = new File(translationArgs.rootModelOutputDirectory, translator.getTarget().getCode());
			if (!targetDirectory.isDirectory()) {
				targetDirectory.mkdir();
			}
			
			String sourceModelFileExtension = translator.getInputFileExtensions()[0];
			for (File sourceModelFile : sourceDirectory.listFiles((dir, name) -> name.endsWith(sourceModelFileExtension))) {
				
				System.out.println("Translating "+sourceModelFile.getName()+" from "+translator.getSource().getCode()+" to "+translator.getTarget().getCode());
				
				String sourceModelFileNameWithNoExtension = translator.getInputModelFileNameWithNoExtension(sourceModelFile);
				Stopwatch sw = Stopwatch.createStarted();
				try (InputModelReaders inputModelReaders = new InputModelReaders(translator, sourceModelFile, sourceModelFileExtension);
					 TranslatedOutputs translatedOutputs = new TranslatedOutputs(translator, sourceModelFile, sourceModelFileExtension)
					 ) {
					
					translator.translate(sourceModelFileNameWithNoExtension, inputModelReaders.readers, translatedOutputs.writers);
				}
				catch (Exception ex) {
					System.err.println("Error during translation");
					ex.printStackTrace();
					System.exit(-1);
				}
				sw.stop();
				
				System.out.println("Took "+sw.toString());
// TODO - append the time it took to translate the file to a .csv file				
			}
		}
	}
	
	private static TranslationArgs getArgs(String[] args) {
		TranslationArgs result = new TranslationArgs();
// TODO - get values from the command line arguments, e.g:
//      -modelFileDirectory -translate=HOGMv1-to-UAI-to-HuginDotNet	
// see RandomHOGMv1Generator for example.		
		result.rootModelOutputDirectory = new File(args[0]);
		result.translators.add(TranslatorFactory.newTranslator(ModelLanguage.HOGMv1, ModelLanguage.UAI));	
		result.translators.add(TranslatorFactory.newTranslator(ModelLanguage.HOGMv1, ModelLanguage.UAI));
		
		return result;
	}
		
	private static class InputModelReaders implements AutoCloseable {
		public Reader[] readers;
		
		public InputModelReaders(Translator translator, File sourceModelFile, String sourceModelFileExtension) throws Exception {
			readers = new Reader[translator.getNumberOfInputs()];
			String modelName = translator.getInputModelFileNameWithNoExtension(sourceModelFile);
			for (int i = 0; i < readers.length; i++) {
				readers[i] = Files.newBufferedReader(new File(sourceModelFile.getParent(), modelName+translator.getInputFileExtensions()[i]).toPath(), 
														translator.getSourceCharset());
			}
		}
			
		@Override
		public void close() throws IOException {
			for (Reader r : readers) {
				r.close();
			}
		}
	}
	
	private static class TranslatedOutputs implements AutoCloseable {
		public PrintWriter[] writers;
		
		public TranslatedOutputs(Translator translator, File sourceModelFile, String sourceModelFileExtension) throws Exception {
			writers = new PrintWriter[translator.getNumberOfOutputs()];
			
			String modelName = translator.getInputModelFileNameWithNoExtension(sourceModelFile);
			File outputDir = new File(sourceModelFile.getParentFile().getParent(), translator.getTarget().getCode());
			for (int i = 0; i < writers.length; i++) {
				writers[i] = new PrintWriter(Files.newBufferedWriter(new File(outputDir, modelName+translator.getOutputFileExtensions()[i]).toPath(),
											 						 translator.getTargetCharset()));
			}
		}
		
		@Override
		public void close() throws IOException {
			for (PrintWriter writer : writers) {
				writer.flush();
				writer.close();
			}
		}
	}
}
