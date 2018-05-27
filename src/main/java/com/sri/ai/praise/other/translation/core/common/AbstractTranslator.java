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
package com.sri.ai.praise.other.translation.core.common;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.nio.file.Files;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.other.translation.api.Translator;
import com.sri.ai.praise.other.translation.api.TranslatorOptions;

/**
 * Abstract Translator class to handle common functionality (e.g. caching).
 * 
 * @author oreilly
 *
 */
@Beta
public abstract class AbstractTranslator implements Translator {
	//
	// START-Translator
	@Override
	public void translate(String inputIdentifier, Reader[] inputModelReaders, PrintWriter[] translatedOutputs,
			TranslatorOptions options) throws Exception {
		if (options.isCacheTranslations()) {
			String cacheIdentifier = computeCacheIdentifier(inputIdentifier);
			List<File> cachedOutputs = new ArrayList<>(translatedOutputs.length);
			for (int i = 0; i < translatedOutputs.length; i++) {
				cachedOutputs.add(new File(options.getCacheDirectory(), getSource().getCode() + "-to-"
						+ getTarget().getCode() + "-" + i + "-" + cacheIdentifier + ".cached"));
			}

			// Ensure we have cached already
			if (!cachedOutputs.stream().allMatch(File::isFile)) {
				// No cached outputs so must generate them

				// Ensure all the files are new
				cachedOutputs.stream().forEach(cacheFile -> {
					try {
						cacheFile.delete();
						cacheFile.createNewFile();
					} catch (IOException ioe) {
						throw new RuntimeException(ioe);
					}
				});

				// We don't have the cached translation information, so must
				// perform the translation, first into the cached files
				PrintWriter[] cachedWriters = new PrintWriter[cachedOutputs.size()];
				for (int i = 0; i < cachedWriters.length; i++) {
					cachedWriters[i] = new PrintWriter(cachedOutputs.get(i));
				}
				translate(inputIdentifier, inputModelReaders, cachedWriters);
				for (int i = 0; i < cachedWriters.length; i++) {
					cachedWriters[i].flush();
					cachedWriters[i].close();
				}
			}
			// We now know we have cached files, now take these and feed them to
			// the translatedOutputs
			for (int i = 0; i < cachedOutputs.size(); i++) {
				BufferedReader br = Files.newBufferedReader(cachedOutputs.get(i).toPath());
				final PrintWriter translatedOutputI = translatedOutputs[i];
				br.lines().forEach(line -> translatedOutputI.println(line));
				translatedOutputI.flush();
			}
		} else {
			translate(inputIdentifier, inputModelReaders, translatedOutputs);
		}
	}
	// END-Translator
	//

	//
	// PROTECTED
	protected abstract void translate(String inputIdentifier, Reader[] inputModelReaders,
			PrintWriter[] translatedOutputs) throws Exception;

	protected String computeCacheIdentifier(String... identifyingInputs) {
		String result;
		try {
			MessageDigest messageDigest = MessageDigest.getInstance("MD5");
			for (int i = 0; i < identifyingInputs.length; i++) {
				messageDigest.update(identifyingInputs[i].getBytes());
			}
			// NOTE: Use replace calls to ensure only legal filenames are
			// generated from the BASE64 alphabet
			result = Base64.getEncoder().encodeToString(messageDigest.digest()).replace('+', '-').replace('/', '_');
		} catch (NoSuchAlgorithmException nsae) {
			throw new RuntimeException("Unexpected exception", nsae);
		}
		return result;
	}
}
