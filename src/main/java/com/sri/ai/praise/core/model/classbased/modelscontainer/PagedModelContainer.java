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
package com.sri.ai.praise.core.model.classbased.modelscontainer;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;

import com.google.common.annotations.Beta;
import com.google.common.base.Charsets;
import com.sri.ai.praise.core.model.api.ModelLanguage;
import com.sri.ai.util.base.Pair;

/**
 * A container model format that can be used to represent different string based model representations 
 * (i.e. is neutral of the modeling language) that can be used to read/write 1..n models of the same language 
 * (i.e. 1 model per page in the container, usually variants of the same base model) to a single file on disk. 
 * 
 * NOTE: by convention we are using '.praise' as the file extension name for these types of container files.
 * 
 * @author oreilly
 *
 */
@Beta
public class PagedModelContainer {
	//
	public static final String DEFAULT_CONTAINER_FILE_EXTENSION = ".praise";
	//
	public static final Charset FILE_CHARSET = Charsets.UTF_8;
	//
	private static final String MODEL_LANGUAGE_PREFIX      = "@LANG:";
	private static final String MODEL_SPECIFICATION_PREFIX = "@MODEL:";
	private static final String MODEL_FRAGMENT_PREFIX      = "@FRAGMENT:";
	//
	private static final String MODEL_FIELD_NAME           = "name";
	private static final String MODEL_FIELD_PARTS          = "parts";
	private static final String MODEL_FIELD_QUERIES        = "queries";	
	//
	private final String name;
	private final List<ModelPage> pages;
	
	public PagedModelContainer(String name, List<ModelPage> pages) {
		this.name  = name;
		this.pages = Collections.unmodifiableList(new ArrayList<>(pages));
	}
	
	public PagedModelContainer(String name, URI uri) throws IOException {
		this.name  = name;
		this.pages = getModelPagesFromURI(uri);
	}

	public PagedModelContainer(File file) throws IOException {
		this(file.getName(), file.toURI());
	}

	public String getName() {
		return name;
	}

	public List<ModelPage> getPages() {
		return pages;
	}
	
	@Override
	public String toString() {
		return name;
	}
	
	/**
	 * Convert a list of pairs, where each pair represents a model and a set of default
	 * queries associated with it, to the internal paged model container representation.<br>
	 * NOTE: Intended primarily to get into a form for saving to persistent storage.
	 * 
	 * @param containerModelLanguage
	 *        the model language to be associated with the models contained in this container.
	 * @param pageContents
	 *        a list of pairs, where each pair represents a model and a set of default queries associated with it.
	 * @return a string representation of the container format.
	 */
	public static String toInternalContainerRepresentation(ModelLanguage containerModelLanguage, List<Pair<String, List<String>>> pageContents) {
		StringBuilder result = new StringBuilder();
		// Output the language
		result.append(MODEL_LANGUAGE_PREFIX);
		result.append(containerModelLanguage.getCode());
		result.append("\n");
		
		// Output the models
		for (int i = 0; i < pageContents.size(); i++) {
			Pair<String, List<String>> pageContent = pageContents.get(i);
			result.append(MODEL_SPECIFICATION_PREFIX);
			appendField(MODEL_FIELD_NAME, Collections.singletonList("Page "+(i+1)), result);
			appendField(MODEL_FIELD_PARTS, Collections.singletonList("page-"+(i+1)), result);
			List<String> queryNamesForPage = new ArrayList<>();
			for (int q = 0; q < pageContent.second.size(); q++) {
				queryNamesForPage.add("query-page-"+(i+1)+"#"+(q+1));
			}
			appendField(MODEL_FIELD_QUERIES, queryNamesForPage, result);
			result.append("\n");
		}
		
		// Output the fragments and queries
		for (int i = 0; i < pageContents.size(); i++) {
			Pair<String, List<String>> pageContent = pageContents.get(i);
			// Ensure fragment indicators are on a newline
			if (!result.toString().endsWith("\n")) {
				result.append("\n");
			}
			result.append(MODEL_FRAGMENT_PREFIX);
			result.append("page-"+(i+1));
			result.append("\n");
			result.append(pageContent.first);
			List<String> queries = pageContent.second;
			for (int q = 0; q < queries.size(); q++) {
				result.append("\n");
				result.append(MODEL_FRAGMENT_PREFIX);
				result.append("query-page-"+(i+1)+"#"+(q+1));
				result.append("\n");
				result.append(queries.get(q));
				result.append("\n");
			}	
		}
		
		return result.toString();
	}
	
	public static List<ModelPage> getModelPagesFromURI(URI uri) {	
		try {
			List<ModelPage> result = new ArrayList<>();

			AtomicReference<ModelLanguage> containerModelLanguage = new AtomicReference<>();
			List<String> modelSpecifications = new ArrayList<>();
			Map<String, List<String>> fragments = new HashMap<>();

			try (BufferedReader in = new BufferedReader(new InputStreamReader(uri.toURL().openStream(), FILE_CHARSET))) {
				getContent(containerModelLanguage, in.lines(), modelSpecifications, fragments);
			}

			for (String modelSpecification : modelSpecifications) {
				String name          = extractField(MODEL_FIELD_NAME, modelSpecification);
				String model         = extractModel(MODEL_FIELD_PARTS, modelSpecification, fragments);
				List<String> queries = extractQueries(MODEL_FIELD_QUERIES, modelSpecification, fragments);

				result.add(new ModelPage(containerModelLanguage.get(), name, model, queries));
			}

			return result;
		}
		catch (IOException ioe) {
			throw new Error(ioe);
		}
	}
	
	//
	// PRIVATE
	//
	private static void getContent(AtomicReference<ModelLanguage> containerModelLanguage, Stream<String> lines, List<String> modelSpecifications, Map<String, List<String>> fragments) {
		StringBuilder currentFragment = new StringBuilder();
		lines.forEachOrdered(line -> {
			if (line.startsWith(MODEL_LANGUAGE_PREFIX)) {
				String languageCode = line.substring(MODEL_LANGUAGE_PREFIX.length(), line.length()).trim();
				containerModelLanguage.set(ModelLanguage.getModelLangageForCode(languageCode));
			}
			else if (line.startsWith(MODEL_SPECIFICATION_PREFIX)) {
				modelSpecifications.add(line);
			}
			else if (line.startsWith(MODEL_FRAGMENT_PREFIX)) {
				currentFragment.setLength(0);
				currentFragment.append(line.substring(MODEL_FRAGMENT_PREFIX.length(), line.length()).trim());
				fragments.put(currentFragment.toString(), new ArrayList<>());
			}
			else {
				if (currentFragment.length() == 0) {
					throw new RuntimeException("No current fragment identifier exists before :" +line);
				}
				
				fragments.get(currentFragment.toString()).add(line);
			}
		});
		
		if (containerModelLanguage.get() == null) {
			throw new RuntimeException("Container FactorNetwork Language Code is not specified: "+MODEL_LANGUAGE_PREFIX);
		}
	}
	
	private static void appendField(String fieldName, List<String> values, StringBuilder sb) {
		sb.append(fieldName);
		sb.append("=[");
		StringJoiner sjValues = new StringJoiner(",");
		values.forEach(v -> sjValues.add(v));
		sb.append(sjValues.toString());
		sb.append("]");
	}
	
	private static String extractField(String fieldName, String modelSpecification) {
		String result     = "";
		String fieldStart = fieldName+"=[";
		String fieldEnd   = "]";
		int s = modelSpecification.indexOf(fieldStart);
		if (s >= 0) {
			s = s + fieldStart.length();
			int e = modelSpecification.indexOf(fieldEnd, s);
			
			result = modelSpecification.substring(s, e);
		}
				
		return result;
	}
	
	private static String extractModel(String fieldName, String modelSpecification, Map<String, List<String>> fragments) {
		StringJoiner result = new StringJoiner("\n");
		
		String names = extractField(fieldName, modelSpecification).trim();
		if (names.length() > 0) {
			String[] partNames = names.split(",");
			for (String partName : partNames) {
				if (!fragments.containsKey(partName)) {
					throw new RuntimeException("Unable to identify fragment: ["+partName+"]");
				}
				for (String line : fragments.get(partName)) {
					result.add(line);
				}
			}
		}
		
		return result.toString();
	}
	
	private static List<String> extractQueries(String fieldName, String modelSpecification, Map<String, List<String>> fragments) {
		List<String> result = new ArrayList<>();
		 
		String names = extractField(fieldName, modelSpecification).trim();
		if (names.length() > 0) {
			String[] queryNames = names.split(",");
			for (String queryName : queryNames) {
				if (!fragments.containsKey(queryName)) {
					throw new RuntimeException("Unable to identify fragment: "+queryName);
				}
				StringJoiner query = new StringJoiner(" and ");
				for (String queryFragment : fragments.get(queryName)) {
					if (queryFragment.trim().length() > 0) {
						query.add(queryFragment.trim());
					}
				}
				result.add(query.toString());
			}
		}
		
		return result;
	}
}
