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
package com.sri.ai.praise.sgsolver.demo.model;

import java.io.File;
import java.net.URI;
import java.nio.charset.Charset;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;
import java.util.stream.Stream;

import com.google.common.annotations.Beta;
import com.google.common.base.Charsets;
import com.sri.ai.praise.sgsolver.demo.FXUtil;
import com.sri.ai.util.base.Pair;

@Beta
public class ExamplePages {
	public static final Charset FILE_CHARSET = Charsets.UTF_8;
	//
	private static final String MODEL_FRAGMENT_PREFIX      = "@FRAGMENT:";
	private static final String MODEL_SPECIFICATION_PREFIX = "@MODEL:";
	private static final String MODEL_FIELD_NAME           = "name";
	private static final String MODEL_FIELD_PARTS          = "parts";
	private static final String MODEL_FIELD_QUERIES        = "queries";
	//
	private final String name;
	private final List<ExamplePage> pages;
	
	public ExamplePages(String name, List<ExamplePage> pages) {
		this.name = name;
		this.pages = Collections.unmodifiableList(new ArrayList<>(pages));
	}

	public String getName() {
		return name;
	}

	public List<ExamplePage> getPages() {
		return pages;
	}
	
	@Override
	public String toString() {
		return name;
	}
	
	public static List<ExamplePage> getExamplePagesFromResource(String resourceName) {
		List<ExamplePage> result = null;
		try {
			result = getExamplePagesFromURI(ExamplePages.class.getResource(resourceName).toURI());
		}
		catch (Throwable t) {
			FXUtil.exception(t);
		}
		return result;
	}
	
	public static String getModel(List<Pair<String, List<String>>> pageContents) {
		StringBuilder result = new StringBuilder();
		
		// Output the models first
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
	
	public static List<ExamplePage> getExamplePagesFromFile(File file) {
		List<ExamplePage> result = null;
		try {
			result = getExamplePagesFromURI(file.toURI());
		}
		catch (Throwable t) {
			FXUtil.exception(t);
		}
		return result;
	}
		
	public static List<ExamplePage> getExamplePagesFromURI(URI uri) {	
		List<ExamplePage> result = new ArrayList<>();
		
		List<String> modelSpecifications = new ArrayList<>();
		Map<String, List<String>> fragments = new HashMap<>();
		
		// Need to do this if reading form jar file.
		if (uri.toString().contains("!")) {
			final Map<String, String> env = new HashMap<>();
			final String[] array = uri.toString().split("!");
			try (FileSystem fs  = FileSystems.newFileSystem(URI.create(array[0]), env)) {
				Path path = fs.getPath(array[1]);
				try (Stream<String> lines = Files.lines(path, FILE_CHARSET)) {	
					getContent(lines, modelSpecifications, fragments);	
				}
			} catch (Exception ex) {
				FXUtil.exception(ex);
			}
		}
		else {
			try (Stream<String> lines = Files.lines(Paths.get(uri), FILE_CHARSET)) {	
				getContent(lines, modelSpecifications, fragments);	
			} catch (Exception ex) {
				FXUtil.exception(ex);
			}
		}
		
		for (String modelSpecification : modelSpecifications) {
			String name          = extractField(MODEL_FIELD_NAME, modelSpecification);
			String model         = extractModel(MODEL_FIELD_PARTS, modelSpecification, fragments);
			List<String> queries = extractQueries(MODEL_FIELD_QUERIES, modelSpecification, fragments);
			
			result.add(new ExamplePage(name, model, queries));
		}
		
		return result;
	}
	
	//
	// PRIVATE
	//
	private static void getContent(Stream<String> lines, List<String> modelSpecifications, Map<String, List<String>> fragments) {
		StringBuilder currentFragment = new StringBuilder();
		lines.forEachOrdered(line -> {
			if (line.startsWith(MODEL_SPECIFICATION_PREFIX)) {
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
