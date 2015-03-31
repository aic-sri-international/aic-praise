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

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;
import java.util.stream.Stream;

import com.google.common.annotations.Beta;

@Beta
public class ExamplePages {
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
		List<ExamplePage> result = new ArrayList<>();
		
		List<String> modelSpecifications = new ArrayList<>();
		Map<String, List<String>> fragments = new HashMap<>();
		StringBuilder currentFragment = new StringBuilder();
		try (Stream<String> lines = Files.lines(Paths.get(ExamplePages.class.getResource(resourceName).toURI()))) {	
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
		} catch (Exception ex) {
			ex.printStackTrace();
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
		
		String[] partNames = extractField(fieldName, modelSpecification).split(",");
		for (String partName : partNames) {
			if (!fragments.containsKey(partName)) {
				throw new RuntimeException("Unable to identify fragment: "+partName);
			}
			for (String line : fragments.get(partName)) {
				result.add(line);
			}
		}
		
		return result.toString();
	}
	
	private static List<String> extractQueries(String fieldName, String modelSpecification, Map<String, List<String>> fragments) {
		List<String> result = new ArrayList<>();
		
		String[] queryNames = extractField(fieldName, modelSpecification).split(",");
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
		
		return result;
	}
}