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
package com.sri.ai.praise.linkeddata.jsonld;

import static com.sri.ai.util.Util.map;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import com.github.jsonldjava.core.JsonLdError;
import com.github.jsonldjava.core.JsonLdOptions;
import com.github.jsonldjava.core.JsonLdProcessor;
import com.sri.ai.praise.linkeddata.api.LinkedDataObject;
import com.sri.ai.util.Util;

public class JSONLDJavaUtil {

	@SuppressWarnings("unchecked")
	public static LinkedDataObject make(Object jsonldJavaObject, Function<String, Object> objectIndex) {
		LinkedDataObject result;
		if (jsonldJavaObject == null) {
			result = null;
		}
		else if (jsonldJavaObject instanceof String) {
			result = new JSONLDString((String) jsonldJavaObject);
		}
		else if (jsonldJavaObject instanceof Number) {
			result = new JSONLDNumber((Number) jsonldJavaObject);
		}
		else if (jsonldJavaObject instanceof Boolean) {
			result = new JSONLDBoolean((Boolean) jsonldJavaObject);
		}
		else if (jsonldJavaObject instanceof List) {
			result = new JSONLDList((List<Object>) jsonldJavaObject, objectIndex);
		}
		else if (jsonldJavaObject instanceof Map) {
			result = getObjectFromMap(jsonldJavaObject, objectIndex);
		}
		else {
			throw new Error("Unrecognized JSONLD-JAVA object of class " + jsonldJavaObject.getClass() + ": " + jsonldJavaObject);
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	private static LinkedDataObject getObjectFromMap(Object jsonldJavaObject, Function<String, Object> objectIndex) {
		LinkedDataObject result;
		Object jsonldJavaObjectToUse = determineJSONLDObjectToUse(jsonldJavaObject, objectIndex);
		result = new JSONLDMap((Map<String, Object>) jsonldJavaObjectToUse, objectIndex);
		return result;
	}

	private static Object determineJSONLDObjectToUse(Object jsonldJavaObject, Function<String, Object> objectIndex) {
		Object jsonldJavaObjectToUse;
		String idIfThisIsIdOnly = getIdIfThisIsIdOnly(jsonldJavaObject);
		if (idIfThisIsIdOnly != null) {
			jsonldJavaObjectToUse = objectIndex.apply(idIfThisIsIdOnly);
		}
		else {
			jsonldJavaObjectToUse = jsonldJavaObject;
		}
		return jsonldJavaObjectToUse;
	}
	
	private static String getIdIfThisIsIdOnly(Object jsonldJavaObject) {
		String result;
		if (hasSingleEntry(jsonldJavaObject)) {
			String id = getIdIfAny(jsonldJavaObject);
			if (id != null) {
				result = id;
			}
			else {
				result = null;
			}
		}
		else {
			result = null;
		}
		return result;
	}

	private static boolean hasSingleEntry(Object jsonldJavaObject) {
		boolean result = ((Map)jsonldJavaObject).size() == 1;
		return result;
	}

	private static String getIdIfAny(Object jsonldJavaObject) {
		String result = (String) ((Map)jsonldJavaObject).get("@id");
		return result;
	}

	/**
	 * Returns a {@link LinkedDataObject} corresponding to the root objects of a JSONLD-JAVA object.
	 * @param jsonldJavaObject
	 * @return
	 */
	public static LinkedDataObject getRootLinkedDataObject(Object jsonldJavaObject) {
		Object flattenedExpanded = getFlattenedExpandedJSONLDJavaObject(jsonldJavaObject);
		Object root = getRoot(flattenedExpanded);
		Map<String, Object> objectIndex = makeObjectIndex(flattenedExpanded);
		Util.println("Object index:\n" + Util.join("\n", objectIndex.entrySet()));
		LinkedDataObject result = make(root, objectIndex::get);
		return result;
	}

	private static Object getFlattenedExpandedJSONLDJavaObject(Object jsonldJavaObject) throws Error {
		Map context = new HashMap();
		JsonLdOptions options = new JsonLdOptions();
		Object flattened;
		try {
			Object expanded = JsonLdProcessor.expand(jsonldJavaObject, options);
			flattened = JsonLdProcessor.flatten(expanded, context, options);
		} catch (JsonLdError e) {
			throw new Error(e);
		}
		return flattened;
	}

	private static Object getRoot(Object flattenedExpanded) {
		List graph = getGraph(flattenedExpanded);
		Object root = graph.get(0);
		return root;
	}

	private static List getGraph(Object flattenedExpanded) {
		List result = (List) ((Map) flattenedExpanded).get("@graph");
		return result;
	}

	private static Map<String, Object> makeObjectIndex(Object flattenedExpanded) {
		Map<String, Object> objectIndex = map();
		for (Object node : getGraph(flattenedExpanded)) {
			Map nodeMap = (Map) node;
			String id = (String) nodeMap.get("@id");
			Map nodeAlreadyInIndex = (Map) objectIndex.get(id);
			if (ifNewNodeMoreInformativeThan(nodeMap, nodeAlreadyInIndex)) {
				objectIndex.put(id, nodeMap);
			}
		}
		return objectIndex;
	}

	private static boolean ifNewNodeMoreInformativeThan(Map nodeMap, Map nodeAlreadyInIndex) {
		return nodeAlreadyInIndex == null || nodeAlreadyInIndex.size() < nodeMap.size();
	}
}
