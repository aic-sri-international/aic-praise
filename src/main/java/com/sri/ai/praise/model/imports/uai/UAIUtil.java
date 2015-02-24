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
package com.sri.ai.praise.model.imports.uai;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.DPLLGeneralizedAndSymbolic;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.EqualityTheory;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.Max;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.ProblemType;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.SymbolTermTheory;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.Theory;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.CartesianProductEnumeration;

/**
 * UAI utilities specific to UAI file format processing.
 * 
 * @author oreilly
 */
@Beta
public class UAIUtil {
	public static String readLine(BufferedReader br) throws IOException {
		String result = "";
		// This ensures empty lines are removed
		while (result.equals("")) {
			result = br.readLine().trim();
		}
		return result;
	}
	
	public static String[] split(String line) {
		return line.split("\\s+");
	}
	
	public static Expression constructTableExpression(FunctionTable ft) {
		Expression result = null;
		
		Double sum = ft.getEntries().stream().reduce((e1, e2) -> e1 + e2).get();
		
		StringBuilder e = new StringBuilder();
		CartesianProductEnumeration<Integer> cartProd = new CartesianProductEnumeration<>(cardinalityValues(ft));
		int cnt = 0;
		while (cartProd.hasMoreElements()) {
			cnt++;
			List<Integer> values = cartProd.nextElement();
			Double normalizedEntryValue = ft.entryFor(values) / sum;
			if (cnt == cartProd.size().intValue()) {
				// i.e. final value
				e.append(normalizedEntryValue);
			}
			else {
				e.append("if ");
				for (int i = 0; i < values.size(); i++) {
					if (i > 0) {
						e.append(" and ");
					}
					e.append("V");
					e.append(i);
					e.append(" = ");
					e.append("consV");
					e.append(i);
					e.append("_");
					e.append(values.get(i));
				}
				e.append(" then ");
				e.append(normalizedEntryValue);
				e.append(" else ");
			}
		}
		
		// The theory of equality on symbols (includes a model counter for formulas in it).
		Theory      theory      = new EqualityTheory(new SymbolTermTheory());
		ProblemType problemType = new Max(); // the problem type actually does not matter, because we are not going to have any indices.
		Expression  table       = Expressions.parse(e.toString());
		
		Collection<Expression> indices = Util.list(); // no indices; we want to keep all variables
		Map<String, String> mapFromTypeNameToSizeString   = new LinkedHashMap<>();
		Map<String, String> mapFromVariableNameToTypeName = new LinkedHashMap<>();
		for (int i = 0; i < ft.numberVariables(); i++) {
			String typeName = "V"+i+"SIZE"; ;
			mapFromTypeNameToSizeString.put(typeName, ""+ft.cardinality(i));
			mapFromVariableNameToTypeName.put("V"+i, typeName);
		}
		
		/* The solver for the parameters above. */
		DPLLGeneralizedAndSymbolic solver = new DPLLGeneralizedAndSymbolic(theory, problemType);
		
		/* Solve the problem. */
		result = solver.solve(table, indices, mapFromVariableNameToTypeName, mapFromTypeNameToSizeString);

System.out.println("Original table is:\n"+e);		
System.out.println("Decision tree for table is:\n" + result);	

		
		return result;
	}
	
	public static List<List<Integer>> cardinalityValues(FunctionTable ft) {
		List<List<Integer>> result = new ArrayList<>();
		
		for (int v = 0; v < ft.numberVariables(); v++) {
			List<Integer> vals = new ArrayList<>();
			for (int c = 0; c < ft.cardinality(v); c++) {
				vals.add(c);
			}
			result.add(vals);
		}
		
		return result;
	}
}
