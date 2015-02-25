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
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.SyntacticSubstitute;
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
		while ("".equals(result)) {
			result = br.readLine();
			if (result != null) {
				result = result.trim();
			}
		}
		return result;
	}
	
	public static String[] split(String line) {
		return line.split("\\s+");
	}
	
	public static Expression constructGenericTableExpression(FunctionTable functionTable) {
		StringBuilder table = new StringBuilder();
		CartesianProductEnumeration<Integer> cartesianProduct = new CartesianProductEnumeration<>(cardinalityValues(functionTable));
		int cnt = 0;
		while (cartesianProduct.hasMoreElements()) {
			cnt++;
			List<Integer> values = cartesianProduct.nextElement();
			Double entryValue = functionTable.entryFor(values);
			if (cnt == cartesianProduct.size().intValue()) {
				// i.e. final value
				table.append(entryValue);
			}
			else {
				table.append("if ");
				for (int i = 0; i < values.size(); i++) {
					if (i > 0) {
						table.append(" and ");
					}
					table.append(genericVariableName(i));
					table.append(" = ");
					table.append(genericConstantValueForVariable(values.get(i), i, functionTable.cardinality(i)));
				}
				table.append(" then ");
				table.append(entryValue);
				table.append(" else ");
			}
		}
		
		// The theory of equality on symbols (includes a model counter for formulas in it).
		Theory      theory      = new EqualityTheory(new SymbolTermTheory());
		ProblemType problemType = new Max(); // the problem type actually does not matter, because we are not going to have any indices.
		Expression  tableExpr   = Expressions.parse(table.toString());
		
		Collection<Expression> indices = Util.list(); // no indices; we want to keep all variables
		Map<String, String> mapFromTypeNameToSizeString   = new LinkedHashMap<>();
		Map<String, String> mapFromVariableNameToTypeName = new LinkedHashMap<>();
		for (int i = 0; i < functionTable.numberVariables(); i++) {
			String typeName = genericTypeNameForVariable(i, functionTable.cardinality(i));
			mapFromTypeNameToSizeString.put(typeName, ""+functionTable.cardinality(i));
			mapFromVariableNameToTypeName.put(genericVariableName(i), typeName);
		}
		
		// The solver for the parameters above.
		DPLLGeneralizedAndSymbolic solver = new DPLLGeneralizedAndSymbolic(theory, problemType);
	
		// Solve the problem.
		Expression result = solver.solve(tableExpr, indices, mapFromVariableNameToTypeName, mapFromTypeNameToSizeString);	
		
		return result;
	}
	
	public static Expression convertGenericTableToInstance(FunctionTable functionTable, Expression genericFunctionTableExpr, List<Integer> instanceVarIdxs) {
		Expression result = genericFunctionTableExpr;
		RewritingProcess process = new DefaultRewritingProcess(null);
		for (int i = 0; i < functionTable.numberVariables(); i++) {
			// Replace the generic variable name with the correct instance name
			result = SyntacticSubstitute.replace(result, Expressions.makeSymbol(genericVariableName(i)), Expressions.makeSymbol(instanceVariableName(instanceVarIdxs.get(i))), process);
			int varCardinality = functionTable.cardinality(i);
			for (int c = 0; c < varCardinality; c++) {
				// Replace the generic constants with constants for the variable index (if they differ)
				Expression genericConstant  = Expressions.makeSymbol(genericConstantValueForVariable(c, i, varCardinality));
				Expression instanceConstant = Expressions.makeSymbol(instanceConstantValueForVariable(c, instanceVarIdxs.get(i), varCardinality));
				if (!genericConstant.equals(instanceConstant)) {
					result = SyntacticSubstitute.replace(result, genericConstant, instanceConstant, process);
				}
			}
		}
		
		return result;
	}
	
	public static String genericVariableName(int varIdx) {
		return "G"+varIdx;
	}
	
	public static String instanceVariableName(int varIdx) {
		return "V"+varIdx;
	}
	
	public static String genericConstantValueForVariable(int value, int variableIndex, int varCardinality) {
		if (varCardinality == 2) {
			if (value == 0) {
				return "false";
			}
			return "true";
		}
		return "cons"+genericVariableName(variableIndex)+"_"+value;
	}
	
	public static String instanceConstantValueForVariable(int value, int variableIndex, int varCardinality) {
		if (varCardinality == 2) {
			if (value == 0) {
				return "false";
			}
			return "true";
		}
		return "cons"+instanceVariableName(variableIndex)+"_"+value;
	}
	
	public static String genericTypeNameForVariable(int variableIndex, int varCardinality) {
		if (varCardinality == 2) {
			return "Boolean";
		}
		return "G"+variableIndex+"SIZE";
	}
	
	public static String instanceTypeNameForVariable(int variableIndex, int varCardinality) {
		if (varCardinality == 2) {
			return "Boolean";
		}
		return "V"+variableIndex+"SIZE";
	}
	
	public static List<List<Integer>> cardinalityValues(FunctionTable functionTable) {
		List<List<Integer>> result = new ArrayList<>();
		
		for (int v = 0; v < functionTable.numberVariables(); v++) {
			List<Integer> vals = new ArrayList<>();
			for (int c = 0; c < functionTable.cardinality(v); c++) {
				vals.add(c);
			}
			result.add(vals);
		}
		
		return result;
	}
}
