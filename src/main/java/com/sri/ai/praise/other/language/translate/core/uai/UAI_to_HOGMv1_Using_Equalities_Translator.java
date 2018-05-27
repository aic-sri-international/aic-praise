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
package com.sri.ai.praise.other.language.translate.core.uai;

import java.util.List;
import java.util.StringJoiner;
import java.util.stream.IntStream;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.model.core.hogm.components.HOGMSortDeclaration;
import com.sri.ai.praise.core.model.core.uai.core.UAIUtil;
import com.sri.ai.praise.core.model.core.uai.core.data.FunctionTable;

/**
 * Translator: UAI->HOGMv1 using equalities
 * 
 * @author oreilly
 *
 */
@Beta
public class UAI_to_HOGMv1_Using_Equalities_Translator extends AbstractUAI_to_HOGMv1_Translator {

	@Override
	public void addSortAndRandomVariableDeclarationsRegarding(int varIdx, int varCardinality, List<String> sorts, List<String> randoms) {
		String varName     = UAIUtil.instanceVariableName(varIdx);
		String varTypeName = UAIUtil.instanceTypeNameForVariable(varIdx, varCardinality);
		
		StringJoiner sortConstants = new StringJoiner(", ", ", ", ";");
		final int innerVarIdx = varIdx;
		IntStream.range(0, varCardinality).forEach(valIdx -> {
			sortConstants.add(UAIUtil.instanceConstantValueForVariable(valIdx, innerVarIdx, varCardinality));
		});
		if (!HOGMSortDeclaration.IN_BUILT_BOOLEAN.getName().equals(varTypeName)) {
			sorts.add("sort "+varTypeName+": "+varCardinality+sortConstants.toString());
		}
		randoms.add("random "+varName+": "+varTypeName+";");
	}

	@Override
	public Expression convertToHOGMv1Expression(FunctionTable table) {
		Expression result = UAIUtil.constructGenericTableExpressionUsingEqualities(table);
		return result;
	}
}