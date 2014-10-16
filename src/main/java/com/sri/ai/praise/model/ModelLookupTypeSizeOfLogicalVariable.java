/*
 * Copyright (c) 2013, SRI International
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
package com.sri.ai.praise.model;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityOfType;
import com.sri.ai.praise.PRAiSEConfiguration;

/**
 * Implementation of
 * CardinalityTypeOfLogicalVariable.TypeSizeOfLogicalVariable that looks up
 * the model in order to determine the type size of a logical variable.
 * 
 * @author oreilly
 * 
 */
@Beta
public class ModelLookupTypeSizeOfLogicalVariable implements
		CardinalityOfType.TypeSizeOfSymbolOrType {

	private boolean lookupSizes = PRAiSEConfiguration.isAllTypeSizesKnownInModel();
	private Model   model       = null;
	private Integer defaultSize = null;
	
	public ModelLookupTypeSizeOfLogicalVariable(Model model) {
		this.model = model;
	}
	
	//
	// START-TypeSizeOfLogicalVariable
	@Override
	public Integer getSize(Expression logicalVariable, RewritingProcess process) {
		Integer result = null;
		if (lookupSizes) {
			if (defaultSize == null) {
				defaultSize = PRAiSEConfiguration.getModelDefaultSizeOfAllTypes();
			}
			// Assume the default size initially
			Integer size = defaultSize;
			
			// Now determine what sort the logical variable belongs to.
			// Start with the Universe of discourse initially.
			Expression sortNameForLogicalVariable = SortDeclaration.UNIVERSE_OF_DISCOURSE;

			// TODO - add support for when there is more than 1 sort defined in the model, i.e:
			// ALBP-199 Retrieve type information associated with logical variables from models 
			// with multiple types
			if (model.getSortDeclarations().size() == 1) {
				SortDeclaration sortDeclaration = model.getSortDeclarations().get(0);
				sortNameForLogicalVariable = sortDeclaration.getName();
				
				size = possiblyUpdateSize(size, sortDeclaration.getSize());
			}
			
			// Now see if the user has explicitly overridden the size of the sort 
			// via the process, i.e.: | <sortName> | = some value.			
			size = lookupAndPossiblyUpdateSize(size, 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.CARDINALITY, sortNameForLogicalVariable),
					process);
			
			
			// Finally the user may more finely override the size to be associated
			// with the logical variable by declaring: | type(<logicalVariable>) | = some value
			// in the process.
			size = lookupAndPossiblyUpdateSize(size, 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.CARDINALITY, 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(CardinalityOfType.TYPE_LABEL, logicalVariable)),
					process); 
			
			// Note: < 0 implies its infinity and therefore unknown
			if (size >= 0) {
				result = size;	
			}
		}
		return result;
	}
	// END-TypeSizeOfLogicalVariable
	//
	
	public Model getModel() {
		return model;
	}
	
	//
	// PRIVATE METHODS
	//
	private Integer possiblyUpdateSize(Integer currentSize, Expression possibleNewSize) {
		Integer result = currentSize;
		
		if (null != possibleNewSize) {
			try {
				// Note: This may not be an integer (i.e. 'Unknown' for a sort or not specified at all)
				// if so we use the default size by ignoring the exception thrown.
				result = possibleNewSize.intValueExact(); 
			} catch (Throwable t) {
				// ignore as can happen if size on sort declaration is unknown.
			}
		}
		
		return result;
	}
	
	private Integer lookupAndPossiblyUpdateSize(Integer currentSize, Expression globalObjectsKey, RewritingProcess process) {
		Integer result = currentSize;
				
		Object value;
		if ((value = process.getGlobalObject(globalObjectsKey)) != null) {
			result = possiblyUpdateSize(currentSize, Expressions.wrap(value));
		}
		
		return result;
	}
}
