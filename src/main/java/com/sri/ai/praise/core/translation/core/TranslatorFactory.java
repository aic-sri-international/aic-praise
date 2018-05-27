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
package com.sri.ai.praise.core.translation.core;

import java.util.HashMap;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.core.model.classbased.api.ModelLanguage;
import com.sri.ai.praise.core.translation.api.Translator;
import com.sri.ai.praise.core.translation.core.hugin.UAI_to_HuginDotNet_Translator;
import com.sri.ai.praise.core.translation.core.uai.HOGMv1_to_UAI_Translator;
import com.sri.ai.praise.core.translation.core.uai.UAI_to_HOGMv1_Using_Inequalities_Translator;
import com.sri.ai.util.base.Pair;

/**
 * A factory class for constructing translators by name.
 * 
 * @author oreilly
 *
 */
@Beta
public class TranslatorFactory {
	public static final Map<Pair<ModelLanguage, ModelLanguage>, Class<?>> _translators = new HashMap<>();
	static {
		_translators.put(new Pair<>(ModelLanguage.HOGMv1, ModelLanguage.UAI), HOGMv1_to_UAI_Translator.class);
		//
		_translators.put(new Pair<>(ModelLanguage.UAI, ModelLanguage.HOGMv1),      UAI_to_HOGMv1_Using_Inequalities_Translator.class);
//		_translators.put(new Pair<>(ModelLanguage.UAI, ModelLanguage.HOGMv1),      UAI_to_HOGMv1_Using_Equalities_Translator.class);
		_translators.put(new Pair<>(ModelLanguage.UAI, ModelLanguage.HuginDotNet), UAI_to_HuginDotNet_Translator.class);
// TODO - implementations of:
// Church -> HOGMv1
// HOGMv1 -> PMTK3
// HOGMv1 -> HuginDotNet		
//
// PagedModelContainer -> Church
// PagedModelContainer -> HOGMv1
// Church              -> PagedModelContainer
// HOGMv1              -> PagedModelContainer		
	};
	
	/**
	 * Instantiate a new Translator based on the given source->target mapping.
	 * @param source
	 *        the source modeling language to translate from.
	 * @param target
	 *        the target modeling language to translate to.
	 * @return a new Translator instance capable of translating source->target
	 * @throws RuntimeException if unable to instantiate a translator for the given source and target.
	 */
	public static Translator newTranslator(ModelLanguage source, ModelLanguage target) {
		Translator result          = null;
		Class<?>   translatorClass = _translators.get(new Pair<ModelLanguage, ModelLanguage>(source, target));
		
		try {
			result = (Translator) translatorClass.newInstance();
		}
		catch (Exception ex) {
			throw new RuntimeException("Unable to create a new translator for "+source+"->"+target, ex);
		}
		
		return result;
	}
}