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
package com.sri.ai.praise.application.praise.app.perspective;

import java.util.Arrays;
import java.util.List;

import javafx.fxml.FXMLLoader;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.application.praise.app.FXUtil;
import com.sri.ai.praise.application.praise.app.editor.HOGMPageEditorController;
import com.sri.ai.praise.application.praise.app.editor.ModelPageEditor;
import com.sri.ai.praise.application.praise.app.model.EarthquakeBurglaryAlarm;
import com.sri.ai.praise.application.praise.app.model.Election;
import com.sri.ai.praise.application.praise.app.model.ElectionAsInIJCAI2016Paper;
import com.sri.ai.praise.application.praise.app.model.ExamplePages;
import com.sri.ai.praise.application.praise.app.model.MontyHallProblem;
import com.sri.ai.praise.application.praise.app.model.Position;
import com.sri.ai.praise.lang.ModelLanguage;

@Beta
public class HOGMPerspective extends AbstractPerspective {
	
	//
	// START-Perspective
	@Override
	public List<ExamplePages> getExamples() {
		return Arrays.asList(
				new EarthquakeBurglaryAlarm(), 
				new MontyHallProblem(),
				new Election(), 
				new ElectionAsInIJCAI2016Paper(),
				new Position());
	}
	// END-Perspective
	//
	
	@Override
	protected ModelLanguage getModelLanguage() {
		return ModelLanguage.HOGMv1;
	}
	
	@Override
	protected ModelPageEditor create(String model, List<String> defaultQueries) {
		ModelPageEditor result = null;
		FXMLLoader      loader = HOGMPageEditorController.newLoader();
		try {
			loader.load();
			result = loader.getController();
			result.setPage(model, defaultQueries);
		}
		catch (Throwable t) {
			FXUtil.exception(t);
		}
		
		return result;
	}
	
}
