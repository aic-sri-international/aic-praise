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
package com.sri.ai.praise.other.application.praise.app.perspective;

import java.io.File;
import java.util.List;
import java.util.function.Supplier;

import javafx.beans.property.IntegerProperty;
import javafx.beans.property.ReadOnlyBooleanProperty;
import javafx.beans.property.ReadOnlyMapProperty;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.collections.ObservableMap;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.other.application.praise.app.editor.ModelPageEditor;
import com.sri.ai.praise.other.application.praise.app.model.ExamplePages;

@Beta
public interface Perspective {	
	void setCurrentModelPageIndexProperty(IntegerProperty currentModelPageIndexProperty);
	
	List<ExamplePages> getExamples();
	
	boolean isCanUndoModelPageEdit();
	ReadOnlyBooleanProperty canUndoModelPageEditProperty(); 
	boolean isCanRedoModelPageEdit();
	ReadOnlyBooleanProperty canRedoModelPageEditProperty();
	
	boolean isCanUndoPageChange();
	ReadOnlyBooleanProperty canUndoPageChange(); 
	boolean isCanRedoPageChange();
	ReadOnlyBooleanProperty canRedoPageChange();
	
	void undoPageChange();
	void redoPageChange();

	ObservableMap<Integer, Supplier<ModelPageEditor>> getModelPageEditors();
	ReadOnlyMapProperty<Integer, Supplier<ModelPageEditor>> modelPageEditorsProperty();	
	
	void newModel(String contents, List<String> defaultQueries);
	void newModel(File modelFile);
	void newModel(ExamplePages examples);
	
	void addPage(Integer atPageIndex);
	void removePage(Integer pageIndex);
	
	File getModelFile();
	ReadOnlyObjectProperty<File> modelFileProperty();
	
	boolean isSaveRequired();
	ReadOnlyBooleanProperty saveRequiredProperty();
	
	void save();
	void saveAs(File file);
	
	void gotoModelEditor();
	void gotoQueryEditor();
	void executeQuery();
}
