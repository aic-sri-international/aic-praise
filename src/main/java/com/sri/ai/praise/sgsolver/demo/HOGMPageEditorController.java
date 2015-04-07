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
package com.sri.ai.praise.sgsolver.demo;

import java.io.IOException;
import java.util.List;

import org.fxmisc.undo.UndoManager;

import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.Pane;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.sgsolver.demo.editor.HOGMCodeArea;

@Beta
public class HOGMPageEditorController implements ModelPageEditor {
	@FXML private Pane       rootPane;
	@FXML private AnchorPane modelEditorPane;
	@FXML private AnchorPane queryOutputPane;
	//
	private HOGMCodeArea    modelCodeArea = new HOGMCodeArea();
	private QueryController queryController;
	
	public static FXMLLoader newLoader( ) {
		FXMLLoader result = new FXMLLoader(HOGMPageEditorController.class.getResource("hogmpageeditor.fxml"));
		return result;
	}
	
	//
	// START-ModelPageEditor
	@Override
	public Pane getRootPane() {
		return rootPane;
	}
	
	@Override
	public void setPage(String modelPage, List<String> defaultQueries) {
		modelCodeArea.setText(modelPage);
		queryController.addDefaultQueries(defaultQueries);		
	}
	
	@Override
	public String getCurrentPageContents() {
		return modelCodeArea.getText();
	}
	
	@Override
	public List<String> getCurrentQueries() {
		return queryController.getCurrentQueries();
	}
	
	@Override
	public UndoManager getUndoManager() {
		return modelCodeArea.getUndoManager();
	}
	
	@Override
	public void undo() {
		modelCodeArea.undo();
	}
	
	@Override
	public void redo() {
		modelCodeArea.redo();
	}
	
	@Override
	public void highlight(int startIdx, int endIdx) {
		modelCodeArea.highlight(startIdx, endIdx);
	}
	
	@Override
	public void gotoModelEditor() {
		modelCodeArea.setFocus();
	}
	
	@Override
	public void gotoQueryEditor() {
		queryController.gotoQueryEditor();
	}
	
	@Override 
	public void executeQuery() {
		queryController.executeQuery();
	}
	// END-ModelPageEditor
	//
	
	@FXML
	private void initialize() throws IOException {
		FXUtil.anchor(modelCodeArea);
		modelEditorPane.getChildren().add(modelCodeArea);
		
		FXMLLoader queryLoader = QueryController.newLoader();
		Pane queryPane  = queryLoader.load();
		queryController = queryLoader.getController();
		queryController.setModelPageEditor(this);
		FXUtil.anchor(queryPane);
		queryOutputPane.getChildren().add(queryPane);
	}
}
