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
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Pagination;
import javafx.scene.control.Tooltip;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.sgsolver.demo.model.ExamplePages;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcons;

@Beta
public class SGSolverDemoController {
	//
	@FXML private Button openMenuButton;
	//
	@FXML private Button newButton;
	@FXML private Button openFileButton;
	@FXML private Button saveButton;
	//
	@FXML private ComboBox<ExamplePages> examplesComboBox;
	//
	@FXML private Button addPageButton;
	@FXML private Button removePageButton;
	//
	@FXML private Tooltip undoTooltip;
	@FXML private Tooltip redoTooltip;
	//
	@FXML private Pagination modelPagination;
	
	//
	private Perspective perspective;
	private Map<Integer, ModelEditor> modelPages = new HashMap<>();
	
	//
	// PRIVATE
	//
	@FXML
	private void exampleSelected(ActionEvent ae) {
// TODO - reset up
//		SGExample eg = examplesComboBox.getValue();
//		this.activeModelEditor.setExample(eg);
		
		
// TODO - reset up
//		evidencePagination.setPageCount(0);
//		evidenceCodeAreas.clear();
//		
//		List<StringBuilder> modelParts= new ArrayList<>();
//		StringBuilder modelPart = new StringBuilder();
//		modelParts.add(modelPart);
//		try (BufferedReader br = new BufferedReader(new StringReader(example.getModel()))) {
//			String line;
//			while ((line = br.readLine()) != null) {
//				if (line.startsWith(EVIDENCE_SCENARIO_MARKER_PREFIX)) {
//					modelPart = new StringBuilder();
//					modelParts.add(modelPart);
//				}
//				else {
//					modelPart.append(line);
//					modelPart.append("\n");
//				}
//			}
//		} catch (Exception ex) {
//			// ignore
//		}
//		
//		modelCodeArea.setText(modelParts.get(0).toString());
//		if (modelParts.size() == 1) {
//			evidencePagination.setPageCount(1); // i.e. default evidence page
//		}
//		else {
//			for (int i = 1; i < modelParts.size(); i++) {
//				HOGMCodeArea evidenceCodeArea = new HOGMCodeArea();
//				evidenceCodeArea.setText(modelParts.get(i).toString());
//				evidenceCodeAreas.put(i-1, evidenceCodeArea);
//			}
//			evidencePagination.setPageCount(modelParts.size()-1);
//		}

// TODO - notify query controller for active page
//		if (eg.getDefaultQueriesToRun().size() > 0) {
//			eg.getDefaultQueriesToRun().forEach(query -> {
//				if (!queryComboBox.getItems().contains(query)) {
//					queryComboBox.getItems().add(query);
//				}
//			});
//			queryComboBox.setValue(eg.getDefaultQueriesToRun().get(0));
//		}
	}

    @FXML
    private void initialize() throws IOException {
    	FXUtil.setDefaultButtonIcon(openMenuButton, FontAwesomeIcons.BARS);
    	//
    	FXUtil.setDefaultButtonIcon(newButton, FontAwesomeIcons.FILE_ALT);
    	FXUtil.setDefaultButtonIcon(openFileButton, FontAwesomeIcons.FOLDER_OPEN);
    	FXUtil.setDefaultButtonIcon(saveButton, FontAwesomeIcons.SAVE);
    	//
    	FXUtil.setDefaultButtonIcon(addPageButton, FontAwesomeIcons.PLUS);
    	FXUtil.setDefaultButtonIcon(removePageButton, FontAwesomeIcons.MINUS);
  	
		modelPagination.pageCountProperty().addListener(new ChangeListener<Number>() {
			public void changed(ObservableValue<? extends Number> observable, Number oldValue, Number newValue) {				
				if (newValue.intValue() <= 1) {
					removePageButton.setDisable(true);
				}
				else {
					removePageButton.setDisable(false);
				}
			}
		});
		
		modelPagination.setPageFactory(this::createModelPage);
		
		setPerspective(new HOGMPerspective());
    }
    
    private void setPerspective(Perspective perspective) {
    	this.perspective = perspective;
// TODO - wire up examples.   	
    	modelPagination.setPageCount(1);
    }
    
 	private Node createModelPage(Integer pgIndex) {	
 		ModelEditor modelEditor = modelPages.get(pgIndex);
 		if (modelEditor == null) {
 			try {
 				modelEditor = perspective.create("", Collections.emptyList()); 
 				modelPages.put(pgIndex, modelEditor);
 			}
 			catch (IOException ioe) {
 // TODO - handle properly
 				ioe.printStackTrace();				
 			}
 		}
 		
 		return modelEditor.getRootPane();
 	}
    
    
 // TODO - rewrire up to work at top level	
// 	@FXML
// 	private void addEvidencePage(ActionEvent ae) {
// 		Integer currentPageIdx = evidencePagination.getCurrentPageIndex();
// 		
// 		Map<Integer, HOGMCodeArea> newEvidenceCodeAreaPageIdxs = new HashMap<>();
// 		evidenceCodeAreas.entrySet().forEach(e -> {
// 			if (e.getKey() > currentPageIdx) {
// 				newEvidenceCodeAreaPageIdxs.put(e.getKey()+1, e.getValue());
// 			}
// 			else {
// 				newEvidenceCodeAreaPageIdxs.put(e.getKey(), e.getValue());
// 			}
// 		});
// 		evidenceCodeAreas.clear();
// 		evidenceCodeAreas.putAll(newEvidenceCodeAreaPageIdxs);
// 		
// 		evidencePagination.setPageCount(evidencePagination.getPageCount()+1);
// 		evidencePagination.setCurrentPageIndex(currentPageIdx+1);
// 	}
 //	
// 	@FXML
// 	private void removeEvidencePage(ActionEvent ae) {
// 		Integer currentPageIdx = evidencePagination.getCurrentPageIndex();
// 		evidenceCodeAreas.remove(currentPageIdx);
// 		Map<Integer, HOGMCodeArea> newEvidenceCodeAreaPageIdxs = new HashMap<>();
// 		evidenceCodeAreas.entrySet().forEach(e -> {
// 			if (e.getKey() > currentPageIdx) {
// 				newEvidenceCodeAreaPageIdxs.put(e.getKey()-1, e.getValue());
// 			}
// 			else {
// 				newEvidenceCodeAreaPageIdxs.put(e.getKey(), e.getValue());
// 			}
// 		});
// 		evidenceCodeAreas.clear();
// 		evidenceCodeAreas.putAll(newEvidenceCodeAreaPageIdxs);	
// 		// Reduce the # of pages
// 		evidencePagination.setPageCount(evidencePagination.getPageCount()-1);
// 		
// 		if (currentPageIdx < evidencePagination.getPageCount()) {
// 			evidencePagination.setCurrentPageIndex(currentPageIdx);
// 		}
// 		else {
// 			evidencePagination.setCurrentPageIndex(evidencePagination.getPageCount()-1);
// 		}
// 	}
 //	
 
// TODO - rework how this gets setup    
//    private void setHOGMPerspective() throws IOException {
//    	FXMLLoader editorLoader = new FXMLLoader(HOGMEditorController.class.getResource("hogmeditor.fxml"));
//    	Pane editorPane = editorLoader.load();
//    	FXUtil.anchor(editorPane);
//    	this.editorPane.getChildren().add(editorPane);
//    	
//    	setEditorPerspective(editorLoader.getController());
//    }
//    
//    private void setEditorPerspective(ModelEditor modelEditor) {
//    	this.activeModelEditor = modelEditor;
//    	
//    	// Set up the examples
//    	examplesComboBox.getItems().clear();
//    	modelEditor.getExamples().forEach(eg -> examplesComboBox.getItems().add(eg));
//    	examplesComboBox.setValue(modelEditor.getExamples().get(0));
//    }
}
