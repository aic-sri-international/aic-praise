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

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.Pagination;
import javafx.stage.FileChooser;
import javafx.stage.Stage;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.sgsolver.demo.model.ExamplePage;
import com.sri.ai.praise.sgsolver.demo.model.ExamplePages;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcons;

@Beta
public class SGSolverDemoController {
	private Stage mainStage;
	//
	@FXML private Button openMenuButton;
	//
	@FXML private Button newButton;
	@FXML private Button openFileButton;
	@FXML private Button saveButton;
	//
	@FXML private ComboBox<ExamplePages> examplesComboBox;
	//
	@FXML private Button undoButton;
	@FXML private Button redoButton;
	//
	@FXML private Pagination modelPagination;
	//
	@FXML private Button removePageButton;
	@FXML private Button previousPageButton;
	@FXML private Label  pageNofPLabel;
	@FXML private Button nextPageButton;
	@FXML private Button addPageButton;
	//
	private FileChooser fileChooser;
	
	//
	private Perspective perspective;
	private Map<Integer, ModelPage> modelPages = new HashMap<>();
	
	public void setMainStage(Stage stage) {
		this.mainStage = stage;
	}
	
	//
	// PRIVATE
	//
    @FXML
    private void initialize() throws IOException {
    	FXUtil.setDefaultButtonIcon(openMenuButton, FontAwesomeIcons.BARS);
    	//
    	FXUtil.setDefaultButtonIcon(newButton, FontAwesomeIcons.FILE_ALT);
    	FXUtil.setDefaultButtonIcon(openFileButton, FontAwesomeIcons.FOLDER_OPEN);
    	FXUtil.setDefaultButtonIcon(saveButton, FontAwesomeIcons.SAVE);
		//
    	FXUtil.setDefaultButtonIcon(undoButton, FontAwesomeIcons.ROTATE_LEFT);
    	FXUtil.setDefaultButtonIcon(redoButton, FontAwesomeIcons.ROTATE_RIGHT);
    	//
    	FXUtil.setPaginationButtonIcon(removePageButton, FontAwesomeIcons.MINUS);
    	FXUtil.setPaginationButtonIcon(previousPageButton, FontAwesomeIcons.CARET_LEFT);
    	FXUtil.setPaginationButtonIcon(nextPageButton, FontAwesomeIcons.CARET_RIGHT);
    	FXUtil.setPaginationButtonIcon(addPageButton, FontAwesomeIcons.PLUS);
    	//
    	//
    	fileChooser = new FileChooser();
    	fileChooser.getExtensionFilters().addAll(
    	         		new FileChooser.ExtensionFilter("Model Files", "*.sgmodel"));
    	//
    	//
    	modelPagination.setPageFactory(this::createModelPage);
		modelPagination.pageCountProperty().addListener(new ChangeListener<Number>() {
			public void changed(ObservableValue<? extends Number> observable, Number oldValue, Number newValue) {
				updatePaginationControls(modelPagination.getCurrentPageIndex(), newValue.intValue());
			}
		});
		modelPagination.currentPageIndexProperty().addListener(new ChangeListener<Number>() {
			public void changed(ObservableValue<? extends Number> observable, Number oldValue, Number newValue) {
				updatePaginationControls(newValue.intValue(), modelPagination.getPageCount());
			}
		});
		
		examplesComboBox.getSelectionModel().selectedIndexProperty().addListener(new ChangeListener<Number>() {
			public void changed(ObservableValue<? extends Number> observable, Number oldValue, Number newValue) {
				if (newValue.intValue() >= 0 && newValue.intValue() < examplesComboBox.getItems().size()) {
// TODO - check if current model should be saved first
					
					ExamplePages egPages = examplesComboBox.getItems().get(newValue.intValue());
									
					modelPages.clear();
					
					List<ExamplePage> pages = egPages.getPages();
					for (int i = 0; i < pages.size(); i++) {
						ExamplePage page = pages.get(i);
						modelPages.put(i, new ModelPage(() -> constructModelEditor(page.getModel(), page.getDefaultQueriesToRun())));
					}
			
					modelPagination.setPageCount(pages.size());
					modelPagination.setCurrentPageIndex(0);
				}
			}
		});
		
		setPerspective(new HOGMPerspective());
    }
    
    private void setPerspective(Perspective perspective) {
    	this.perspective = perspective;
    	
    	// Set up the examples
    	examplesComboBox.getItems().clear();    	
    	perspective.getExamples().forEach(eg -> examplesComboBox.getItems().add(eg));
    	examplesComboBox.getSelectionModel().selectFirst();
    }
   
    @FXML
    private void newModel(ActionEvent ae) {
 // TODO - check if current model should be saved first
    	
    	modelPages.clear();
    	
    	modelPagination.setPageCount(1);
		modelPagination.setCurrentPageIndex(0);
		
		// Want to indicate that we are not using a particular example after a new model is instantiated.
		examplesComboBox.getSelectionModel().select(-1);
    }
    
    @FXML
    private void openModel(ActionEvent ae) {
// TODO - check if current model should be saved first
    	File selectedFile = fileChooser.showOpenDialog(mainStage);
    	if (selectedFile != null) {
// TODO - tell the perspective to open it    		 
    	}
    }
    
    @FXML
    private void saveModel(ActionEvent ae) {
    	File savedFile = fileChooser.showSaveDialog(mainStage);
    	if (savedFile != null) {
 // TODO - tell the perspective to save to it   		
    	}
    }
    
 	private Node createModelPage(Integer pgIndex) {	
 		ModelPage modelPage = modelPages.get(pgIndex);		
 		if (modelPage == null) {
 			modelPage = new ModelPage(() -> constructModelEditor("", Collections.emptyList())); 
 			modelPages.put(pgIndex, modelPage);
 		}
 		
 		Node result = modelPage.getModelEditor().getRootPane();			
 		return result;
 	}
	
	@FXML
 	private void addModelPage(ActionEvent ae) {
 		Integer currentPageIdx = modelPagination.getCurrentPageIndex();
 		
 		Map<Integer, ModelPage> newModelPageIdxs = new HashMap<>();
 		modelPages.entrySet().forEach(e -> {
 			if (e.getKey() > currentPageIdx) {
 				newModelPageIdxs.put(e.getKey()+1, e.getValue());
 			}
 			else {
 				newModelPageIdxs.put(e.getKey(), e.getValue());
 			}
 		});
 		modelPages.clear();
 		modelPages.putAll(newModelPageIdxs);
 		
 		modelPagination.setPageCount(modelPagination.getPageCount()+1);
 		modelPagination.setCurrentPageIndex(currentPageIdx+1);
 	}
	
	@FXML
 	private void previousModelPage(ActionEvent ae) {
		int prevPageIdx = modelPagination.getCurrentPageIndex()-1;
		modelPagination.setCurrentPageIndex(prevPageIdx);
	}
	
	@FXML
 	private void nextModelPage(ActionEvent ae) {
		int nextPageIdx = modelPagination.getCurrentPageIndex()+1;
		modelPagination.setCurrentPageIndex(nextPageIdx);
	}
 	
	@FXML
 	private void removeModelPage(ActionEvent ae) {
 		Integer currentPageIdx = modelPagination.getCurrentPageIndex();
 		modelPages.remove(currentPageIdx);
 		Map<Integer, ModelPage> newModelPageIdxs = new HashMap<>();
 		modelPages.entrySet().forEach(e -> {
 			if (e.getKey() > currentPageIdx) {
 				newModelPageIdxs.put(e.getKey()-1, e.getValue());
 			}
 			else {
 				newModelPageIdxs.put(e.getKey(), e.getValue());
 			}
 		});
 		modelPages.clear();
 		modelPages.putAll(newModelPageIdxs);	
 		// Reduce the # of pages
 		modelPagination.setPageCount(modelPagination.getPageCount()-1);
 		
 		if (currentPageIdx < modelPagination.getPageCount()) {
 			modelPagination.setCurrentPageIndex(currentPageIdx);
 		}
 		else {
 			modelPagination.setCurrentPageIndex(modelPagination.getPageCount()-1);
 		}
 	}
	
	private void updatePaginationControls(int currentPageIndex, int pageCount) {
		if (pageCount <= 1) {
			removePageButton.setDisable(true);
		}
		else {
			removePageButton.setDisable(false);
		}
		pageNofPLabel.setText(""+(currentPageIndex+1)+"/"+pageCount);
		
		if (currentPageIndex <= 0) {
			previousPageButton.setDisable(true);
		}
		else {
			previousPageButton.setDisable(false);
		}
		if (currentPageIndex >= modelPagination.getPageCount() -1) {
			nextPageButton.setDisable(true);
		}
		else {
			nextPageButton.setDisable(false);
		}
	}
	
	private ModelEditor constructModelEditor(String model, List<String> defaultQueries) {
		ModelEditor result = null;
		try {
			result = perspective.create(model, defaultQueries); 
		}
		catch (IOException ioe) {
// TODO - handle properly
			ioe.printStackTrace();				
		}
		return result;
	}
	
	private class ModelPage {
		private Supplier<ModelEditor> modelEditorSupplier;
		private ModelEditor modelEditor;
		public ModelPage(Supplier<ModelEditor> modelEditorSupplier) {
			this.modelEditorSupplier = modelEditorSupplier;
		}
		
		public ModelEditor getModelEditor() {
			if (modelEditor == null) {
				modelEditor = modelEditorSupplier.get();
			}
			return modelEditor;
		}
	}
}