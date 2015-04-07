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
import java.util.function.Consumer;
import java.util.function.Supplier;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableMap;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.Pagination;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.stage.FileChooser;
import javafx.stage.Stage;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.sgsolver.demo.model.ExamplePages;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcons;

@Beta
public class SGSolverDemoController {
	public static final KeyCombination GOTO_EDITOR_SHORTCUT = new KeyCodeCombination(KeyCode.E, KeyCombination.SHORTCUT_DOWN);
	public static final KeyCombination GOTO_QUERY_SHORTCUT  = new KeyCodeCombination(KeyCode.W, KeyCombination.SHORTCUT_DOWN);
	public static final KeyCombination RUN_SHORTCUT         = new KeyCodeCombination(KeyCode.R, KeyCombination.SHORTCUT_DOWN);
	//
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
	@FXML private Button undoModelEditButton;
	@FXML private Button redoModelEditButton;
	//
	@FXML private Button undoPagesChangeButton;
	@FXML private Button redoPagesChangeButton;
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
	
	public void setMainStage(Stage stage) {
		this.mainStage = stage;
		this.mainStage.setTitle("PRAiSE");
		
		stage.sceneProperty().addListener((observer, oldScene, newScene) -> {
			if (newScene != null) {				
				newScene.setOnKeyPressed(ke -> {
					if (GOTO_EDITOR_SHORTCUT.match(ke)) {						
						perspective.gotoModelEditor();
					} 
					else if (GOTO_QUERY_SHORTCUT.match(ke)) {
						perspective.gotoQueryEditor();
					}
					else if (RUN_SHORTCUT.match(ke)) {
						perspective.executeQuery();
					}
				});
			}
		});
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
    	FXUtil.setDefaultButtonIcon(undoModelEditButton, FontAwesomeIcons.ROTATE_LEFT);
    	FXUtil.setDefaultButtonIcon(redoModelEditButton, FontAwesomeIcons.ROTATE_RIGHT);
    	//
    	FXUtil.setButtonStackedIcons(undoPagesChangeButton, FontAwesomeIcons.ROTATE_LEFT, FontAwesomeIcons.SQUARE_ALT);
    	FXUtil.setButtonStackedIcons(redoPagesChangeButton, FontAwesomeIcons.ROTATE_RIGHT, FontAwesomeIcons.SQUARE_ALT);
    	//
    	FXUtil.setPaginationButtonIcon(removePageButton, FontAwesomeIcons.MINUS);
    	FXUtil.setPaginationButtonIcon(previousPageButton, FontAwesomeIcons.CARET_LEFT);
    	FXUtil.setPaginationButtonIcon(nextPageButton, FontAwesomeIcons.CARET_RIGHT);
    	FXUtil.setPaginationButtonIcon(addPageButton, FontAwesomeIcons.PLUS);
    	//
    	fileChooser = new FileChooser();
    	fileChooser.getExtensionFilters().addAll(
    	         		new FileChooser.ExtensionFilter("Model Files", "*.sgmodel"));
    	//
    	//
    	modelPagination.setPageFactory(this::createModelPage);
		
    	modelPagination.pageCountProperty().addListener((observable, oldValue, newValue) ->
				updatePaginationControls(modelPagination.getCurrentPageIndex(), newValue.intValue()));
		
		modelPagination.currentPageIndexProperty().addListener((observable, oldValue, newValue) ->
				updatePaginationControls(newValue.intValue(), modelPagination.getPageCount()));
		
		examplesComboBox.getSelectionModel().selectedIndexProperty().addListener(this::exampleSelectionChaned);
		
		setPerspective(new HOGMPerspective());
    }
    
    private void setPerspective(Perspective perspective) {
    	if (this.perspective != null) {
    		this.perspective.modelPageEditorsProperty().removeListener(this::modelPagesChanged);
    		this.perspective.canUndoModelPageEditProperty().removeListener(this::undoModelEditChanged);
    		this.perspective.canRedoModelPageEditProperty().removeListener(this::redoModelEditChanged);
    		this.perspective.canUndoPageChange().removeListener(this::undoPageChange);
    		this.perspective.canRedoPageChange().removeListener(this::redoPageChange);
    		this.perspective.saveRequiredProperty().removeListener(this::saveRequiredChange);
    		this.perspective.modelFileProperty().removeListener(this::modelFileChanged);
    	}
    	
    	this.perspective = perspective;
    	this.perspective.setCurrentModelPageIndexProperty(modelPagination.currentPageIndexProperty());
    	this.perspective.modelPageEditorsProperty().addListener(this::modelPagesChanged);
    	this.perspective.canUndoModelPageEditProperty().addListener(this::undoModelEditChanged);
		this.perspective.canRedoModelPageEditProperty().addListener(this::redoModelEditChanged);
		this.perspective.canUndoPageChange().addListener(this::undoPageChange);
		this.perspective.canRedoPageChange().addListener(this::redoPageChange);
		this.perspective.saveRequiredProperty().addListener(this::saveRequiredChange);
		this.perspective.modelFileProperty().addListener(this::modelFileChanged);
    	
    	// Set up the examples
    	examplesComboBox.getItems().clear();    	
    	perspective.getExamples().forEach(eg -> examplesComboBox.getItems().add(eg));
    	examplesComboBox.getSelectionModel().selectFirst();
    }
   
    public void exampleSelectionChaned(ObservableValue<? extends Number> observable, Number oldValue, Number newValue) {
		if (newValue.intValue() >= 0 && newValue.intValue() < examplesComboBox.getItems().size()) {					
			ExamplePages egPages = examplesComboBox.getItems().get(newValue.intValue());
			
			checkSaveRequired(true);
			modelPagination.setPageCount(Pagination.INDETERMINATE);
			perspective.newModel(egPages);
		}
	}
    
    @FXML
    private void newModel(ActionEvent ae) {
    	checkSaveRequired(true);
    	modelPagination.setPageCount(Pagination.INDETERMINATE);
    	perspective.newModel();
		
		// Want to indicate that we are not using a particular example after a new model is instantiated.
		examplesComboBox.getSelectionModel().select(-1);
    }
    
    @FXML
    private void openModel(ActionEvent ae) {
    	checkSaveRequired(true);
    	File selectedFile = fileChooser.showOpenDialog(mainStage);
    	if (selectedFile != null) {
    		modelPagination.setPageCount(Pagination.INDETERMINATE);
    		perspective.newModel(selectedFile);
        	// Want to indicate that we are not using a particular example after a new model is instantiated.
        	examplesComboBox.getSelectionModel().select(-1);
    	}
    }
    
    @FXML
    private void saveModel(ActionEvent ae) {
    	checkSaveRequired(false);
    	// After saving indicate not an example
    	examplesComboBox.getSelectionModel().select(-1);
    }
    
    @FXML
    private void undoModelPageEdit(ActionEvent ae) {
    	callCurrentModelPageEditor(mpe -> mpe.undo());
    }
    
    @FXML
    private void redoModelPageEdit(ActionEvent ae) {
    	callCurrentModelPageEditor(mpe -> mpe.redo());
    }
    
    @FXML
    private void undoPagesChange(ActionEvent ae) {
    	perspective.undoPageChange();
    }
    
    @FXML
    private void redoPagesChange(ActionEvent ae) {
    	perspective.redoPageChange();
    }    
	
	@FXML
 	private void addModelPage(ActionEvent ae) {
 		Integer currentPageIdx = modelPagination.getCurrentPageIndex();

 		perspective.addPage(currentPageIdx);
 	}
	
	@FXML
 	private void removeModelPage(ActionEvent ae) {
		Integer currentPageIdx = modelPagination.getCurrentPageIndex();
		
 		perspective.removePage(currentPageIdx);
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
	
    private void callCurrentModelPageEditor(Consumer<ModelPageEditor> caller) {
    	int currentPageIdx = modelPagination.getCurrentPageIndex();
    	if (perspective.getModelPageEditors().containsKey(currentPageIdx)) {
    		ModelPageEditor modelPage = perspective.getModelPageEditors().get(currentPageIdx).get();
    		caller.accept(modelPage);
    	}	
    }
	
    private void checkSaveRequired(boolean confirmationRequired) {
    	if (perspective.isSaveRequired()) {
    		if (perspective.getModelFile() == null) {
    			File saveAsFile = fileChooser.showSaveDialog(mainStage);
    			if (saveAsFile != null) {
    				perspective.saveAs(saveAsFile);
    			}
    		}
    		else {
    			if (!confirmationRequired || FXUtil.confirmation(mainStage, "Save Changes?")) {
    				perspective.save();
    			}
    		}
    	}
    }
    
 	private Node createModelPage(Integer pgIndex) {	
 		Node result = null;
 		if (perspective != null && perspective.getModelPageEditors().containsKey(pgIndex)) {			
 			ModelPageEditor modelPage = perspective.getModelPageEditors().get(pgIndex).get();
 			result = modelPage.getRootPane(); 
 		}
 		
 		return result;
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

	private void modelPagesChanged(
			ObservableValue<? extends ObservableMap<Integer, Supplier<ModelPageEditor>>> observable,
			ObservableMap<Integer, Supplier<ModelPageEditor>> oldValue,
			ObservableMap<Integer, Supplier<ModelPageEditor>> newValue) {
				
		// Ensure the # of pages is correct
		modelPagination.setPageCount(newValue.size());
	}
	
	private void undoModelEditChanged(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {		
		undoModelEditButton.setDisable(!newValue);
	}
	
	private void redoModelEditChanged(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {
		redoModelEditButton.setDisable(!newValue);
	}
	
	private void undoPageChange(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {		
		undoPagesChangeButton.setDisable(!newValue);
	}
	
	private void redoPageChange(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {
		redoPagesChangeButton.setDisable(!newValue);
	}
	
	private void saveRequiredChange(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {
		saveButton.setDisable(!newValue);
	}
	
	private void modelFileChanged(ObservableValue<? extends File> observable, File oldValue, File newValue) {
		String title = "PRAiSE";
		if (newValue != null) {
			title += " ["+newValue.getAbsolutePath()+"]";
		}
		this.mainStage.setTitle(title);
	}
}