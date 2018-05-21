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
package com.sri.ai.praise.application.praise.app;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Supplier;

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableMap;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.Pagination;
import javafx.scene.control.Separator;
import javafx.scene.control.Spinner;
import javafx.scene.control.SpinnerValueFactory;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.FileChooser;
import javafx.stage.Stage;

import org.controlsfx.control.PopOver;
import org.controlsfx.control.PopOver.ArrowLocation;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.praise.application.praise.app.editor.ModelPageEditor;
import com.sri.ai.praise.application.praise.app.model.ExamplePages;
import com.sri.ai.praise.application.praise.app.perspective.HOGMPerspective;
import com.sri.ai.praise.application.praise.app.perspective.Perspective;
import com.sri.ai.praise.inference.expressionbased.ExpressionFactorsAndTypes;
import com.sri.ai.praise.inference.hogm.HOGMFactorsAndTypes;
import com.sri.ai.praise.language.ModelLanguage;
import com.sri.ai.praise.language.translate.TranslatorOptions;
import com.sri.ai.praise.language.translate.core.HOGMv1_to_UAI_Translator;
import com.sri.ai.praise.language.translate.core.UAI_to_HOGMv1_Using_Equalities_Translator;
import com.sri.ai.praise.model.v1.hogm.antlr.HOGMParserWrapper;
import com.sri.ai.praise.model.v1.hogm.antlr.ParsedHOGModel;
import com.sri.ai.util.math.Rational;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcons;

@Beta
public class PRAiSEController {
	public static final KeyCombination GOTO_EDITOR_SHORTCUT = new KeyCodeCombination(KeyCode.E, KeyCombination.SHORTCUT_DOWN);
	public static final KeyCombination GOTO_QUERY_SHORTCUT  = new KeyCodeCombination(KeyCode.U, KeyCombination.SHORTCUT_DOWN);
	public static final KeyCombination RUN_SHORTCUT         = new KeyCodeCombination(KeyCode.R, KeyCombination.SHORTCUT_DOWN);
	public static final KeyCombination NEXT_PAGE_SHORTCUT   = new KeyCodeCombination(KeyCode.PAGE_UP, KeyCombination.SHORTCUT_ANY);
	public static final KeyCombination PREV_PAGE_SHORTCUT   = new KeyCodeCombination(KeyCode.PAGE_DOWN, KeyCombination.SHORTCUT_ANY);
	//
	public enum DisplayRoundingMode {
		UP(Rational.ROUND_UP), 
		DOWN(Rational.ROUND_DOWN), 
		CEILING(Rational.ROUND_CEILING), 
		FLOOR(Rational.ROUND_FLOOR), 
		HALF_UP(Rational.ROUND_HALF_UP), 
		HALF_DOWN(Rational.ROUND_HALF_DOWN), 
		HALF_EVEN(Rational.ROUND_HALF_EVEN), 
		HALF_CEILING(Rational.ROUND_HALF_CEILING),
		HALF_FLOOR(Rational.ROUND_HALF_FLOOR), 
		ROUND_HALF_ODD(Rational.ROUND_HALF_ODD);
	
		private int value;
		private DisplayRoundingMode(int value) {
			this.value = value;
		}
		
		public int getValue() {
			return value;
		}
	}
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
	@FXML private Button configureButton;
	//
	static IntegerProperty     _displayPrecision         = new SimpleIntegerProperty(4);
	static BooleanProperty     _isDisplayExact           = new SimpleBooleanProperty(false);
	static DisplayRoundingMode _displayRoundingMode      = DisplayRoundingMode.FLOOR;
	static IntegerProperty     _displayScientificGreater = new SimpleIntegerProperty(8);
	static IntegerProperty     _displayScientificAfter   = new SimpleIntegerProperty(6);
	static BooleanProperty     _inDebugMode              = new SimpleBooleanProperty(false);
	//
	private FileChooser praiseFileChooser;
	private FileChooser uaiFileChooser;
	private PopOver openMenuPopOver          = new PopOver();
	private PopOver configureSettingsPopOver = new PopOver();
	//
	private Button      importUAIModelButton         = new Button("Import UAI FactorNetwork...");
	private Button      exportUAIModelButton         = new Button("Export UAI FactorNetwork...");
	//
	private Spinner<Integer> displayPrecisionSpinner  = new Spinner<>();
	private CheckBox         displayExactCheckBox     = new CheckBox();
	private Spinner<Integer> displayScientificGreater = new Spinner<>();
	private Spinner<Integer> displayScientificAfter   = new Spinner<>();
	private CheckBox         debugModeCheckBox        = new CheckBox();
	
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
					else if (NEXT_PAGE_SHORTCUT.match(ke)) {
						if (!nextPageButton.isDisabled()) {
							nextPageButton.fire();
						}
					}
					else if (PREV_PAGE_SHORTCUT.match(ke)) {
						if (!previousPageButton.isDisabled()) {
							previousPageButton.fire();
						}
					}
				});
			}
		});
	}
	
	public static void computeExpressionWithDesiredPrecision(Runnable computeCallback) {				
		int oldRoundingMode      = Rational.setToStringDotRoundingMode(_displayRoundingMode.getValue());
		int oldDisplayPrecision  = ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInApproximateRepresentationOfNumericalSymbols(_displayPrecision.get());
		boolean oldDisplayExact  = ExpressoConfiguration.setDisplayNumericsExactlyForSymbols(_isDisplayExact.getValue());
		int oldScientificGreater = ExpressoConfiguration.setDisplayNumericsMostIntegerPlacesBeforeSwitchingToScientificNotation(_displayScientificGreater.get());
		int oldScientificAfter   = ExpressoConfiguration.setDisplayNumericsGreatestInitialNonZeroDecimalPlacePositionBeforeSwitchingToScientificNotation(_displayScientificAfter.get());
		
		try {
			computeCallback.run();
		}
		catch (Throwable t) {
			FXUtil.exception(t);
		}
		finally {
			Rational.setToStringDotRoundingMode(oldRoundingMode);
			ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInApproximateRepresentationOfNumericalSymbols(oldDisplayPrecision);
			ExpressoConfiguration.setDisplayNumericsExactlyForSymbols(oldDisplayExact);
			ExpressoConfiguration.setDisplayNumericsMostIntegerPlacesBeforeSwitchingToScientificNotation(oldScientificGreater);
			ExpressoConfiguration.setDisplayNumericsGreatestInitialNonZeroDecimalPlacePositionBeforeSwitchingToScientificNotation(oldScientificAfter);
		}
	}

	public static boolean isInDebugMode() {
		return _inDebugMode.get();
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
    	FXUtil.setDefaultButtonIcon(configureButton, FontAwesomeIcons.WRENCH);
    	//
    	praiseFileChooser = new FileChooser();
    	praiseFileChooser.getExtensionFilters().addAll(
    	         		new FileChooser.ExtensionFilter("FactorNetwork Files", "*.praise"));
    	
    	uaiFileChooser = new FileChooser();
    	uaiFileChooser.getExtensionFilters().addAll(
         		new FileChooser.ExtensionFilter("UAI Files", "*.uai"));
    	
    	openMenuPopOver.setArrowLocation(ArrowLocation.LEFT_TOP);
    	openMenuPopOver.setAutoHide(true);
    	openMenuPopOver.setDetachedTitle("Menu");
    	openMenuPopOver.setContentNode(openMenuContent());
    	
    	configureSettingsPopOver.setArrowLocation(ArrowLocation.RIGHT_TOP);
    	configureSettingsPopOver.setAutoHide(true);
    	configureSettingsPopOver.setDetachedTitle("Configure Settings");
    	configureSettingsPopOver.setContentNode(configureSettingsContent());
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
    	checkSaveRequired(true);
    	
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
    private void openMenu(ActionEvent ae) {
    	if (openMenuPopOver.isShowing()) {
    		openMenuPopOver.hide();
    	}
    	else {
    		openMenuPopOver.show(openMenuButton);
    	}
    }
    
    @FXML
    private void newModel(ActionEvent ae) {
    	newModel("", Collections.emptyList());
    }
    
    @FXML
    private void openModel(ActionEvent ae) {
    	checkSaveRequired(true);
    	File selectedFile = praiseFileChooser.showOpenDialog(mainStage);
    	if (selectedFile != null) {
    		modelPagination.setPageCount(Pagination.INDETERMINATE);
    		perspective.newModel(selectedFile);
        	// Want to indicate that we are not using a particular example after a new model is instantiated.
        	examplesComboBox.getSelectionModel().select(-1);
    	}
    }
    
    @FXML
    private void saveModel(ActionEvent ae) {
    	if (checkSaveRequired(false)) {
	    	// After saving indicate not an example
	    	examplesComboBox.getSelectionModel().select(-1);
    	}
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
	
	@FXML
 	private void addModelPage(ActionEvent ae) {
 		Integer currentPageIdx = modelPagination.getCurrentPageIndex();

 		perspective.addPage(currentPageIdx);
 	}
	
    @FXML
    private void configureSettings(ActionEvent ae) {
    	if (configureSettingsPopOver.isShowing()) {
    		configureSettingsPopOver.hide();
    	}
    	else {
    		configureSettingsPopOver.show(configureButton);
    	}
    }
    
    private void newModel(String contents, List<String> defaultQueries) {
    	checkSaveRequired(true);
    	modelPagination.setPageCount(Pagination.INDETERMINATE);
    	perspective.newModel(contents, defaultQueries);
		
		// Want to indicate that we are not using a particular example after a new model is instantiated.
		examplesComboBox.getSelectionModel().select(-1);
    }
    
    private void saveModelAs(ActionEvent ae) {
    	File saveAsFile = praiseFileChooser.showSaveDialog(mainStage);
		if (saveAsFile != null) {
			perspective.saveAs(saveAsFile);
			// After saving indicate not an example
	    	examplesComboBox.getSelectionModel().select(-1);
		}
    }
    
    private void importUAIModel(ActionEvent ae) { 	
		File uaiModelFile = uaiFileChooser.showOpenDialog(mainStage);
		if (uaiModelFile != null) {
			File         uaiEvidenceFile = new File(uaiModelFile.getParent(), uaiModelFile.getName()+".evid");
			StringWriter hogmWriter  = new StringWriter();
			try (BufferedReader uaiModelReader   = new BufferedReader(new FileReader(uaiModelFile));
				BufferedReader uaiEvidenceReader = new BufferedReader(new FileReader(uaiEvidenceFile));
				PrintWriter    hogmPrintWriter   = new PrintWriter(hogmWriter)
				) {
				
				UAI_to_HOGMv1_Using_Equalities_Translator translator = new UAI_to_HOGMv1_Using_Equalities_Translator();
				translator.translate(uaiModelFile.getName(), 
						new Reader[] {uaiModelReader, uaiEvidenceReader}, 
						new PrintWriter[] {hogmPrintWriter}, 
						new TranslatorOptions());
				
				String hogmModel = hogmWriter.toString();
			
				// For convenience, pull out all possible queries
				HOGMParserWrapper parser          = new HOGMParserWrapper();
				ParsedHOGModel    parsedModel     = parser.parseModel(hogmModel);
				ExpressionFactorsAndTypes   factorsAndTypes = new HOGMFactorsAndTypes(parsedModel);
				List<String>      queries         = new ArrayList<>(factorsAndTypes.getMapFromRandomVariableNameToTypeName().keySet());
				
				newModel(hogmModel, queries);
			}
			catch (Throwable th) {
	    		FXUtil.exception(th);
	    	}
		}
    }
    
    private void exportUAIModel(ActionEvent ae) {
    	try {
	    	callCurrentModelPageEditor(modelPage -> {
	    		File uaiModelFile = uaiFileChooser.showSaveDialog(mainStage);
	    		if (uaiModelFile != null) {
	    			if (!uaiModelFile.getName().endsWith(ModelLanguage.UAI.getDefaultFileExtension())) {
	    				uaiModelFile = new File(uaiModelFile.getParent(), uaiModelFile.getName()+ModelLanguage.UAI.getDefaultFileExtension());
	    			}
	    			File uaiEvidenceFile = new File(uaiModelFile.getParent(), uaiModelFile.getName()+ModelLanguage.UAI.getDefaultFileExtension()+".evid");
	    			try (PrintWriter uaiModelWriter    = new PrintWriter(uaiModelFile);
	    				 PrintWriter uaiEvidenceWriter = new PrintWriter(uaiEvidenceFile)) {
	    				HOGMv1_to_UAI_Translator translator = new HOGMv1_to_UAI_Translator();
	    				translator.translate(uaiModelFile.getName(), 
	    						new Reader[] {new StringReader(modelPage.getCurrentPageContents())}, 
	    						new PrintWriter[] {uaiModelWriter, uaiEvidenceWriter},
	    						new TranslatorOptions());
	    			}
	    			catch (Throwable th) {
	    				FXUtil.exception(th);
	    			}
	    		}
	    	});
    	} catch (Throwable th) {
    		FXUtil.exception(th);
    	}
    }
	
    private void callCurrentModelPageEditor(Consumer<ModelPageEditor> caller) {
    	int currentPageIdx = modelPagination.getCurrentPageIndex();
    	if (perspective.getModelPageEditors().containsKey(currentPageIdx)) {
    		ModelPageEditor modelPage = perspective.getModelPageEditors().get(currentPageIdx).get();
    		caller.accept(modelPage);
    	}	
    }
	
    private boolean checkSaveRequired(boolean confirmationRequired) {
    	boolean result = false; // Not saved
    	if (perspective != null && perspective.isSaveRequired()) {
    		if (perspective.getModelFile() == null) {
    			File saveAsFile = praiseFileChooser.showSaveDialog(mainStage);
    			if (saveAsFile != null) {
    				perspective.saveAs(saveAsFile);
    				result = true;
    			}
    		}
    		else {
    			if (!confirmationRequired || FXUtil.confirmation(mainStage, "Save Changes?")) {
    				perspective.save();
    				result = true;
    			}
    		}
    	}
    	return result;
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
	
	private Node openMenuContent() {
		VBox openMenu = new VBox(2);
		openMenu.setPadding(new Insets(3,3,3,3));
		
		Button saveAsButton = new Button("Save As...");
		saveAsButton.setOnAction(this::saveModelAs);
    	FXUtil.setButtonStackedIcons(saveAsButton, FontAwesomeIcons.SAVE, FontAwesomeIcons.PENCIL);
    	HBox saveAsHBox = newButtonHBox();
    	saveAsHBox.getChildren().addAll(saveAsButton, new Label("Save As..."));
		
		Separator hSep = new Separator(Orientation.HORIZONTAL);
		hSep.setPrefWidth(170);
		
		importUAIModelButton.setOnAction(this::importUAIModel);
		FXUtil.setDefaultButtonIcon(importUAIModelButton, FontAwesomeIcons.PUZZLE_PIECE);
		HBox importUAIHBox = newButtonHBox();
		importUAIHBox.getChildren().addAll(importUAIModelButton, new Label("Import UAI FactorNetwork..."));
		
		exportUAIModelButton.setOnAction(this::exportUAIModel);
		FXUtil.setDefaultButtonIcon(exportUAIModelButton, FontAwesomeIcons.ARCHIVE);
		HBox exportUAIHBox = newButtonHBox();
		exportUAIHBox.getChildren().addAll(exportUAIModelButton, new Label("Export to UAI FactorNetwork..."));
		
		openMenu.getChildren().addAll(
				saveAsHBox,
				hSep,
				importUAIHBox,
				exportUAIHBox
		);
		
		return openMenu;
	}
	
	private Node configureSettingsContent() {
		VBox configureMenu = new VBox(2);
		configureMenu.setPadding(new Insets(3,3,3,3));
		
		displayExactCheckBox.setSelected(_isDisplayExact.get());
		displayExactCheckBox.setText("Display numbers exactly (using fractions if needed).");
		_isDisplayExact.bind(displayExactCheckBox.selectedProperty());
		
		HBox displayPrecisionHBox = newButtonHBox();
		displayPrecisionSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(1, 80, _displayPrecision.get()));
		displayPrecisionSpinner.setPrefWidth(60);
		_displayPrecision.bind(displayPrecisionSpinner.valueProperty());
		displayPrecisionHBox.getChildren().addAll(new Label("If approximating, use up to "), displayPrecisionSpinner, new Label(" decimal places; otherwise, resort to scientific notation."));
		
		HBox displayScientificHBox = newButtonHBox();
		displayScientificGreater.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(2, 80, _displayScientificGreater.get()));
		displayScientificGreater.setPrefWidth(60);
		_displayScientificGreater.bind(displayScientificGreater.valueProperty());	
		displayScientificAfter.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(2, 80, _displayScientificAfter.get()));
		displayScientificAfter.setPrefWidth(60);
		_displayScientificAfter.bind(displayScientificAfter.valueProperty());
		displayScientificHBox.getChildren().addAll(new Label("Also use scientific notation if number has more than "), displayScientificGreater, new Label(" integer places or first non-zero decimal place occurs after the "), displayScientificAfter, new Label("-th position."));

		debugModeCheckBox.setSelected(_inDebugMode.get());
		debugModeCheckBox.setText("Run in debug mode.");
		_inDebugMode.bind(debugModeCheckBox.selectedProperty());
		
		configureMenu.getChildren().addAll(
				displayExactCheckBox,
				displayPrecisionHBox,						
				displayScientificHBox,
				new Separator(Orientation.HORIZONTAL),
				debugModeCheckBox
		);
		
		return configureMenu;
	}
	
	private HBox newButtonHBox() {
		HBox hBox = new HBox(2);
		hBox.setAlignment(Pos.CENTER_LEFT);
		
		return hBox;
	}
	
//	private static double calculateCompressedEntries(Expression compressedTableExpression) {
//		AtomicDouble count = new AtomicDouble(0);
//		
//		visitCompressedTableEntries(compressedTableExpression, count);
//		
//		return count.doubleValue();
//	}
	
//	private static void visitCompressedTableEntries(Expression compressedTableExpression, AtomicDouble count) {
//		if (IfThenElse.isIfThenElse(compressedTableExpression)) {
//			visitCompressedTableEntries(IfThenElse.thenBranch(compressedTableExpression), count);
//			visitCompressedTableEntries(IfThenElse.elseBranch(compressedTableExpression), count);
//		}
//		else {
//			// We are at a leaf node, therefore increment the count
//			count.addAndGet(1);
//		}
//	}
}