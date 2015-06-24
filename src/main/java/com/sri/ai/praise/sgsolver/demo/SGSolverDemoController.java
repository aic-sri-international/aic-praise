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
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.IntStream;

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
import javafx.scene.control.RadioButton;
import javafx.scene.control.Separator;
import javafx.scene.control.Spinner;
import javafx.scene.control.SpinnerValueFactory;
import javafx.scene.control.ToggleGroup;
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
import com.google.common.util.concurrent.AtomicDouble;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.lang.ModelLanguage;
import com.sri.ai.praise.lang.grounded.common.FunctionTable;
import com.sri.ai.praise.lang.translate.impl.HOGMv1_to_UAI_Translator;
import com.sri.ai.praise.model.v1.HOGMSortDeclaration;
import com.sri.ai.praise.model.v1.imports.uai.UAIEvidenceReader;
import com.sri.ai.praise.model.v1.imports.uai.UAIModel;
import com.sri.ai.praise.model.v1.imports.uai.UAIModelReader;
import com.sri.ai.praise.model.v1.imports.uai.UAIUtil;
import com.sri.ai.praise.sgsolver.demo.editor.ModelPageEditor;
import com.sri.ai.praise.sgsolver.demo.model.ExamplePages;
import com.sri.ai.praise.sgsolver.demo.perspective.ChurchPerspective;
import com.sri.ai.praise.sgsolver.demo.perspective.HOGMPerspective;
import com.sri.ai.praise.sgsolver.demo.perspective.Perspective;
import com.sri.ai.util.math.Rational;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcons;

@Beta
public class SGSolverDemoController {
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
	private ToggleGroup perspectiveToggleGroup       = new ToggleGroup();
	private RadioButton hogmPerspectiveRadioButton   = new RadioButton("HOGM");
	private RadioButton churchPerspectiveRadioButton = new RadioButton("Church");
	private Button      importUAIModelButton         = new Button("Import UAI Model...");
	private Button      exportUAIModelButton         = new Button("Export UAI Model...");
	//
	private Spinner<Integer> displayPrecisionSpinner  = new Spinner<>();
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
	
	public static String displayResultPrecision(String answer) {
		String result = answer;
				
		int oldRoundingMode      = Rational.setToStringDotRoundingMode(_displayRoundingMode.getValue());
		int oldDisplayPrecision  = SyntaxTrees.setNumericDisplayPrecision(_displayPrecision.get());
		int oldScientificGreater = SyntaxTrees.setDisplayScientificGreaterNIntegerPlaces(_displayScientificGreater.get());
		int oldScientificAfter   = SyntaxTrees.setDisplayScientificAfterNDecimalPlaces(_displayScientificAfter.get());
		
		try {
			Expression parsed = Expressions.parse(answer);
			if (parsed != null) {
				result = toTermFormat(parsed);
			}
		}
		catch (Throwable t) {
			FXUtil.exception(t);
		}
		finally {
			Rational.setToStringDotRoundingMode(oldRoundingMode);
			SyntaxTrees.setNumericDisplayPrecision(oldDisplayPrecision);
			SyntaxTrees.setDisplayScientificGreaterNIntegerPlaces(oldScientificGreater);
			SyntaxTrees.setDisplayScientificAfterNDecimalPlaces(oldScientificAfter);
		}
		
		return result;
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
    	         		new FileChooser.ExtensionFilter("Model Files", "*.praise"));
    	
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
    
    private void togglePerspective(ActionEvent ae) { 	
    	if (perspectiveToggleGroup.getSelectedToggle() == hogmPerspectiveRadioButton) {
    		setPerspective(new HOGMPerspective());
    		importUAIModelButton.setDisable(false);
    		exportUAIModelButton.setDisable(false);
    	}
    	else if (perspectiveToggleGroup.getSelectedToggle() == churchPerspectiveRadioButton) {
    		setPerspective(new ChurchPerspective());
    		importUAIModelButton.setDisable(true);
    		exportUAIModelButton.setDisable(true);
    	}
    }
    
    private void importUAIModel(ActionEvent ae) { 	
    	try {
    		File uaiFile = uaiFileChooser.showOpenDialog(mainStage);
    		if (uaiFile != null) {
    			UAIModel model = UAIModelReader.read(uaiFile);
    			UAIEvidenceReader.read(uaiFile, model);
    			
    			StringJoiner sj = new StringJoiner("\n");
    			sj.add("// IMPORT OF: "+uaiFile.getName());
    			sj.add("//");
    			sj.add("// #variables                                = "+model.numberVariables());
    			sj.add("// #tables                                   = "+model.numberTables());
    			sj.add("// #unique function tables                   = "+model.numberUniqueFunctionTables());
    			sj.add("// Largest variable cardinality              = "+model.largestCardinality());
    			sj.add("// Largest # entries                         = "+model.largestNumberOfFunctionTableEntries());
    			sj.add("// Total #entries across all function tables = "+model.totalNumberEntriesForAllFunctionTables());

    			double totalNumberUniqueEntries        = 0;
    			double totalCompressedEntries          = 0;
    			double bestIndividualCompressionRatio  = 100; // i.e. none at all
    			double worstIndividualCompressionRatio = 0;
    			List<Expression> tables = new ArrayList<>();
    			for (int i = 0; i < model.numberUniqueFunctionTables(); i++) {
    				FunctionTable table = model.getUniqueFunctionTable(i);
    				
    				totalNumberUniqueEntries += table.numberEntries();
    				
    				Expression genericTableExpression = UAIUtil.constructGenericTableExpression(table, solver -> {
    					return solver;
    				});
    				
    				double compressedEntries = calculateCompressedEntries(genericTableExpression);
    				
    				double compressedRatio = compressedEntries / table.numberEntries();
    				if (compressedRatio < bestIndividualCompressionRatio) {
    					bestIndividualCompressionRatio = compressedRatio;
    				}
    				if (compressedRatio > worstIndividualCompressionRatio) {
    					worstIndividualCompressionRatio = compressedRatio;
    				}
    				
    				totalCompressedEntries += compressedEntries;
    				
    				for (int tableIdx : model.getTableIndexes(i)) {
    					Expression instanceTableExpression = UAIUtil.convertGenericTableToInstance(table, genericTableExpression, model.getVariableIndexesForTable(tableIdx));
    					tables.add(instanceTableExpression);
    				}
    			}
    			
    			sj.add("//");
    			sj.add("// Table compression ratio            = "+(totalCompressedEntries/totalNumberUniqueEntries));
    			sj.add("// Best individual compression ratio  = "+bestIndividualCompressionRatio);
    			sj.add("// Worst individual compression ratio = "+worstIndividualCompressionRatio);
    	
    			List<String> sorts   = new ArrayList<>();
    			List<String> randoms = new ArrayList<>(); 
    			List<String> queries = new ArrayList<>();
    			for (int varIdx = 0; varIdx < model.numberVariables(); varIdx++) {
    				int varCardinality = model.cardinality(varIdx);
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
    				queries.add(varName);
    			}
    			if (sorts.size() > 0) {
	    			sj.add("");
	    			sj.add("// SORT DECLARATIONS:");
	    			sorts.forEach(sort -> sj.add(sort));
    			}
    			sj.add("");
    			sj.add("// RANDOM VARIABLE DECLARATIONS:");
    			randoms.forEach(random -> sj.add(random));
    			sj.add("");
    			sj.add("// RULES:");
    			tables.forEach(table -> sj.add(table.toString()+";"));
    			
    			if (model.getEvidence().size() > 0) {
    				sj.add("");
        			sj.add("// EVIDENCE:");
    				for (Map.Entry<Integer, Integer> entry : model.getEvidence().entrySet()) {
	    				int varIdx = entry.getKey();
	    				int valIdx = entry.getValue();
	    				Expression varExpr   = Expressions.makeSymbol(UAIUtil.instanceVariableName(varIdx));
	    				Expression valueExpr = Expressions.makeSymbol(UAIUtil.instanceConstantValueForVariable(valIdx, varIdx, model.cardinality(varIdx)));
	    				if (valueExpr.equals(Expressions.TRUE)) {
	    					sj.add(varExpr.toString()+";");
	    				}
	    				else if (valueExpr.equals(Expressions.FALSE)) {
	    					sj.add(Not.make(varExpr).toString()+";");
	    				}
	    				else {
	    					sj.add(Equality.make(varExpr, valueExpr).toString()+";");
	    				}
	    			}
    			}
    			
    			newModel(sj.toString(), queries);
    		}
    	}
    	catch (Throwable th) {
    		FXUtil.exception(th);
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
	    				translator.translate("SGSolverDemo", new Reader[] {new StringReader(modelPage.getCurrentPageContents())}, 
	    						             new PrintWriter[] {uaiModelWriter, uaiEvidenceWriter});
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
	
		Label perspectiveLabel = new Label("Perspective");
		
		hogmPerspectiveRadioButton.setSelected(true); // HOGM perspective by default
		hogmPerspectiveRadioButton.setToggleGroup(perspectiveToggleGroup);
		churchPerspectiveRadioButton.setToggleGroup(perspectiveToggleGroup);
		hogmPerspectiveRadioButton.setOnAction(this::togglePerspective);
		churchPerspectiveRadioButton.setOnAction(this::togglePerspective);
		
		importUAIModelButton.setOnAction(this::importUAIModel);
		FXUtil.setDefaultButtonIcon(importUAIModelButton, FontAwesomeIcons.PUZZLE_PIECE);
		HBox importUAIHBox = newButtonHBox();
		importUAIHBox.getChildren().addAll(importUAIModelButton, new Label("Import UAI Model..."));
		
		exportUAIModelButton.setOnAction(this::exportUAIModel);
		FXUtil.setDefaultButtonIcon(exportUAIModelButton, FontAwesomeIcons.ARCHIVE);
		HBox exportUAIHBox = newButtonHBox();
		exportUAIHBox.getChildren().addAll(exportUAIModelButton, new Label("Export to UAI Model..."));
		
		openMenu.getChildren().addAll(
				saveAsHBox,
				hSep,
				perspectiveLabel,
				hogmPerspectiveRadioButton,
				churchPerspectiveRadioButton,
				new Separator(Orientation.HORIZONTAL),
				importUAIHBox,
				exportUAIHBox
		);
		
		return openMenu;
	}
	
	private Node configureSettingsContent() {
		VBox configureMenu = new VBox(2);
		configureMenu.setPadding(new Insets(3,3,3,3));
		
		HBox displayPrecisionHBox = newButtonHBox();
		displayPrecisionSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(1, 80, _displayPrecision.get()));
		displayPrecisionSpinner.setPrefWidth(60);
		_displayPrecision.bind(displayPrecisionSpinner.valueProperty());
		displayPrecisionHBox.getChildren().addAll(new Label("Display Numeric Precision:"), displayPrecisionSpinner);
		
		HBox displayScientificHBox = newButtonHBox();
		displayScientificGreater.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(2, 80, _displayScientificGreater.get()));
		displayScientificGreater.setPrefWidth(60);
		_displayScientificGreater.bind(displayScientificGreater.valueProperty());	
		displayScientificAfter.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(2, 80, _displayScientificAfter.get()));
		displayScientificAfter.setPrefWidth(60);
		_displayScientificAfter.bind(displayScientificAfter.valueProperty());
		displayScientificHBox.getChildren().addAll(new Label("Use Scientific When Outside Range:"), displayScientificGreater, new Label("."), displayScientificAfter);

		debugModeCheckBox.setSelected(_inDebugMode.get());
		debugModeCheckBox.setText("In Debug Mode");
		_inDebugMode.bind(debugModeCheckBox.selectedProperty());
		
		configureMenu.getChildren().addAll(
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
	
	private static double calculateCompressedEntries(Expression compressedTableExpression) {
		AtomicDouble count = new AtomicDouble(0);
		
		visitCompressedTableEntries(compressedTableExpression, count);
		
		return count.doubleValue();
	}
	
	private static void visitCompressedTableEntries(Expression compressedTableExpression, AtomicDouble count) {
		if (IfThenElse.isIfThenElse(compressedTableExpression)) {
			visitCompressedTableEntries(IfThenElse.thenBranch(compressedTableExpression), count);
			visitCompressedTableEntries(IfThenElse.elseBranch(compressedTableExpression), count);
		}
		else {
			// We are at a leaf node, therefore increment the count
			count.addAndGet(1);
		}
	}
	
	private static String toTermFormat(Expression expr) {
		StringBuilder result = new StringBuilder();
		
		toTermFormat(expr, result);
		
		return result.toString();
	}
	
	private static void toTermFormat(Expression expr, StringBuilder sb) {
		if (IfThenElse.isIfThenElse(expr)) {
			Expression condition  = IfThenElse.condition(expr);
			Expression thenBranch = IfThenElse.thenBranch(expr);
			Expression elseBranch = IfThenElse.elseBranch(expr);
			
			if (Expressions.isNumber(thenBranch) && Expressions.isNumber(thenBranch)) {
				// Simplify the form of the conditional
				Rational potential = thenBranch.rationalValue();	
				if (potential.isZero()) { // things with 0 potential are negations; it's more intuitive to convert them to that.
					if (condition.hasFunctor(FunctorConstants.NOT)) { // negate condition, avoiding double negations
						condition = condition.get(0);
					}
					else {
						if (Equality.isEquality(condition) && condition.numberOfArguments() == 2) {
							condition = Disequality.make(condition.get(0), condition.get(1));
						}
						else if (Disequality.isDisequality(condition)) {
							condition = Equality.make(condition.getArguments());
						}
						else {
							condition = Not.make(condition);
						}
					}
					potential = new Rational(1);
				}
				else if (potential.compareTo(1) < 0 && condition.hasFunctor(FunctorConstants.NOT)) {
					// 'unlikely negations' are better understood as likely statements -- eliminating a sort of double negation
					condition = condition.get(0);
					potential = potential.subtract(1).negate(); // this is the same as potential = 1.0 - potential;
				}
				sb.append(condition.toString());
				sb.append(" ");
				sb.append(Expressions.makeSymbol(potential).toString());
			}
			else {
				sb.append("if ");
				sb.append(condition.toString());
				sb.append(" then ");
				toTermFormat(thenBranch, sb);
				sb.append(" else ");
				toTermFormat(elseBranch, sb);
			}
		}
		else {
			sb.append(expr.toString());
		}
		
	}
}