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
package com.sri.ai.praise.sgsolver.demo.query;

import java.util.ArrayList;
import java.util.List;
import java.util.StringJoiner;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.model.v1.hogm.antlr.ParsedHOGModel;
import com.sri.ai.praise.sgsolver.demo.FXUtil;
import com.sri.ai.praise.sgsolver.demo.SGSolverDemoController;
import com.sri.ai.praise.sgsolver.demo.editor.HOGMCodeArea;
import com.sri.ai.praise.sgsolver.demo.editor.ModelPageEditor;
import com.sri.ai.praise.sgsolver.demo.service.ExecuteHOGMQueryService;
import com.sri.ai.praise.sgsolver.solver.HOGMQueryError;
import com.sri.ai.util.base.Pair;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcons;
import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Node;
import javafx.scene.control.Accordion;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ListView;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.ProgressIndicator;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TitledPane;
import javafx.scene.control.Tooltip;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;

@Beta
public class QueryController {
	//
	@FXML private ComboBox<String> queryComboBox;
	//
	@FXML private Button executeButton;
	@FXML private Button clearOutputButton;
	//
	@FXML private ProgressBar queryProgressBar;
	//
	@FXML private ScrollPane outputScrollPane;
	@FXML private Accordion outputAccordion;
	//
	@FXML private Tooltip executeTooltip;
	//
	private ModelPageEditor modelPageEditor;
	//
	private ExecuteHOGMQueryService executeQueryService = new ExecuteHOGMQueryService();
	
	public static FXMLLoader newLoader( ) {
		FXMLLoader result = new FXMLLoader(QueryController.class.getResource("querypane.fxml"));
		return result;
	}
	
	public void setModelPageEditor(ModelPageEditor modelPageEditor) {
		this.modelPageEditor = modelPageEditor;
	}
	
	
	public void addDefaultQueries(List<String> queries) {
		if (queries.size() > 0) {
			queries.forEach(query -> {
				if (!queryComboBox.getItems().contains(query)) {
					queryComboBox.getItems().add(query);
				}
			});
			queryComboBox.getSelectionModel().select(queryComboBox.getItems().indexOf(queries.get(0)));
		}
	}
	
	public String getCurrentQuery() {
		return queryComboBox.getEditor().getText();
	}
	
	public List<String> getCurrentQueries() {
		List<String> result = new ArrayList<>(queryComboBox.getItems().subList(0, queryComboBox.getItems().size()));
		return result;
	}
	
	public void gotoQueryEditor() {
		queryComboBox.requestFocus();
		queryComboBox.getEditor().selectAll();
	}
	
	public void executeQuery() {		
		queryComboBox.getEditor().commitValue();
		queryComboBox.setValue(queryComboBox.getEditor().getText());	
		Platform.runLater(() -> executeButton.fire());
	}
	
	@FXML
	private void initialize() {
		//
    	FXUtil.setDefaultButtonIcon(executeButton, FontAwesomeIcons.PLAY);
    	FXUtil.setDefaultButtonIcon(clearOutputButton, FontAwesomeIcons.ERASER);
    	
    	queryComboBox.valueProperty().addListener((observable, oldValue, newValue) -> {
    		if (newValue != null) {
    			String value = newValue.trim();
    			if (value.length() > 0) {
    				if (!queryComboBox.getItems().contains(value)) {
    					queryComboBox.getItems().add(value);
    					queryComboBox.getSelectionModel().select(queryComboBox.getItems().size()-1);
    				}
    			}
    		}
    	});
    	// For details - 
    	// see: http://stackoverflow.com/questions/26512143/javafx-capture-enter-key-pressed
    	queryComboBox.getEditor().addEventFilter(KeyEvent.KEY_PRESSED, (KeyEvent keyEvent) -> {
            if (keyEvent.getCode() == KeyCode.ENTER) {
            	keyEvent.consume();
            	// Run later so that we can ensure the value is set on the combo box before triggering the query.
            	Platform.runLater(() -> {
            		executeButton.fire();
            	});
            }
            else if (keyEvent.getCode() == KeyCode.DOWN && !queryComboBox.isShowing()) {
            	//keyEvent.consume(); // NOTE: comment out as we want the default behavior of going to the end of the text to still work
            	Platform.runLater(() -> {
            		queryComboBox.show();
            	});
            }
        });
    	
    	executeQueryService.runningProperty().addListener((observable, previouslyRunning, currentlyRunning) -> {
    		if (currentlyRunning) {
    			FXUtil.setDefaultButtonIcon(executeButton, FontAwesomeIcons.STOP);
    			executeTooltip.setText("Stop query");
    			queryProgressBar.setProgress(ProgressIndicator.INDETERMINATE_PROGRESS);
    		}
    		else {
    			FXUtil.setDefaultButtonIcon(executeButton, FontAwesomeIcons.PLAY);
    			executeTooltip.setText("Run query");
    			queryProgressBar.setProgress(0);
    		}
    	});
    	
    	executeQueryService.valueProperty().addListener((observable, oldResult, newResult) -> {
			if (newResult != null) { 
				if (newResult.isErrors()) {
					displayQueryErrors(newResult.getQueryString(), newResult.getErrors(), newResult.getMillisecondsToCompute());			
				}
				else {
					displayQueryAnswer(newResult.getQueryString(), newResult.getResult(), newResult.getParsedModel(), newResult.getMillisecondsToCompute());	
				}
			}
    	});
    	
    	executeQueryService.onFailedProperty().addListener((workStateEvent) -> {
    		// something unexpected occurred, as errors are meant to be returned in the query result if detected, therefore notify user of situation  
    		FXUtil.exception(executeQueryService.getException());
    	});
	}
	
	@FXML
    private void executeQuery(ActionEvent event) {
		if (executeQueryService.isRunning()) {
			executeQueryService.cancel();
		}
		else {
			Pair<List<HOGMQueryError>, String> initialModelValidation = modelPageEditor.validateAndGetModel();
			
			if (initialModelValidation.first.size() > 0) {
				displayQueryErrors(getCurrentQuery(), initialModelValidation.first, 0);
			}
			else {
				executeQueryService.setModel(initialModelValidation.second);
				executeQueryService.setQuery(getCurrentQuery());
				executeQueryService.restart();
			}
		}
	}
	
	@FXML
    private void clearOutput(ActionEvent event) {
		outputAccordion.getPanes().clear();
	}
	
	private void displayQueryErrors(String query, List<HOGMQueryError> queryErrors, long millisecondsToCompute) {
		String title = "Query '"+query+"' encountered "+queryErrors.size()+" error(s) when attempting to compute answer ("+duration("took ", millisecondsToCompute)+")";
		ListView<HOGMQueryError> errors = new ListView<>(FXCollections.observableList(queryErrors));
		//errors.setFixedCellSize(24);
		errors.setPrefHeight(24*5);
		errors.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
		errors.getSelectionModel().selectedIndexProperty().addListener((obs, oldValue, newValue) -> {
			if (newValue.intValue() >= 0) {
				HOGMQueryError qError = errors.getItems().get(newValue.intValue());
				if (qError.getContext() == HOGMQueryError.Context.MODEL) {
					modelPageEditor.highlight(qError.getStartContextIndex(), qError.getEndContextIndex());
				}
				else if (qError.getContext() == HOGMQueryError.Context.QUERY) {
					queryComboBox.getEditor().selectAll();
				}
			}
		});
		TitledPane resultPane = new TitledPane(title, errors);
		FXUtil.setTitledPaneIcon(resultPane, FontAwesomeIcons.TIMES);
		
		showResultPane(resultPane);
		
		errors.getSelectionModel().selectFirst();
	}
	
	private void displayQueryAnswer(String query, Expression result, ParsedHOGModel parseModel, long millisecondsToCompute) {
		String title = "Query '"+query+duration("' took ", millisecondsToCompute)+" to compute answer '"+result+"'";
		HOGMCodeArea resultCodeArea = new HOGMCodeArea(false);
		 
		resultCodeArea.setText("P("+ query + " | ... ) = "+result);
		resultCodeArea.setEditable(false);
		
		Node resultContent = null;
		if (SGSolverDemoController.isInDebugMode()) {
			HOGMCodeArea parsedModelArea = new HOGMCodeArea();
			StringJoiner sj = new StringJoiner("\n");
			if (parseModel == null) {
				sj.add("// UNABLE TO PARSE");
			}
			else {
				sj.add("// SORT DECLARATIONS:");
				parseModel.getSortDeclarations().forEach(sd -> {
					sj.add(sd.getSortDeclaration().toString()+";");
				});
				if (parseModel.getConstatDeclarations().size() > 0) {
					sj.add("// CONSTANT DECLARATIONS:");
					parseModel.getConstatDeclarations().forEach(cd -> {
						sj.add(cd.getConstantDeclaration().toString()+";");
					});
				}
				sj.add("// RANDOM VARIABLE DECLARATIONS:");
				parseModel.getRandomVariableDeclarations().forEach(rd -> {
					sj.add(rd.getRandomVariableDeclaration().toString()+";");
				});
				sj.add("// CONDITIONED POTENTIALS:");
				parseModel.getConditionedPotentials().forEach(cp -> {
					sj.add(cp.toString()+";");
				});
				parsedModelArea.setText(sj.toString());
				parsedModelArea.setEditable(false);
			}
			
			
			TabPane resultTabs = new TabPane();
			resultTabs.getTabs().add(new Tab("Answer", resultCodeArea));
			resultTabs.getTabs().add(new Tab("Parsed As", parsedModelArea));
			
			resultContent = resultTabs;
		}
		else {
			resultContent = resultCodeArea;
		}
		
		TitledPane resultPane = new TitledPane(title, resultContent);
		FXUtil.setTitledPaneIcon(resultPane, FontAwesomeIcons.CHECK);
		
		showResultPane(resultPane);
	}
	
	private void showResultPane(TitledPane resultPane) {
		resultPane.setPrefWidth(outputScrollPane.getViewportBounds().getWidth());
		outputScrollPane.viewportBoundsProperty().addListener((observer, oldValue, newValue) -> resultPane.setPrefWidth(newValue.getWidth()));
		outputAccordion.getPanes().add(0, resultPane);
		resultPane.setExpanded(true);
		outputScrollPane.setVvalue(0);
	}
	
	private String duration(String prefix, long duration) {
		long hours = 0L, minutes = 0L, seconds = 0L, milliseconds = 0L;
		
		if (duration != 0) {
			hours    = duration / 3600000;
			duration = duration % 3600000; 
		}
		if (duration != 0) {
			minutes  = duration / 60000;
			duration = duration % 60000;
		}
		if (duration != 0) {
			seconds  = duration / 1000;
			duration = duration % 1000;
		}
		milliseconds = duration;
		
		String result = prefix + hours + "h" + minutes + "m" + seconds + "." + milliseconds+"s";
		
		return result;
	}
}
