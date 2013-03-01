/*
 * Copyright (c) 2013, SRI International
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
 * Neither the name of the aic-expresso nor the names of its
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
package com.sri.ai.praise.demo;

import java.awt.EventQueue;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Action;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.SwingWorker;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.UndoManager;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.Token;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeNodeStream;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParserWrapper;
import com.sri.ai.praise.demo.action.ClearOutputAction;
import com.sri.ai.praise.demo.action.ExecuteQueryAction;
import com.sri.ai.praise.demo.action.ExitAction;
import com.sri.ai.praise.demo.action.ExportAction;
import com.sri.ai.praise.demo.action.NewAction;
import com.sri.ai.praise.demo.action.HideToolBarAction;
import com.sri.ai.praise.demo.action.NewWindowAction;
import com.sri.ai.praise.demo.action.OpenFileAction;
import com.sri.ai.praise.demo.action.RedoAction;
import com.sri.ai.praise.demo.action.SaveAction;
import com.sri.ai.praise.demo.action.SaveAllAction;
import com.sri.ai.praise.demo.action.SaveAsAction;
import com.sri.ai.praise.demo.action.UndoAction;
import com.sri.ai.praise.demo.action.ValidateAction;
import com.sri.ai.praise.demo.model.Example;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.lbp.LBPQueryEngine;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.ParfactorsDeclaration;
import com.sri.ai.praise.model.RandomVariableDeclaration;
import com.sri.ai.praise.model.SortDeclaration;
import com.sri.ai.praise.rules.ReservedWordException;
import com.sri.ai.praise.rules.RuleConverter;
import com.sri.ai.praise.rules.antlr.RuleAssociativeNodeWalker;
import com.sri.ai.praise.rules.antlr.RuleLexer;
import com.sri.ai.praise.rules.antlr.RuleOutputWalker;
import com.sri.ai.praise.rules.antlr.RuleParser;
import com.sri.ai.util.base.Pair;

/**
 * 
 * @author oreilly
 *
 */
@Beta
public class Controller {

	private LBPQueryEngine queryEngine = LBPFactory.newLBPQueryEngine();
	//
	private PRAiSEDemoApp app = null;
	private RuleEditor activeEditor;
	private File currentModelFile = null;
	private File currentEvidenceFile = null;
	private UndoManager modelUndoManager = new UndoManager();
	private UndoManager evidenceUndoManager = new UndoManager();
	//
	private NewAction newAction = null;
	private OpenFileAction openFileAction = null;
	private SaveAction saveAction = null;
	private SaveAsAction saveAsAction = null;
	private SaveAllAction saveAllAction = null;
	private ExportAction exportAction = null;
	private ExitAction exitAction = null;
	private UndoAction undoAction = null;
	private RedoAction redoAction = null;
	private ValidateAction validateAction = null;
	private ExecuteQueryAction executeQueryAction = null;
	private ClearOutputAction clearOutputAction = null;
	private NewWindowAction newWindowAction = null;
	private HideToolBarAction hideToolBarAction = null;
	//
	private JFileChooser fileChooser = new JFileChooser();

	public Controller(PRAiSEDemoApp app) {
		this.app = app;
		
		queryEngine.addTraceListener(app.outputPanel);
		queryEngine.addJustificationListener(app.outputPanel);
		
		modelUndoManager.setLimit(-1);
		evidenceUndoManager.setLimit(-1);
		app.modelEditPanel.addUndoableEditListener(new UndoableEditListener() {
			
			@Override
			public void undoableEditHappened(UndoableEditEvent e) {
				modelUndoManager.undoableEditHappened(e);
				handleUndoRedo(modelUndoManager);
			}
		});
		app.evidenceEditPanel.addUndoableEditListener(new UndoableEditListener() {
			
			@Override
			public void undoableEditHappened(UndoableEditEvent e) {		
				evidenceUndoManager.undoableEditHappened(e);
				handleUndoRedo(evidenceUndoManager);
			}
		});
		
		updateAppTitle();
	}
	
	public void setActiveEditor(RuleEditor ae) {
		this.activeEditor = ae;
		handleUndoRedo(getActiveUndoManager());
	}
	
	public void setExample(Example example) {
		if (isASaveRequired()) {
			
			int option = JOptionPane.showConfirmDialog(
				    app.frame,
				    "Save existing changes before switching to "+example.getName()+"?",
				    "Save?",
				    JOptionPane.YES_NO_OPTION);
			
			if (option == JOptionPane.YES_OPTION) {
				saveAll();
			}
		}
		
		currentModelFile = null;
		currentEvidenceFile = null;
		
		app.modelEditPanel.setText(example.getModel());
		app.evidenceEditPanel.setText(example.getEvidence());
		app.queryPanel.setCurrentQuery(example.getQueryToRun());
		
		modelUndoManager.discardAllEdits();
		evidenceUndoManager.discardAllEdits();
		
		handleUndoRedo(getActiveUndoManager());
		
		updateAppTitle();
	}
	
	public void newActiveEditorContent() {
		saveIfRequired(activeEditor);
		
		activeEditor.setText("");
		discardActiveEdits();
		
		setOutputFile(activeEditor, null);
	}
	
	public void openFile() {
		int returnVal = fileChooser.showOpenDialog(app.toolBar.btnOpen);
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			File toOpen = fileChooser.getSelectedFile();
			if (toOpen.exists()) {
				
				saveIfRequired(activeEditor);
				
				try {
					LineNumberReader reader = new LineNumberReader(new FileReader(toOpen));
					StringBuilder sb = new StringBuilder();
					String line = null;
					while ( (line = reader.readLine()) != null) {
						sb.append(line);
						sb.append("\n");
					}
					reader.close();
					
					activeEditor.setText(sb.toString());
					discardActiveEdits();
					
					setOutputFile(activeEditor, toOpen);
				} catch (IOException ioe) {
					error("Unable to open file: "+toOpen.getAbsolutePath());
				}
			}
			else {
				warning("File ["+toOpen.getAbsolutePath()+"] does not exist.");
			}
		}
	}
	
	public void save() {
		saveIfRequired(activeEditor);
	}
	
	public void saveAs() {
		saveAs(activeEditor);
	}
	
	public void saveAll() {
		saveIfRequired(app.modelEditPanel);
		saveIfRequired(app.evidenceEditPanel);
	}
	
	public void export() {
// TODO		
information("Currently Not Implemented\n"+"See: http://code.google.com/p/aic-praise/wiki/ExportingModels for more information.");
	}
	
	public void exit() {
		if (isASaveRequired()) {
			
			int option = JOptionPane.showConfirmDialog(
				    app.frame,
				    "Save changes before exiting?",
				    "Save?",
				    JOptionPane.YES_NO_OPTION);
			
			if (option == JOptionPane.YES_OPTION) {
				saveAll();
			}
		}
	}
	
	public void undo() {
		UndoManager undoManager = getActiveUndoManager();
		if (undoManager.canUndo()) {
			try {
				undoManager.undo();						
			} catch (CannotRedoException cue) {
				// ignore
			}
		}
		handleUndoRedo(undoManager);
	}
	
	public void redo() {
		UndoManager undoManager = getActiveUndoManager();
		if (undoManager.canRedo()) {
			try {
				undoManager.redo();
			} catch (CannotRedoException cue) {
				// ignore
			}
		}
		handleUndoRedo(undoManager);
	}
	
	public void validate() {
		validateInput(true);
	}
	
	public void executeQuery() {
		if (validateInput(false)) {
			executeQueryAction.setEnabled(false);
			SwingWorker<String, Object> queryWorker = new SwingWorker<String, Object>() {
				@Override
				public String doInBackground() {
					try {
						printlnToConsole("ABOUT TO RUN QUERY: "+app.queryPanel.getCurrentQuery());
						System.gc();
						
						RuleConverter ruleConverter = new RuleConverter();
		
						Pair<Expression, Model> parseResult = ruleConverter
								.parseModel("'Name'", "'Description'",
										app.modelEditPanel.getText() + "\n"
												+ app.evidenceEditPanel.getText(),
										app.queryPanel.getCurrentQuery());
		
						Expression query = parseResult.first;
						Model      model = parseResult.second;
						
						String overridden = "";
						if (app.optionsPanel.chckbxOverrideModel.isSelected()) {
							model = new Model(model, app.optionsPanel.chckbxKnownDomainSize.isSelected(), 
											        new Integer(app.optionsPanel.domainSizeTextField.getText()));
							overridden = " (Sort Sizes Overridden with Specified Options)";
						}
						
						printlnToConsole("GENERATED MODEL DECLARATION" + overridden);
						printlnToConsole("---------------------------");
						printlnToConsole("SORTS=");
						for (SortDeclaration sd : model.getSortDeclarations()) {
							printlnToConsole(sd.getSortDeclaration().toString());
						}
						printlnToConsole("\nRANDOM VARIABLES=");
						for (RandomVariableDeclaration rvd : model.getRandomVariableDeclarations()) {
							printlnToConsole(rvd.getRandomVariableDeclaration().toString());
						}
						printlnToConsole("\nPARFACTORS=");
						ParfactorsDeclaration pfd = model.getParfactorsDeclaration();
						for (Expression parfactor : pfd.getParfactors()) {
							printlnToConsole(parfactor.toString());
						}
						printlnToConsole("---------------------------");
						
						printlnToConsole("GENERATED QUERY=" + query);
		
						LBPQueryEngine.QueryOptions queryOptions = new LBPQueryEngine.QueryOptions();
						// Assign the selected Options.
						queryOptions.setBeliefPropagationUpdateSchedule(app.optionsPanel.getSelectedSchedule());
						queryOptions.setJustificationsOn(app.optionsPanel.chckbxJustificationEnabled.isSelected());
						queryOptions.setTraceOn(app.optionsPanel.chckbxTraceEnabled.isSelected());
						queryOptions.setKnownDomainSizes(true); // By default.
						if (app.optionsPanel.chckbxOverrideModel.isSelected()) {
							queryOptions.setKnownDomainSizes(app.optionsPanel.chckbxKnownDomainSize.isSelected());
							GrinderConfiguration.setProperty(GrinderConfiguration.KEY_ASSUME_DOMAIN_ALWAYS_LARGE, ""+app.optionsPanel.chckbxAssumeDomainsAlwaysLarge.isSelected());
						}					
						String queryUUID = queryEngine.newQueryUUID(queryOptions);
		
						String belief = queryEngine.queryBeliefOfRandomVariable(
								queryUUID, "belief([" + query + "])",
								model.getModelDeclaration());
		
						printlnToConsole("BELIEF=\n" + belief);					
		
						app.outputPanel.setResult(belief);
					} catch (ReservedWordException rwe) {
						error("Reserved word 'query' is used input Model or Evidence");
					} catch (RuntimeException re) {
						error("Error processing inputs:\n"+re.getMessage());
						re.printStackTrace();
					}
					finally {
						executeQueryAction.setEnabled(true);
					}
					
					printlnToConsole("------");
					
					return "done";
				}
			};
			
			queryWorker.execute();
		}
	}
	
	public void clearOutput() {
		app.outputPanel.clearAllOutputs();
	}
	
	public void newWindow() {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				PRAiSEDemoApp newWindow = new PRAiSEDemoApp();
				int x = app.frame.getBounds().x + 15;
				int y = app.frame.getBounds().y + 15;
				newWindow.frame.setBounds(x, y, app.frame.getBounds().width, app.frame.getBounds().height);
				newWindow.frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
				newWindow.controller.setState(Controller.this);
				newWindow.frame.setVisible(true);
			}
		});
	}
	
	public JFrame getAppFrame() {
		return app.frame;
	}
	
	public ToolBarPanel getToolBar() {
		return app.toolBar;
	}

	public Action getNewAction() {
		if (null == newAction) {
			newAction = new NewAction(this);
		}
		return newAction;
	}
	
	public Action getOpenFileAction() {
		if (null == openFileAction) {
			openFileAction = new OpenFileAction(this);
		}
		return openFileAction;
	}
	
	public Action getSaveAction() {
		if (null == saveAction) {
			saveAction = new SaveAction(this);
		}
		return saveAction;
	}
	
	public Action getSaveAsAction() {
		if (null == saveAsAction) {
			saveAsAction = new SaveAsAction(this);
		}
		return saveAsAction;
	}
	
	public Action getSaveAllAction() {
		if (null == saveAllAction) {
			saveAllAction = new SaveAllAction(this);
		}
		return saveAllAction;
	}
	
	public Action getExportAction() {
		if (null == exportAction) {
			exportAction = new ExportAction(this);
		}
		return exportAction;
	}
	
	public Action getExitAction() {
		if (null == exitAction) {
			exitAction = new ExitAction(this);
		}
		return exitAction;
	}
	
	public Action getUndoAction() {
		if (null == undoAction) {
			undoAction = new UndoAction(this);
		}
		return undoAction;
	}
	
	public Action getRedoAction() {
		if (null == redoAction) {
			redoAction = new RedoAction(this);
		}
		return redoAction;
	}
	
	public Action getValidateAction() {
		if (null == validateAction) {
			validateAction = new ValidateAction(this);
		}
		return validateAction;
	}
	
	public Action getExecuteQueryAction() {
		if (null == executeQueryAction) {
			executeQueryAction = new ExecuteQueryAction(this);
		}
		return executeQueryAction;
	}
	
	public Action getClearOutputAction() {
		if (null == clearOutputAction) {
			clearOutputAction = new ClearOutputAction(this);
		}
		return clearOutputAction;
	}
	
	public Action getNewWindowAction() {
		if (null == newWindowAction) {
			newWindowAction = new NewWindowAction(this);
		}
		return newWindowAction;
	}

	public Action getHideToolBarAction() {
		if (null == hideToolBarAction) {
			hideToolBarAction = new HideToolBarAction(this);
		}
		return hideToolBarAction;
	}
	
	//
	// PRIVATE
	//
	private void updateAppTitle() {
		String title = "PRAiSE - Model[";
		if (currentModelFile == null) {
			title += "*";
		}
		else {
			title += currentModelFile.getAbsolutePath(); 
		}
		
		title += "]::Evidence[";
		if (currentEvidenceFile == null) {
			title += "*";
		}
		else {
			title += currentEvidenceFile.getAbsolutePath();
		}
		title += "]";
		
		app.frame.setTitle(title);
	}
	
	private UndoManager getActiveUndoManager() {
		return getUndoManager(activeEditor);
	}
	
	private UndoManager getUndoManager(RuleEditor ruleEditor) {
		UndoManager undoManager = modelUndoManager;
		if (ruleEditor == app.evidenceEditPanel) {
			undoManager = evidenceUndoManager;
		}
		return undoManager;
	}
	
	public void setOutputFile(RuleEditor editor, File outFile) {
		if (editor == app.modelEditPanel) {
			currentModelFile = outFile;
		}
		else {
			currentEvidenceFile = outFile;
		}
		updateAppTitle();
	}
	
	public File getOutputFile(RuleEditor ruleEditor) {
		File outFile = currentModelFile;
		if (ruleEditor == app.evidenceEditPanel) {
			outFile = currentEvidenceFile;
		}
		return outFile;
	}
	
	private void handleUndoRedo(UndoManager undoManager) {
		getUndoAction().setEnabled(undoManager.canUndo());
		getRedoAction().setEnabled(undoManager.canRedo());
		
		if (getActiveUndoManager().canUndo()) {
			getSaveAction().setEnabled(true);
			getSaveAsAction().setEnabled(true);
		}
		else {
			getSaveAction().setEnabled(false);
			getSaveAsAction().setEnabled(false);
		}
		
		if (isASaveRequired()) {
			getSaveAllAction().setEnabled(true);
		}
		else {
			getSaveAllAction().setEnabled(false);
		}
	}
	
	private boolean isASaveRequired() {
		return modelUndoManager.canUndo() || evidenceUndoManager.canUndo();
	}
	
	private void discardAllEdits() {
		discardEdits(app.modelEditPanel);
		discardEdits(app.evidenceEditPanel);
	}
	
	private void discardActiveEdits() {
		discardEdits(activeEditor);
	}
	
	private void discardEdits(RuleEditor editor) {
		UndoManager um = getUndoManager(editor);
		um.discardAllEdits();
		handleUndoRedo(um);
	}
	
	private void saveIfRequired(RuleEditor editor) {
		if (getUndoManager(editor).canUndo()) {
			File outFile = getOutputFile(editor);
			if (outFile == null) {
				saveAs(editor);
			}
			else {
				saveToFile(editor.getText(), outFile, editor);
			}
		}
	}
	
	private void saveAs(RuleEditor editor) {
		if (getUndoManager(editor).canUndo()) {
			File outFile = getOutputFile(editor);
			if (outFile != null) {
				fileChooser.setSelectedFile(outFile);
			}
			int returnVal = fileChooser.showSaveDialog(app.toolBar.btnSave);
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				outFile = fileChooser.getSelectedFile();
				saveToFile(editor.getText(), outFile, editor);
			}
		}
	}
	
	private void saveToFile(String text, File file, RuleEditor editor) {
		
		try {
			if (!file.exists()) {
				file.createNewFile();
			}
			
			FileWriter fileWriter = new FileWriter(file);
			fileWriter.write(text);
			fileWriter.close();
			
			discardEdits(editor);
			setOutputFile(editor, file);
		} catch (IOException ioe) {
			error("ERROR saving file:\n"+file.getAbsolutePath());
		}
	}
	
	private void printlnToConsole(String msg) {
		app.outputPanel.println(msg);
	}
	
	private void error(String message) {
		JOptionPane.showMessageDialog(app.frame, message, "Error", JOptionPane.ERROR_MESSAGE, null);
	}
	
	private void warning(String message) {
		JOptionPane.showMessageDialog(app.frame, message, "Warning", JOptionPane.WARNING_MESSAGE, null);
	}
	
	private void information(String message) {
		JOptionPane.showMessageDialog(app.frame, message, "Information", JOptionPane.INFORMATION_MESSAGE, null);
	}
	
	private void setState(Controller otherController) {
		app.modelEditPanel.setText(otherController.app.modelEditPanel.getText());
		app.evidenceEditPanel.setText(otherController.app.evidenceEditPanel.getText());
		app.queryPanel.setState(otherController.app.queryPanel);
		
		discardAllEdits();
	}
	
	private boolean validateInput(boolean displayInfoOnSuccess) {
		app.outputPanel.clearProblems();
		boolean error = false;
		int modelParseError = validateRuleParse(app.modelEditPanel.getText());
		if (modelParseError != -1) {
			app.modelEditPanel.indicateErrorAtPosition(modelParseError);
			app.outputPanel.addProblem("ERROR: Model is invalid.");
			error = true;
		}
		else {
			app.modelEditPanel.removeExistingErrorHighlights();
		}
		int evidenceParseError = validateRuleParse(app.evidenceEditPanel.getText());
		if (evidenceParseError != -1) {
			app.evidenceEditPanel.indicateErrorAtPosition(evidenceParseError);
			app.outputPanel.addProblem("ERROR: Evidence is invalid.");
			error = true;
		}
		else {
			app.evidenceEditPanel.removeExistingErrorHighlights();
		}
		int queryParseError = validateQueryParse(app.queryPanel.getCurrentQuery());
		if (queryParseError != -1) {
			app.outputPanel.addProblem("ERROR: Query is invalid.");
			error = true;
		}
		if (error || displayInfoOnSuccess) {
			if (!error) {
				app.outputPanel.addProblem("INFO: All input is valid.");
			}
			app.outputPanel.gotoProblemTab();
		}
		
		return !error;
	}
	
	private int validateRuleParse(String string) {
		int result = -1;
		if (string.trim().length() > 0) {
	    	try {
	    		CharStream cs = new ANTLRStringStream(string);
	    		RuleLexer lexer = new RuleLexer(cs);
	    		CommonTokenStream tokens = new CommonTokenStream(lexer);
	    		RuleParser parser = new RuleParser(tokens);
	    		CommonTree t = (CommonTree)parser.start().getTree();
	
	    		CommonTreeNodeStream nodes = new CommonTreeNodeStream(t);
	    		
	    		RuleAssociativeNodeWalker assoc = new RuleAssociativeNodeWalker(nodes);
	    		t = (CommonTree)assoc.downup(t);//.start().getTree();
	    		nodes = new CommonTreeNodeStream(t);
	
	    		RuleOutputWalker outputWalker = new RuleOutputWalker(nodes);
	    		outputWalker.start();
	
	    		result = -1; // All ok.
	    	}
	    	catch (RecognitionException re) {
	    		result = calculateTokenOffset(re.index, string);
	    	}
	    	catch (RuntimeException re) {
	    		result = string.length()-1; // Don't know where the parse error is.
	    		re.printStackTrace();
	    	}
		}
    	return result;
	}
	
	private int validateQueryParse(String string) {
		int result = -1;
		
		AntlrGrinderParserWrapper parser = new AntlrGrinderParserWrapper();
		if (parser.parse(string) == null) {
			result = 1;
		}
		
		return result;
	}
	
	private int calculateTokenOffset(int tokenIdx, String string) {
		List<String> lines        = new ArrayList<String>();
		List<Integer> lineOffsets = new ArrayList<Integer>();
		BufferedReader reader = new BufferedReader(new StringReader(string));
		String line;
		int offset = 0;
		try {				
			while ((line = reader.readLine()) != null) {
				lines.add(line);
				lineOffsets.add(offset);
				
				offset += line.length()+1; // i.e. include the newline.
			}
			reader.close();
		} catch (IOException ioe) {
			ioe.printStackTrace();
		}
		CharStream cs = new ANTLRStringStream(string);
		RuleLexer lexer = new RuleLexer(cs);
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		
		Token token = null;
		boolean lexerFailed = false;
		int consumed = 1;
		try {
			token = tokens.LT(1);
		} catch (RuntimeException ex) {
			lexerFailed = true;
		}
		while (!lexerFailed && token.getType() != RuleLexer.EOF) {   			
			offset = lineOffsets.get(token.getLine()-1) + token.getCharPositionInLine();
			try {
    			tokens.consume();
    			consumed++;
    			if (consumed >= tokenIdx) {
    				break;
    			}
				token = tokens.LT(1);
			} catch (RuntimeException ex) {
				// ignore and exit.
				lexerFailed = true;
			}
		}
		
		return offset;
	}
}