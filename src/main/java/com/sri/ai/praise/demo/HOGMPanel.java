/*
 * Copyright (c) 2014, SRI International
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
package com.sri.ai.praise.demo;

import java.awt.BorderLayout;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.undo.CannotRedoException;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.demo.model.EarthquakeBurglaryAlarm;
import com.sri.ai.praise.demo.model.EpidemicAndSickDemo;
import com.sri.ai.praise.demo.model.Example;
import com.sri.ai.praise.demo.model.Example7;
import com.sri.ai.praise.demo.model.RelationalEarthquakeBurglaryAlarm;
import com.sri.ai.praise.rules.antlr.RuleLexer;
import com.sri.ai.praise.rules.antlr.RuleParser;

@Beta
public class HOGMPanel extends AbstractEditorPanel {
	private static final long serialVersionUID = 1L;	
	//
	JSplitPane splitPane;
	RuleEditor modelEditPanel;
	JPanel evidencePanel;
	RuleEditor evidenceEditPanel;
	JLabel lblEvidence;
	JPanel modelPanel;
	JLabel lblNewLabel;
	//
	private RuleEditor activeEditor        = null;
	private File       currentModelFile    = null;
	private File       currentEvidenceFile = null;

	public HOGMPanel() {
		
		initialize();
		
		postGUIInitialization();
	}
	
	@Override
	public List<Example> getExamples() {
		return Arrays.asList(new EarthquakeBurglaryAlarm(), 
				new RelationalEarthquakeBurglaryAlarm(), 
				new EpidemicAndSickDemo(),
				new Example7());
	}
	
	@Override
	public void setExample(Example example) {
		currentModelFile = null;
		currentEvidenceFile = null;
		
		modelEditPanel.setText(example.getModel());
		evidenceEditPanel.setText(example.getEvidence());
	}
	
	@Override
	public String getContextTitle() {
		String title = "Model[";
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
		
		return title;
	}
	
	@Override
	public String getModel() {
		return modelEditPanel.getText() + "\n" + evidenceEditPanel.getText();
	}
	
	@Override
	public void setContents(String contents, File fromFile) throws IOException {
		saveIfRequired();
		
		activeEditor.setText(contents);
		activeEditor.discardAllEdits();
		
		setOutputFile(activeEditor, fromFile);
	}
	
	@Override
	public boolean isASaveRequired() {
		return modelEditPanel.canUndo() || evidenceEditPanel.canUndo();
	}
	
	@Override
	public void saveIfRequired() throws IOException {
		saveIfRequired(activeEditor);
	}
	
	@Override
	public void saveAll() throws IOException {
		saveIfRequired(modelEditPanel);
		saveIfRequired(evidenceEditPanel);
	}
	
	@Override
	public void saveAs() throws IOException {
		saveAs(activeEditor);
	}
	
	@Override
	public boolean canUndo() {
		return activeEditor.canUndo();
	}
	
	@Override
	public void undo() {
		if (activeEditor.canUndo()) {
			try {
				activeEditor.undo();						
			} catch (CannotRedoException cue) {
				// ignore
			}
		}	
	}
	
	@Override
	public void redo() {
		if (activeEditor.canRedo()) {
			try {
				activeEditor.redo();
			} catch (CannotRedoException cue) {
				// ignore
			}
		}	
	}
	
	@Override
	public void discardAllEdits() {	
		modelEditPanel.discardAllEdits();
		evidenceEditPanel.discardAllEdits();	
	}
	
	@Override
	public void copyState(AbstractEditorPanel otherEditorPanel) {
		modelEditPanel.setText(((HOGMPanel)otherEditorPanel).modelEditPanel.getText());
		evidenceEditPanel.setText(((HOGMPanel)otherEditorPanel).evidenceEditPanel.getText());	
	}
	
	@Override
	public List<String> validateContents() {
		List<String> problems = new ArrayList<>();
		if (validRuleParse(problems, "ERROR in MODEL: ", modelEditPanel, true)) {
			modelEditPanel.removeExistingErrorHighlights();
		}
		if (validRuleParse(problems, "ERROR in Evidence: ", evidenceEditPanel, true)) {
			evidenceEditPanel.removeExistingErrorHighlights();
		}	
		return problems;
	}
	
	//
	// PRIVATE
	//
	private void initialize() {
		setLayout(new BorderLayout(0, 0));

		splitPane = new JSplitPane();
		splitPane.setContinuousLayout(true);
		splitPane.setResizeWeight(0.85);
		splitPane.setOneTouchExpandable(true);
		splitPane.setOrientation(JSplitPane.VERTICAL_SPLIT);
		
		modelPanel = new JPanel();
		modelPanel.setLayout(new BorderLayout(0, 0));
		
		modelEditPanel = new RuleEditor();
		modelPanel.add(modelEditPanel, BorderLayout.CENTER);
		
		lblNewLabel = new JLabel("Model");
		modelPanel.add(lblNewLabel, BorderLayout.NORTH);
		splitPane.setLeftComponent(modelPanel);
		
		evidencePanel = new JPanel();		
		evidencePanel.setLayout(new BorderLayout(0, 0));
		evidenceEditPanel = new RuleEditor();
		evidencePanel.add(evidenceEditPanel, BorderLayout.CENTER);
		
		lblEvidence = new JLabel("Evidence");
		evidencePanel.add(lblEvidence, BorderLayout.NORTH);			
		splitPane.setRightComponent(evidencePanel);
		
		add(splitPane, BorderLayout.CENTER);
	}
	
	private void postGUIInitialization() {
		// Default to the model editor as being the active editor.
		activeEditor = modelEditPanel;
		// Switch the active editor based on focus gained events.
		modelEditPanel.addTextAreaFocusListener(new FocusAdapter() {
			@Override
			public void focusGained(FocusEvent e) {
				activeEditor = modelEditPanel;
				notifyActiveEditorListeners();
			}
		});
		evidenceEditPanel.addTextAreaFocusListener(new FocusAdapter() {
			@Override
			public void focusGained(FocusEvent e) {
				activeEditor = evidenceEditPanel;
				notifyActiveEditorListeners();
			}
		});
	}
	
	private void setOutputFile(RuleEditor editor, File outFile) {
		if (editor == modelEditPanel) {
			currentModelFile = outFile;
		}
		else {
			currentEvidenceFile = outFile;
		}
	}
	
	private File getOutputFile(RuleEditor ruleEditor) {
		File outFile = currentModelFile;
		if (ruleEditor == evidenceEditPanel) {
			outFile = currentEvidenceFile;
		}
		return outFile;
	}
	
	private void saveIfRequired(RuleEditor editor) throws IOException {
		if (editor.canUndo()) {
			File outFile = getOutputFile(editor);
			if (outFile == null) {
				saveAs(editor);
			}
			else {
				saveToFile(editor.getText(), outFile, editor);
			}
		}
	}
	
	private void saveAs(RuleEditor editor) throws IOException {
		if (editor.canUndo()) {
			File outFile = getOutputFile(editor);
			if (outFile != null) {
				fileChooser.setSelectedFile(outFile);
			}
			int returnVal = fileChooser.showSaveDialog(fileChooserParent);
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				outFile = fileChooser.getSelectedFile();
				saveToFile(editor.getText(), outFile, editor);
			}
		}		
	}
	
	private void saveToFile(String text, File file, RuleEditor editor) throws IOException {	
		if (!file.exists()) {
			file.createNewFile();
		}
		
		FileWriter fileWriter = new FileWriter(file);
		fileWriter.write(text);
		fileWriter.close();
		
		editor.discardAllEdits();
		setOutputFile(editor, file);
	}
	
	private boolean validRuleParse(List<String> problems, final String errorPrefix, final RuleEditor ruleEditor, boolean calculateErrorBeginIndex) {
		final AtomicBoolean result = new AtomicBoolean(true);
		// Ensure at least one token exists, i.e. could be all comments or whitespace.
		if (containsRules(ruleEditor.getText())) {		
	    	
    		ANTLRInputStream input = new ANTLRInputStream(ruleEditor.getText());
    		RuleLexer lexer = new RuleLexer(input);
    		CommonTokenStream tokens = new CommonTokenStream(lexer);
    		RuleParser parser = new RuleParser(tokens);
    		parser.removeErrorListeners();
    		parser.addErrorListener(new BaseErrorListener() {
				@Override
				public void syntaxError(Recognizer<?, ?> recognizer,
						Object offendingSymbol, int line,
						int charPositionInLine, String msg,
						RecognitionException e) {
					
					// Highlight the first error found
					if (result.get()) {
						int[] startEnd = calculateLineOffsets(line, ruleEditor.getText());
						int start = startEnd[0] + charPositionInLine;
						int end   = startEnd[1];
						ruleEditor.indicateErrorAtPosition(start, end);
					}
					problems.add(errorPrefix+msg);
					
					// Indicate a valid parse did not occur
					result.set(false);
				}
    		});
    		parser.model();
		}
		
		return result.get();
	}
	
	/**
	 * Checks to ensure the passed in string is not whitespace or comments only.
	 * @param string
	 * @return
	 */
	private boolean containsRules(String string) {
		boolean result = true;
		
		ANTLRInputStream input = new ANTLRInputStream(string.trim());
		RuleLexer lexer = new RuleLexer(input);
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		try {
			Token token = tokens.LT(1);
			if (token.getType() == Recognizer.EOF) {
				result = false;
			}
		} catch (RuntimeException ex) {
			// This is another problem, i.e. invalid token, so will let follow
			// on logic handle this when it tries to parse.
		}
		
		return result;
	}
	
	private int[] calculateLineOffsets(int parseLine, String string) {	
		BufferedReader reader = new BufferedReader(new StringReader(string));
		String line;
		int[] startEnd = new int[2];
		try {	
			int cnt = 0;
			while ((line = reader.readLine()) != null) {
				cnt++;
				if (cnt > parseLine) {
					break;
				}
				startEnd[0] =  startEnd[1];
				startEnd[1] += line.length()+1; // i.e. include the newline.
			}
			reader.close();
		} catch (IOException ioe) {
			ioe.printStackTrace();
		}
		
		return startEnd;
	}
}
