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
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.undo.CannotRedoException;

import org.apache.commons.lang3.exception.ExceptionUtils;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.demo.model.ChurchEg1;
import com.sri.ai.praise.demo.model.ChurchEg2;
import com.sri.ai.praise.demo.model.ChurchEg3;
import com.sri.ai.praise.demo.model.ChurchEg4;
import com.sri.ai.praise.demo.model.ChurchEg5;
import com.sri.ai.praise.demo.model.ChurchEg6;
import com.sri.ai.praise.demo.model.ChurchEg7;
import com.sri.ai.praise.demo.model.ChurchEg8;
import com.sri.ai.praise.demo.model.Example;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.imports.church.TranslateChurchToModel;
import com.sri.ai.util.base.Triple;

@Beta
public class ChurchPanel extends AbstractEditorPanel {
	private static final long serialVersionUID = 1L;	
	//
	private ChurchEditor churchEditor;
	private RuleEditor   hogmEditor;
	private File         currentChurchFile;
	// 
	private TranslateChurchToModel translator = new TranslateChurchToModel();
	private String lastChurchProgram = null;

	public ChurchPanel() {
		initialize();
	}
	
	@Override
	public List<Example> getExamples() {
		return Arrays.asList(new ChurchEg1(),
				new ChurchEg2(),
				new ChurchEg3(),
				new ChurchEg4(),
				new ChurchEg5(),
				new ChurchEg6(),
				new ChurchEg7(),
				new ChurchEg8()
				);
	}
	
	@Override
	public void setExample(Example example) {
		try {
			setContents(example.getModel(), null);
		}
		catch (IOException ioe) {
			// Should not occur but output to screen just in case.
			ioe.printStackTrace();
		}
	}
	
	@Override
	public String getContextTitle() {
		String title = "Church Program[";
		if (currentChurchFile == null) {
			title += "*";
		}
		else {
			title += currentChurchFile.getAbsolutePath(); 
		}
		title += "]";
		
		return title;
	}
	
	@Override
	public String getModel() {
		// Ensure we give back the latest model that can be generated.
		generateModel();
		return hogmEditor.getText();
	}
		
	@Override
	public void setContents(String contents, File fromFile) throws IOException {		
		churchEditor.setText(contents);
		churchEditor.discardAllEdits();
		currentChurchFile = fromFile;
		
		generateModel();		
	}
	
	@Override
	public boolean isASaveRequired() {
		return churchEditor.canUndo();
	}
	
	@Override
	public void saveIfRequired() throws IOException {
		if (churchEditor.canUndo()) {
			if (currentChurchFile == null) {
				saveAs();
			}
			else {
				saveToFile();
			}
		}
	}

	@Override
	public void saveAll() throws IOException {
		saveIfRequired();
	}
	
	@Override
	public void saveAs() throws IOException {
		if (churchEditor.canUndo()) {
			if (currentChurchFile != null) {
				fileChooser.setSelectedFile(currentChurchFile);
			}
			int returnVal = fileChooser.showSaveDialog(fileChooserParent);
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				currentChurchFile = fileChooser.getSelectedFile();
				saveToFile();
			}
		}
	}
	
	@Override
	public boolean canUndo() {
		return churchEditor.canUndo();	
	}
	
	@Override
	public void undo() {
		if (churchEditor.canUndo()) {
			try {
				churchEditor.undo();						
			} catch (CannotRedoException cue) {
				// ignore
			}
		}	
	}
	
	@Override
	public void redo() {
		if (churchEditor.canRedo()) {
			try {
				churchEditor.redo();
			} catch (CannotRedoException cue) {
				// ignore
			}
		}	
	}
	
	@Override
	public void discardAllEdits() {
		churchEditor.discardAllEdits();
	}
	
	@Override
	public void copyState(AbstractEditorPanel otherEditorPanel) {
		churchEditor.setText(((ChurchPanel)otherEditorPanel).churchEditor.getText());
	}
	
	@Override
	public List<String> validateContents() {
		List<String> problems = generateModel();
		
		return problems;
	}
	
	//
	// PRIVATE
	//
	private void initialize() {
		setLayout(new BorderLayout(0, 0));
		
		JPanel rootPanel = new JPanel();
		add(rootPanel, BorderLayout.CENTER);
		rootPanel.setLayout(new BorderLayout(0, 0));
		
		JSplitPane splitPane = new JSplitPane();
		splitPane.setContinuousLayout(true);
		splitPane.setResizeWeight(0.5);
		splitPane.setOneTouchExpandable(true);
		splitPane.setOrientation(JSplitPane.VERTICAL_SPLIT);
		rootPanel.add(splitPane, BorderLayout.CENTER);
		
		JPanel churchPanel = new JPanel();
		churchPanel.setLayout(new BorderLayout(0, 0));
		
		JLabel lblNewLabel = new JLabel("Church Program");
		churchPanel.add(lblNewLabel, BorderLayout.NORTH);
		
		churchEditor = new ChurchEditor();
		churchPanel.add(churchEditor, BorderLayout.CENTER);
		
		splitPane.setLeftComponent(churchPanel);
		
		JPanel hogmPanel = new JPanel();
		hogmPanel.setLayout(new BorderLayout(0, 0));
		
		JLabel lblNewLabel_1 = new JLabel("Generated Model (Read Only)");
		hogmPanel.add(lblNewLabel_1, BorderLayout.NORTH);
		
		hogmEditor = new RuleEditor();
		hogmEditor.setEditable(false);
		hogmPanel.add(hogmEditor, BorderLayout.CENTER);
		
		splitPane.setRightComponent(hogmPanel);
	}
	
	private List<String> generateModel() {
		List<String> problems = new ArrayList<>();
		try {
			String churchProgram = churchEditor.getText();
			Triple<String, Model, List<Expression>> translation = translator.translate("Church Program", ""
				+ churchProgram
				);
			String hogmModel = translation.first;
			hogmEditor.setText(hogmModel);
			// If there has been a change in the church program update the query field.
			if (!churchProgram.equals(lastChurchProgram)) {
				getQueryPanel().setCurrentQuery(translation.third.get(0).toString());
			}
			lastChurchProgram = churchProgram;
		} catch (Throwable t) {
			String problem = "/* ERROR in Translation:\n"+ExceptionUtils.getStackTrace(t)+"\n*/";
			problems.add(problem);
			hogmEditor.setText(problem);
		}
		return problems;
	}
	
	private void saveToFile() throws IOException {	
		if (!currentChurchFile.exists()) {
			currentChurchFile.createNewFile();
		}
		
		FileWriter fileWriter = new FileWriter(currentChurchFile);
		fileWriter.write(churchEditor.getText());
		fileWriter.close();
		
		churchEditor.discardAllEdits();
	}
}
