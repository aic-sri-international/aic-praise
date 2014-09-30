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
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.demo.model.Example;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JLabel;

@Beta
public class ChurchPanel extends AbstractEditorPanel {
	private static final long serialVersionUID = 1L;	
	//
	ChurchEditor churchEditor;
	RuleEditor   hogmEditor;

	public ChurchPanel() {
		initialize();
	}
	
	@Override
	public void setExample(Example example) {
// TODO		
	}
	
	@Override
	public String getContextTitle() {
		return null; // TODO
	}
	
	@Override
	public String getContents() {
		return null; // TODO
	}
		
	@Override
	public void setContents(String contents, File fromFile) throws IOException {
// TODO		
	}
	
	@Override
	public boolean isASaveRequired() {
		return false; // TODO
	}
	
	@Override
	public void saveIfRequired() throws IOException {
// TODO		
	}

	@Override
	public void saveAll() throws IOException {
// TODO		
	}
	
	@Override
	public void saveAs() throws IOException {
// TODO		
	}
	
	@Override
	public boolean canUndo() {
		return false; // TODO	
	}
	
	@Override
	public void undo() {
// TODO		
	}
	
	@Override
	public void redo() {
// TODO		
	}
	
	@Override
	public void discardAllEdits() {
// TODO		
	}
	
	@Override
	public void copyState(AbstractEditorPanel otherEditorPanel) {
// TODO		
	}
	
	@Override
	public List<String> validateContents() {
		List<String> problems = new ArrayList<>();
// TODO		
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
}
