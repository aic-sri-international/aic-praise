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

import java.awt.BorderLayout;
import java.awt.Dimension;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToolBar;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.demo.model.Example;

/**
 * 
 * @author oreilly
 *
 */
@Beta
public class ToolBarPanel extends JPanel {
	private static final long serialVersionUID = 1L;	
	//
	public JComboBox<Example> exampleComboBox;
	public JButton btnNew;
	public JButton btnOpen;
	public JButton btnSave;
	public JButton btnSaveAll;
	public JButton btnUndo;
	public JButton btnRedo;
	public JButton btnValidate;
	public JButton btnExecuteQuery;
	public JButton btnClearOutput;
	public JButton btnNewWindow;

	/**
	 * Create the panel.
	 */
	public ToolBarPanel() {
		initialize();
	}
	
	//
	// PRIVATE
	//
	private void initialize() {
		setLayout(new BorderLayout(0, 0));
		
		JToolBar toolBar = new JToolBar();
		toolBar.setFloatable(false);
		toolBar.setRollover(true);
		add(toolBar, BorderLayout.NORTH);
		
		btnNew = new JButton("");
		btnNew.setHideActionText(true);
		btnNew.setIcon(ImageLookup.NEW_LARGE);
		btnNew.setToolTipText("New");
		toolBar.add(btnNew);
		
		btnOpen = new JButton("");
		btnOpen.setHideActionText(true);
		btnOpen.setIcon(ImageLookup.OPEN_LARGE);
		btnOpen.setToolTipText("Open");
		toolBar.add(btnOpen);
		
		btnSave = new JButton("");
		btnSave.setHideActionText(true);
		btnSave.setIcon(ImageLookup.SAVE_LARGE);
		btnSave.setToolTipText("Save");
		toolBar.add(btnSave);
		
		btnSaveAll = new JButton("");
		btnSaveAll.setHideActionText(true);
		btnSaveAll.setIcon(ImageLookup.SAVE_ALL_LARGE);
		btnSaveAll.setToolTipText("Save All");
		toolBar.add(btnSaveAll);
		
		toolBar.addSeparator();
		
		JLabel lblExample = new JLabel("Example: ");
		toolBar.add(lblExample);
		
		exampleComboBox = new JComboBox<>();
		exampleComboBox.setPreferredSize(new Dimension(240, 25));
		toolBar.add(exampleComboBox);
		
		toolBar.addSeparator();
		
		btnUndo = new JButton("");
		btnUndo.setHideActionText(true);
		btnUndo.setIcon(ImageLookup.UNDO_LARGE);
		btnUndo.setToolTipText("Undo");
		toolBar.add(btnUndo);
		
		btnRedo = new JButton("");
		btnRedo.setHideActionText(true);
		btnRedo.setIcon(ImageLookup.REDO_LARGE);
		btnRedo.setToolTipText("Redo");
		toolBar.add(btnRedo);
		
		toolBar.addSeparator();
		
		btnValidate = new JButton("");
		btnValidate.setHideActionText(true);
		btnValidate.setIcon(ImageLookup.VALIDATE_LARGE);
		btnValidate.setToolTipText("Validate Model and Evidence");
		toolBar.add(btnValidate);
		
		btnExecuteQuery = new JButton("");
		btnExecuteQuery.setHideActionText(true);
		btnExecuteQuery.setIcon(ImageLookup.EXECUTE_QUERY_LARGE);
		btnExecuteQuery.setToolTipText("Execute Query");
		toolBar.add(btnExecuteQuery);
		
		btnClearOutput = new JButton("");
		btnClearOutput.setHideActionText(true);
		btnClearOutput.setIcon(ImageLookup.CLEAR_LARGE);
		btnClearOutput.setToolTipText("Clear Output");
		toolBar.add(btnClearOutput);
		
		toolBar.addSeparator();
		
		btnNewWindow = new JButton("");
		btnNewWindow.setHideActionText(true);
		btnNewWindow.setIcon(ImageLookup.NEW_WINDOW_LARGE);
		btnNewWindow.setToolTipText("New Window");
		toolBar.add(btnNewWindow);
	}
}
