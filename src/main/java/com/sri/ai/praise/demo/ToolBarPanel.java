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

import javax.swing.JPanel;
import javax.swing.JToolBar;
import java.awt.BorderLayout;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JComboBox;

import com.google.common.annotations.Beta;

import java.awt.Dimension;

/**
 * 
 * @author oreilly
 *
 */
@Beta
public class ToolBarPanel extends JPanel {
	private static final long serialVersionUID = 1L;
	private static final String ICON_RESOLUTION = "32x32";
	
	//
	private ImageIcon imageNew          = createImageIcon("document-new"+ICON_RESOLUTION+".png");
	private ImageIcon imageOpen         = createImageIcon("document-open"+ICON_RESOLUTION+".png");
	private ImageIcon imageSave         = createImageIcon("document-save"+ICON_RESOLUTION+".png");
	private ImageIcon imageSaveAs       = createImageIcon("document-save-as"+ICON_RESOLUTION+".png");
	private ImageIcon imageUndo         = createImageIcon("edit-undo"+ICON_RESOLUTION+".png");
	private ImageIcon imageRedo         = createImageIcon("edit-redo"+ICON_RESOLUTION+".png");
	private ImageIcon imageValidate     = createImageIcon("document-properties"+ICON_RESOLUTION+".png");
	private ImageIcon imageExecuteQuery = createImageIcon("media-seek-forward"+ICON_RESOLUTION+".png");
	private ImageIcon imageClear        = createImageIcon("edit-clear"+ICON_RESOLUTION+".png");
	private ImageIcon imageNewWindow    = createImageIcon("window-new"+ICON_RESOLUTION+".png");
	private JComboBox exampleComboBox;

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
		
		JButton btnNew = new JButton("");
		btnNew.setIcon(imageNew);
		btnNew.setToolTipText("New");
		toolBar.add(btnNew);
		
		JButton btnOpen = new JButton("");
		btnOpen.setIcon(imageOpen);
		btnOpen.setToolTipText("Open");
		toolBar.add(btnOpen);
		
		JButton btnSave = new JButton("");
		btnSave.setIcon(imageSave);
		btnSave.setToolTipText("Save");
		toolBar.add(btnSave);
		
		JButton btnSaveAs = new JButton("");
		btnSaveAs.setIcon(imageSaveAs);
		btnSaveAs.setToolTipText("Save As");
		toolBar.add(btnSaveAs);
		
		toolBar.addSeparator();
		
		JLabel lblExample = new JLabel("Example: ");
		toolBar.add(lblExample);
		
		exampleComboBox = new JComboBox();
		exampleComboBox.setPreferredSize(new Dimension(200, 25));
		toolBar.add(exampleComboBox);
		
		toolBar.addSeparator();
		
		JButton btnUndo = new JButton("");
		btnUndo.setIcon(imageUndo);
		btnUndo.setToolTipText("Undo");
		toolBar.add(btnUndo);
		
		JButton btnRedo = new JButton("");
		btnRedo.setIcon(imageRedo);
		btnRedo.setToolTipText("Redo");
		toolBar.add(btnRedo);
		
		toolBar.addSeparator();
		
		JButton btnValidate = new JButton("");
		btnValidate.setIcon(imageValidate);
		btnValidate.setToolTipText("Validate");
		toolBar.add(btnValidate);
		
		JButton btnExecuteQuery = new JButton("");
		btnExecuteQuery.setIcon(imageExecuteQuery);
		btnExecuteQuery.setToolTipText("Execute Query");
		toolBar.add(btnExecuteQuery);
		
		JButton btnClearOutput = new JButton("");
		btnClearOutput.setIcon(imageClear);
		btnClearOutput.setToolTipText("Clear Output");
		toolBar.add(btnClearOutput);
		
		toolBar.addSeparator();
		
		JButton btnNewWindow = new JButton("");
		btnNewWindow.setIcon(imageNewWindow);
		btnNewWindow.setToolTipText("New Window");
		toolBar.add(btnNewWindow);
	}
	
	private static ImageIcon createImageIcon(String path) {
	    java.net.URL imgURL = ToolBarPanel.class.getResource(path);
	    return new ImageIcon(imgURL);
	}

}
