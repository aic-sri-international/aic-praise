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

import javax.swing.JFrame;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.demo.ExpressionEditor;
import com.sri.ai.grinder.demo.OutputPanel;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import javax.swing.JSplitPane;
import java.awt.Dimension;
import javax.swing.JTabbedPane;
import javax.swing.JMenuBar;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

/**
 * 
 * @author oreilly
 *
 */
@Beta
public class PRAiSEDemoApp {

	private JFrame frame;
	private ToolBarPanel toolBar = new ToolBarPanel();

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					String configuredLookAndFeel = GrinderConfiguration.getDemoAppDefaultLookAndFeel();
				    for (LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
				        if (configuredLookAndFeel.equals(info.getName())) {
				            UIManager.setLookAndFeel(info.getClassName());
				            break;
				        }
				    }
				} catch (Exception e) {
				    // If the configured look and feel is not available, the default system look and feel will remain in place.
				}
				
				try {
					PRAiSEDemoApp window = new PRAiSEDemoApp();
					window.frame.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Create the application.
	 */
	public PRAiSEDemoApp() {
		initialize();
	}

	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize() {
		frame = new JFrame();
		frame.setBounds(100, 100, 1000, 640);
		frame.setTitle("PRAiSE: Relational Probabilistic Inference Demo");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		JPanel backgroundPanel = new JPanel();
		frame.getContentPane().add(backgroundPanel, BorderLayout.CENTER);
		backgroundPanel.setLayout(new BorderLayout(0, 0));
		
		JSplitPane inputOutputSplitPane = new JSplitPane();
		inputOutputSplitPane.setResizeWeight(0.9);
		inputOutputSplitPane.setOrientation(JSplitPane.VERTICAL_SPLIT);
		inputOutputSplitPane.setOneTouchExpandable(true);
		backgroundPanel.add(inputOutputSplitPane, BorderLayout.CENTER);
		
		JPanel inputPanel = new JPanel();
		inputPanel.setPreferredSize(new Dimension(400, 500));
		inputOutputSplitPane.setLeftComponent(inputPanel);
		inputPanel.setLayout(new BorderLayout(0, 0));
		
		JSplitPane splitPane = new JSplitPane();
		splitPane.setOneTouchExpandable(true);
		splitPane.setResizeWeight(0.9);
		inputPanel.add(splitPane, BorderLayout.CENTER);
		
		JPanel editorPanel = new JPanel();
		editorPanel.setPreferredSize(new Dimension(400, 500));
		splitPane.setLeftComponent(editorPanel);
		editorPanel.setLayout(new BorderLayout(0, 0));
		
		JTabbedPane editorsTabbedPane = new JTabbedPane(JTabbedPane.TOP);
		editorPanel.add(editorsTabbedPane);
		
		ExpressionEditor modelEditPanel = new ExpressionEditor();
		editorsTabbedPane.addTab("Model", null, modelEditPanel, null);
		
		ExpressionEditor evidenceEditPanel = new ExpressionEditor();
		editorsTabbedPane.addTab("Evidence", null, evidenceEditPanel, null);
		
		QueryPanel queryPanel = new QueryPanel();
		queryPanel.setPreferredSize(new Dimension(300, 400));
		splitPane.setRightComponent(queryPanel);
		
		OutputPanel outputPanel = new OutputPanel();
		outputPanel.setPreferredSize(new Dimension(400, 60));
		inputOutputSplitPane.setRightComponent(outputPanel);
		
		JPanel controlPanel = new JPanel();
		controlPanel.setLayout(new BorderLayout(0, 0));
		controlPanel.add(toolBar, BorderLayout.CENTER);
		backgroundPanel.add(controlPanel, BorderLayout.NORTH);
		
		JPanel notificationPanel = new JPanel();
		backgroundPanel.add(notificationPanel, BorderLayout.SOUTH);
		
		JMenuBar menuBar = new JMenuBar();
		frame.getContentPane().add(menuBar, BorderLayout.NORTH);
		
		JMenu mnFile = new JMenu("File");
		menuBar.add(mnFile);
		
		JMenuItem mntmNew = new JMenuItem("New");
		mnFile.add(mntmNew);
		
		JMenuItem mntmOpenFile = new JMenuItem("Open File...");
		mnFile.add(mntmOpenFile);
		
		mnFile.addSeparator();
		
		JMenuItem mntmSave = new JMenuItem("Save");
		mnFile.add(mntmSave);
		
		JMenuItem mntmSaveAs = new JMenuItem("Save As...");
		mnFile.add(mntmSaveAs);
		
		JMenuItem mntmSaveAll = new JMenuItem("Save All");
		mnFile.add(mntmSaveAll);
		
		mnFile.addSeparator();
		
		JMenuItem mntmExport = new JMenuItem("Export...");
		mnFile.add(mntmExport);
		
		mnFile.addSeparator();
		
		JMenuItem mntmExit = new JMenuItem("Exit");
		mnFile.add(mntmExit);
		
		JMenu mnEdit = new JMenu("Edit");
		menuBar.add(mnEdit);
		
		JMenuItem mntmUndo = new JMenuItem("Undo");
		mnEdit.add(mntmUndo);
		
		JMenuItem mntmRedo = new JMenuItem("Redo");
		mnEdit.add(mntmRedo);
		
		JMenu mnRun = new JMenu("Run");
		menuBar.add(mnRun);
		
		JMenuItem mntmValidate = new JMenuItem("Validate Model and Evidence");
		mnRun.add(mntmValidate);
		
		JMenuItem mntmExecuteQuery = new JMenuItem("Execute Query");
		mnRun.add(mntmExecuteQuery);
		
		JMenuItem mntmClearOutput = new JMenuItem("Clear Output");
		mnRun.add(mntmClearOutput);
		
		JMenu mnWindow = new JMenu("Window");
		menuBar.add(mnWindow);
		
		JMenuItem mntmNewWindow = new JMenuItem("New Window");
		mnWindow.add(mntmNewWindow);
		
		JMenuItem mntmShowToolBar = new JMenuItem("Show Tool Bar");
		mnWindow.add(mntmShowToolBar);
	}

}
