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
import java.awt.EventQueue;
import java.awt.Font;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;

import org.slf4j.Marker;

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.core.DefaultWriter;
import com.sri.ai.grinder.ui.ExpressionNode;
import com.sri.ai.grinder.ui.ExpressionTreeView;
import com.sri.ai.grinder.ui.TreeUtil;
import com.sri.ai.praise.lbp.LBPQueryEngine;

@Beta
public class OutputPanel extends JPanel implements LBPQueryEngine.TraceListener, LBPQueryEngine.JustificationListener {
	private static final long serialVersionUID = 1L;
	
	//
	private ExpressionNode activeJustificationNode, rootJustificationNode = new ExpressionNode("", null);
	private DefaultTreeModel treeJustificationModel = new DefaultTreeModel(rootJustificationNode);
	private int traceCurrentIndentLevel = 0, justificationCurrentIndentLevel = 0;
	private boolean traceFirstTime = true;
	private ExpressionNode activeTraceNode, rootTraceNode = new ExpressionNode("", null);
	private DefaultTreeModel treeTraceModel = new DefaultTreeModel(rootTraceNode);
	//
	private OptionsPanel options = null;
	//
	private JTextArea consoleOutputTextArea;
	private ExpressionTreeView justificationTree;
	private ExpressionTreeView traceTree;
	private RuleEditor resultEditor;

	/**
	 * Create the panel.
	 */
	public OutputPanel() {
		initialize();
		postGUIInitialization();
	}
	
	public void clearAllOutputs() {
		resultEditor.setText("");
		consoleOutputTextArea.setText("");
		clearJustificationTree();
		clearTraceTree();
	}
	
	public void println(String line) {
		consoleOutputTextArea.append(line);
		consoleOutputTextArea.append("\n");
		consoleOutputTextArea.setCaretPosition(consoleOutputTextArea.getDocument().getLength());
	}
	
	public void setResult(String result) {
		resultEditor.setText(result);
	}
	
	public void setOptions(OptionsPanel options) {
		this.options = options;
	}
	
	//
	// START LBPQueryEngine.TraceListener
	@Override
	public void traceEvent(String queryUUID, int traceLevel, Long profileInfo, Marker marker,
			String formattedMsg, Object... args) {
		
		if (options.chckbxTraceEnabled.isSelected()) {
			StringBuilder consoleLine = new StringBuilder();
			consoleLine.append(formattedMsg);
			
			if (profileInfo != null) {
				consoleLine.append(" [");
				// Convert nanoseconds to milliseconds
				consoleLine.append(profileInfo / 1000000);
				consoleLine.append("ms.]");
			}
			
			if (options.chckbxTraceToConsole.isSelected()) {
				StringBuilder tab = new StringBuilder();
				for (int i = 0; i < traceLevel; i++) {
					tab.append("  ");
				}
				tab.append(consoleLine);		
				println(tab.toString());
			}
			
			
			if (options.chckbxTraceToTrace.isSelected()) {
				while (traceLevel > traceCurrentIndentLevel) {
					if (!traceFirstTime) {
						addTrace(">>");
					}
					startTraceLevel();
					traceCurrentIndentLevel++;
				}
				traceFirstTime = false;
				
				while (traceLevel < traceCurrentIndentLevel) {
					endTraceLevel();
					traceCurrentIndentLevel--;
				}
				
				if (formattedMsg != null && !formattedMsg.equals("")) {
					addTrace(consoleLine.toString());
				}
		
				if (args != null) {
					for (Object arg : args) {
						addTrace(arg);
					}
				}
			}
		}
	}
	// END LBPQueryEngine.TraceListener
	//
	
	// 
	// START LBPQueryEngine.JustificationListener
	@Override
	public void justificationEvent(String queryUUID, int justificationLevel, Marker marker,
			String formattedMsg, Object... args) {		
		if (options.chckbxJustificationEnabled.isSelected()) {
			StringBuilder consoleLine = new StringBuilder();
			consoleLine.append(formattedMsg);
			
			if (options.chckbxJustificationToConsole.isSelected()) {
				println(consoleLine.toString());
			}
			
			if (options.chckbxJustificationToJustTab.isSelected()) {
				while (justificationLevel > justificationCurrentIndentLevel) {
					startJustificationLevel();
					justificationCurrentIndentLevel++;
				}
				
				while (justificationLevel < justificationCurrentIndentLevel) {
					endJustificationLevel();
					justificationCurrentIndentLevel--;
				}
				
				if (formattedMsg != null && !formattedMsg.equals("")) {
					addJustification(consoleLine.toString());
				}
		
				if (args != null) {
					for (Object arg : args) {
						addJustification(arg);
					}
				}
			}
		}
	}
	// END LBPQueryEngine.JustificationListener
	//
	
	//
	// PRIVATE
	//
	private void initialize() {
		setLayout(new BorderLayout(0, 0));
		JTabbedPane outputPane = new JTabbedPane(JTabbedPane.TOP);
		add(outputPane);
		
		JPanel resultPanel = new JPanel();
		outputPane.addTab("Result", null, resultPanel, null);
		resultPanel.setLayout(new BorderLayout(0, 0));
		
		resultEditor = new RuleEditor();
		resultEditor.setEditable(false);
		resultPanel.add(resultEditor, BorderLayout.CENTER);
		
		JPanel consolePanel = new JPanel();
		outputPane.addTab("Console", null, consolePanel, null);
		consolePanel.setLayout(new BorderLayout(0, 0));
		
		JScrollPane consoleScrollPane = new JScrollPane();
		consolePanel.add(consoleScrollPane, BorderLayout.CENTER);
		
		consoleOutputTextArea = new JTextArea();
		consoleOutputTextArea.setEditable(false);
		consoleOutputTextArea.setAutoscrolls(true);
		consoleScrollPane.setViewportView(consoleOutputTextArea);
		
		JPanel justificationPanel = new JPanel();
		outputPane.addTab("Justification", null, justificationPanel, null);
		justificationPanel.setLayout(new BorderLayout(0, 0));
		
		JScrollPane justificationTreeScrollPane = new JScrollPane();
		justificationPanel.add(justificationTreeScrollPane, BorderLayout.CENTER);
		
		justificationTree = new ExpressionTreeView(treeJustificationModel, true);
		justificationTree.setEditable(true);
		justificationTree.setRootVisible(false);
		justificationTree.getSelectionModel().setSelectionMode(
				TreeSelectionModel.SINGLE_TREE_SELECTION);
		justificationTree.setShowsRootHandles(true);
		justificationTreeScrollPane.setViewportView(justificationTree);
		
		JPanel tracePanel = new JPanel();
		outputPane.addTab("Trace", null, tracePanel, null);
		tracePanel.setLayout(new BorderLayout(0, 0));
		
		JScrollPane traceTreeScrollPane = new JScrollPane();
		tracePanel.add(traceTreeScrollPane, BorderLayout.CENTER);
		
		traceTree = new ExpressionTreeView(treeTraceModel, true);
		traceTree.setEditable(true);
		traceTree.setRootVisible(false);
		traceTree.getSelectionModel().setSelectionMode(
				TreeSelectionModel.SINGLE_TREE_SELECTION);
		traceTree.setShowsRootHandles(true);
		traceTreeScrollPane.setViewportView(traceTree);
	}
	
	private void postGUIInitialization() {
		// Configure the output consoled window.
		consoleOutputTextArea.setFont(new Font(Font.MONOSPACED, consoleOutputTextArea.getFont().getStyle(), 14));

		TreeUtil.setWriter(DefaultWriter.newDefaultConfiguredWriter());
		clearTraceTree();
	}
	
	private void clearJustificationTree() {
		rootJustificationNode = new ExpressionNode("", null);
		activeJustificationNode = rootJustificationNode;
		treeJustificationModel = new DefaultTreeModel(rootJustificationNode);
		justificationTree.setModel(treeJustificationModel);
		justificationCurrentIndentLevel = 0;
	}
	
	private void clearTraceTree() {
		rootTraceNode = new ExpressionNode("", null);
		activeTraceNode = rootTraceNode;
		treeTraceModel = new DefaultTreeModel(rootTraceNode);
		traceTree.setModel(treeTraceModel);
		traceCurrentIndentLevel = 0;
		traceFirstTime = true;
	}
	
	private void startJustificationLevel() {
		if (activeJustificationNode.getChildCount() == 0) {
			activeJustificationNode = rootJustificationNode;
		} 
		else {
			activeJustificationNode = (ExpressionNode) activeJustificationNode
					.getChildAt(activeJustificationNode.getChildCount() - 1);
		}
	}
	
	private void startTraceLevel() {
		if (activeTraceNode.getChildCount() == 0) {
			activeTraceNode = rootTraceNode;
		} 
		else {
			activeTraceNode = (ExpressionNode) activeTraceNode
					.getChildAt(activeTraceNode.getChildCount() - 1);
		}
	}
	
	private void endJustificationLevel() {
		activeJustificationNode = (ExpressionNode) activeJustificationNode.getParent();
		if (activeJustificationNode == null) {
			activeJustificationNode = rootJustificationNode; 
		}
	}
	
	private void endTraceLevel() {
		activeTraceNode = (ExpressionNode) activeTraceNode.getParent();
		if (activeTraceNode == null) {
			activeTraceNode = rootTraceNode; 
		}
	}

	private void addTrace(Object obj) {
		activeTraceNode.add(obj);
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				treeTraceModel.reload();
				traceTree.restoreExpandedPaths();
			}
		});
	}
	
	private void addJustification(Object obj) {
		activeJustificationNode.add(obj);
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				treeJustificationModel.reload();
				justificationTree.restoreExpandedPaths();
			}
		});
	}
}
