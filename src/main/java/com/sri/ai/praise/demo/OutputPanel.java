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

import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.SwingConstants;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;

import org.slf4j.Marker;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.ui.BaseTreeUtilAppender;
import com.sri.ai.grinder.ui.ExpressionNode;
import com.sri.ai.grinder.ui.ExpressionTreeView;
import com.sri.ai.praise.lbp.LBPQueryEngine;

@Beta
public class OutputPanel extends JPanel implements LBPQueryEngine.TraceListener, LBPQueryEngine.JustificationListener {
	private static final long serialVersionUID = 1L;
	//
	private static final int RESULT_TAB_IDX  = 0;
	private static final int PROBLEM_TAB_IDX = 1;
	
	//
	private ExpressionNode activeJustificationNode, rootJustificationNode = new ExpressionNode("", null);
	private DefaultTreeModel treeJustificationModel = new DefaultTreeModel(rootJustificationNode);
	private int traceCurrentIndentLevel = 0, justificationCurrentIndentLevel = 0;
	private ExpressionNode activeTraceNode, rootTraceNode = new ExpressionNode("", null);
	private DefaultTreeModel treeTraceModel = new DefaultTreeModel(rootTraceNode);
	private DefaultListModel<String> problemListModel = new DefaultListModel<>();
	//
	private OptionsPanel options = null;
	//
	private JTextArea consoleOutputTextArea;
	private ExpressionTreeView justificationTree;
	private ExpressionTreeView traceTree;
	private RuleEditor resultEditor;
	private JList<String> problemsList;
	private JTabbedPane outputTabbedPane;

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
		resultEditor.discardAllEdits();
	}
	
	public void setOptions(OptionsPanel options) {
		this.options = options;
	}
	
	public void clearProblems() {
		problemListModel.removeAllElements();
	}
	
	public void addProblem(String problem) {
		problemListModel.addElement(problem);
	}
	
	public void gotoProblemTab() {
		outputTabbedPane.setSelectedIndex(PROBLEM_TAB_IDX);
	}
	
	public void switchToResultTabIfOnProblemTab() {
		if (outputTabbedPane.getSelectedIndex() == PROBLEM_TAB_IDX) {
			outputTabbedPane.setSelectedIndex(RESULT_TAB_IDX);
		}
	}
	
	//
	// START LBPQueryEngine.TraceListener
	@Override
	public void traceEvent(String queryUUID, int traceLevel, Long rootProfileInfo, Long profileInfo, Marker marker,
			String formattedMsg, Object... args) {
		
		if (options.chckbxTraceEnabled.isSelected()) {
			StringBuilder consoleLine = new StringBuilder();
			
			if (profileInfo != null) {
				consoleLine.append("[");
				// Convert nanoseconds to milliseconds
				consoleLine.append(profileInfo / 1000000);
				consoleLine.append("ms");
				if (rootProfileInfo != null) {
					consoleLine.append(", ");
					// Convert nanoseconds to milliseconds
					consoleLine.append(rootProfileInfo / 1000000);
					consoleLine.append("ms total");
				}
				
				consoleLine.append("]");
			}
			
			consoleLine.append(formattedMsg);

			if (options.chckbxTraceToConsole.isSelected()) {
				StringBuilder tab = new StringBuilder();
				for (int i = 0; i < traceLevel; i++) {
					tab.append("  ");
				}
				tab.append(consoleLine);		
				println(tab.toString());
			}
			
			
			if (options.chckbxTraceToTrace.isSelected()) {
				String outputMsg = null;
				if (formattedMsg != null && !formattedMsg.equals("") && BaseTreeUtilAppender.outputFormattedMessage(formattedMsg, args)) {
					outputMsg = consoleLine.toString();
				}
				
				while (traceCurrentIndentLevel < traceLevel) {
					if (traceCurrentIndentLevel == (traceLevel-1)) {
						if (outputMsg != null) {
							addTrace(outputMsg);
							outputMsg = null; // indicates already output
						}
						else {
							addTrace(">>");
						}
					}
					else {
						addTrace(">>");
					}
					startTraceLevel();
					traceCurrentIndentLevel++;
				}
				
				while (traceCurrentIndentLevel > traceLevel) {
					endTraceLevel();
					traceCurrentIndentLevel--;
				}
				
				if (outputMsg != null) {
					addTrace(outputMsg);
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
				
				if (formattedMsg != null && !formattedMsg.equals("") && BaseTreeUtilAppender.outputFormattedMessage(formattedMsg, args)) {
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
		outputTabbedPane = new JTabbedPane(SwingConstants.TOP);
		add(outputTabbedPane);
		
		JPanel resultPanel = new JPanel();
		outputTabbedPane.addTab("Result", null, resultPanel, null);
		resultPanel.setLayout(new BorderLayout(0, 0));
		
		resultEditor = new RuleEditor();
		resultEditor.setEditable(false);
		resultEditor.setLineNumbersEnabled(false);
		resultEditor.setHighlightCurrentLine(false);
		resultPanel.add(resultEditor, BorderLayout.CENTER);
		
		JPanel problemsPanel = new JPanel();
		outputTabbedPane.addTab("Problems", null, problemsPanel, null);
		problemsPanel.setLayout(new BorderLayout(0, 0));
		
		JScrollPane problemsScrollPane = new JScrollPane();
		problemsPanel.add(problemsScrollPane, BorderLayout.CENTER);
		
		problemsList = new JList<>(problemListModel);
		problemsScrollPane.setViewportView(problemsList);
		
		JPanel consolePanel = new JPanel();
		outputTabbedPane.addTab("Console", null, consolePanel, null);
		consolePanel.setLayout(new BorderLayout(0, 0));
		
		JScrollPane consoleScrollPane = new JScrollPane();
		consolePanel.add(consoleScrollPane, BorderLayout.CENTER);
		
		consoleOutputTextArea = new JTextArea();
		consoleOutputTextArea.setEditable(false);
		consoleOutputTextArea.setAutoscrolls(true);
		consoleScrollPane.setViewportView(consoleOutputTextArea);
		
		JPanel justificationPanel = new JPanel();
		outputTabbedPane.addTab("Justification", null, justificationPanel, null);
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
		
		justificationPanel.add(justificationTree.getFindPanel(), BorderLayout.SOUTH);
		
		JPanel tracePanel = new JPanel();
		outputTabbedPane.addTab("Trace", null, tracePanel, null);
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
		
		tracePanel.add(traceTree.getFindPanel(), BorderLayout.SOUTH);
	}
	
	private void postGUIInitialization() {
		// Configure the output consoled window.
		consoleOutputTextArea.setFont(new Font(Font.MONOSPACED, consoleOutputTextArea.getFont().getStyle(), 14));

		clearTraceTree();
		clearJustificationTree();
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
	}
	
	private void startJustificationLevel() {
		if (activeJustificationNode.getChildCount() == 0) {
			addJustification(">>");
		}
		
		activeJustificationNode = (ExpressionNode) activeJustificationNode
					.getChildAt(activeJustificationNode.getChildCount() - 1);

	}
	
	private void startTraceLevel() {
		if (activeTraceNode.getChildCount() == 0) {
			addTrace(">>");
		} 

		activeTraceNode = (ExpressionNode) activeTraceNode
					.getChildAt(activeTraceNode.getChildCount() - 1);

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
			@Override
			public void run() {
				treeTraceModel.reload();
				traceTree.restoreExpandedPaths();
			}
		});
	}
	
	private void addJustification(Object obj) {
		activeJustificationNode.add(obj);
		EventQueue.invokeLater(new Runnable() {
			@Override
			public void run() {
				treeJustificationModel.reload();
				justificationTree.restoreExpandedPaths();
			}
		});
	}
}
