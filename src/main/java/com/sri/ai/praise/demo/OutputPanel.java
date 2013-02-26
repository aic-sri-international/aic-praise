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
import java.io.PrintStream;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;

import org.slf4j.ILoggerFactory;
import org.slf4j.LoggerFactory;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.core.DefaultWriter;
import com.sri.ai.grinder.helper.RewriterLoggingNamedRewriterFilter;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.ui.ExpressionNode;
import com.sri.ai.grinder.ui.ExpressionTreeView;
import com.sri.ai.grinder.ui.TreeUtil;
import com.sri.ai.util.log.LogX;

@Beta
public class OutputPanel extends JPanel {
	private static final long serialVersionUID = 1L;
	
	//
	@SuppressWarnings("unused")
	private ExpressionNode activeJustificationNode, rootJustificationNode = new ExpressionNode("", null);
	private DefaultTreeModel treeJustificationModel = new DefaultTreeModel(rootJustificationNode);
	private ExpressionNode activeTraceNode, rootTraceNode = new ExpressionNode("", null);
	private DefaultTreeModel treeTraceModel = new DefaultTreeModel(rootTraceNode);
	//
	//
	private LoggerContext               loggerContext = null;
	private AppenderBase<ILoggingEvent> traceAppender = null;
	//
	private JTextArea consoleOutputTextArea;
	private ExpressionTreeView justificationTree;
	private ExpressionTreeView traceTree;

	/**
	 * Create the panel.
	 */
	public OutputPanel() {
		initialize();
		postGUIInitialization();
	}
	
	public void clearAllOutputs() {
		consoleOutputTextArea.setText("");
		clearJustificationTree();
		clearTraceTree();
	}
	
	/**
	 * Provides a print stream which can be used to redirect standard output
	 * streams.
	 */
	public PrintStream getConsoleOutputPrintStream() {
		return new PrintStream(new ConsoleOutputStream());
	}
	
	public void notifySelected() {
		if (loggerContext == null) {
			while (loggerContext == null) {
				ILoggerFactory lf = LoggerFactory.getILoggerFactory();
				if (lf instanceof LoggerContext) {
					loggerContext = (LoggerContext) lf;
				} 
				else {
					try {
						Thread.sleep(100);
					} catch (Exception ex) {
						
					}
				}
			}
			traceAppender = new AppenderBase<ILoggingEvent>() {
				//
				private int currentIndentLevel = 0;
				private boolean firstTime = true;
				//
				@Override
				protected void append(ILoggingEvent eventObject) {
					String msg = eventObject.getFormattedMessage();
					Object[] args = eventObject.getArgumentArray();

					int indentLevel = LogX.getTraceLevel(eventObject.getLoggerName());

					while (indentLevel > currentIndentLevel) {
						if (!firstTime) {
							addTrace(">>");
						}
						startTraceLevel();
						currentIndentLevel++;
					}
					
					firstTime = false;

					StringBuilder sb = new StringBuilder(msg);
					while (indentLevel < currentIndentLevel) {
						endTraceLevel();
						currentIndentLevel--;
					}
					
					// Suffix the profiler information to the message
					// if available.
					Long profileInfo = LogX.getProfileInfo(eventObject.getLoggerName());
					if (profileInfo != null) {
						sb.append(" [");
						// Convert nanoseconds to milliseconds
						sb.append(profileInfo / 1000000);
						sb.append("ms.]");
					}

					if (msg != null && !msg.equals("")) {
						addTrace(sb.toString());
					}

					if (args != null) {
						for (Object arg : args) {
							addTrace(arg);
						}
					}
				}
			};
			
			traceAppender.addFilter(new RewriterLoggingNamedRewriterFilter());
			traceAppender.setContext(loggerContext);
			loggerContext.getLogger(Trace.getDefaultLoggerName()).addAppender(traceAppender);
		}
		
		traceAppender.start();
	}
	
	public void notifyUnselected() {
		traceAppender.stop();
	}
	
	//
	// PRIVATE
	//
	private void initialize() {
		setLayout(new BorderLayout(0, 0));
		JTabbedPane outputPane = new JTabbedPane(JTabbedPane.TOP);
		add(outputPane);
		
		JPanel consolePanel = new JPanel();
		outputPane.addTab("Console", null, consolePanel, null);
		consolePanel.setLayout(new BorderLayout(0, 0));
		
		JScrollPane consoleScrollPane = new JScrollPane();
		consolePanel.add(consoleScrollPane, BorderLayout.CENTER);
		
		consoleOutputTextArea = new JTextArea();
		consoleOutputTextArea.setEditable(false);
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
	}
	
	private void clearTraceTree() {
		rootTraceNode = new ExpressionNode("", null);
		activeTraceNode = rootTraceNode;
		treeTraceModel = new DefaultTreeModel(rootTraceNode);
		traceTree.setModel(treeTraceModel);
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
	
	/** Writes everything into the text area. */
	private class ConsoleOutputStream extends java.io.OutputStream {
		@Override
		public void write(int b) throws java.io.IOException {
			String s = new String(new char[] { (char) b });
			consoleOutputTextArea.append(s);
		}
	}
}
