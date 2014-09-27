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

import javax.swing.JPanel;

import java.awt.BorderLayout;

import javax.swing.JSplitPane;
import javax.swing.JLabel;

import com.google.common.annotations.Beta;

@Beta
public class HOGMPanel extends AbstractEditorPanel {
	private static final long serialVersionUID = 1L;	
	//
	RuleEditor modelEditPanel;
	JPanel evidencePanel;
	RuleEditor evidenceEditPanel;
	JLabel lblEvidence;
	private JPanel modelPanel;
	private JLabel lblNewLabel;

	public HOGMPanel() {
		
		initialize();
	}
	
	//
	// PRIVATE
	//
	private void initialize() {
		setLayout(new BorderLayout(0, 0));
		
		JSplitPane splitPane = new JSplitPane();
		splitPane.setContinuousLayout(true);
		splitPane.setResizeWeight(0.65);
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

}
