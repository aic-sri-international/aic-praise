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
import java.awt.BorderLayout;
import java.awt.Dimension;

import javax.swing.border.TitledBorder;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JButton;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.MutableComboBoxModel;

import java.awt.FlowLayout;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.JFormattedTextField;
import java.awt.Component;

import com.google.common.annotations.Beta;
import javax.swing.JLabel;

/**
 * 
 * @author oreilly
 *
 */
@Beta
public class QueryPanel extends JPanel {
	private static final long serialVersionUID = 1L;
	
	JButton btnExecuteQuery;
	//
	private MutableComboBoxModel queryModel = new DefaultComboBoxModel();
	//
	private JFormattedTextField domainSizeTextField = null;
	private RuleEditor resultEditor = null;
	private JComboBox queryComboBox;

	/**
	 * Create the panel.
	 */
	public QueryPanel() {
		initialize();
	}

	public void setCurrentQuery(String query) {
		int idx = 0;
		boolean exists = false;
		for (int i = 0; i < queryModel.getSize(); i++) {
			if (query.equals(queryModel.getElementAt(i))) {
				idx = i;
				exists = true;
				break;
			}
		}
		if (!exists) {
			queryModel.insertElementAt(query, 0);
		}
		queryComboBox.setSelectedIndex(idx);
	}
	
	public void addQuery(String query) {
		boolean exists = false;	
		for (int i = 0; i < queryModel.getSize(); i++) {
			if (query.equals(queryModel.getElementAt(i))) {
				// Move the query to the top of the list 
				String existingQuery = queryModel.getElementAt(i).toString();
				queryModel.removeElementAt(i);
				queryModel.insertElementAt(existingQuery, 0);
				exists = true;
				break;
			}
		}
		if (!exists) {
			queryModel.insertElementAt(query, 0);
		}
		queryComboBox.setSelectedIndex(0);		
	}
	
	public String getCurrentQuery() {
		return queryComboBox.getEditor().getItem().toString();
	}
	
	public void setResult(String result) {
		resultEditor.setText(result);
	}
	
	//
	// PRIVATE
	//
	private void initialize() {
		setLayout(new BorderLayout(0, 0));
		
		JPanel queryAndOptionsPanel = new JPanel();
		add(queryAndOptionsPanel, BorderLayout.NORTH);
		queryAndOptionsPanel.setLayout(new BorderLayout(0, 0));
		
		JPanel queryPanel = new JPanel();
		queryPanel.setBorder(new TitledBorder(null, "Query", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		queryAndOptionsPanel.add(queryPanel, BorderLayout.NORTH);
		queryPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));
		
		queryComboBox = new JComboBox(queryModel);
		queryComboBox.setPreferredSize(new Dimension(250, 25));
		queryComboBox.setEditable(true);
		queryComboBox.getEditor().getEditorComponent().addFocusListener(new FocusListener() {
			
			@Override
			public void focusLost(FocusEvent e) {
				addQuery(getCurrentQuery());
			}
			
			@Override
			public void focusGained(FocusEvent e) {
				// Do nothing
			}
		});
		queryPanel.add(queryComboBox);
		
		btnExecuteQuery = new JButton("");
		btnExecuteQuery.setPreferredSize(new Dimension(40, 36));
		btnExecuteQuery.setHideActionText(true);
		btnExecuteQuery.setIcon(ImageLookup.EXECUTE_QUERY_LARGE);
		btnExecuteQuery.setToolTipText("Execute Query");
		queryPanel.add(btnExecuteQuery);
		
		JPanel optionsPanel = new JPanel();
		optionsPanel.setBorder(new TitledBorder(null, "Options", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		queryAndOptionsPanel.add(optionsPanel, BorderLayout.CENTER);
		optionsPanel.setLayout(new BoxLayout(optionsPanel, BoxLayout.Y_AXIS));
		
		JCheckBox chckbxNewCheckBox = new JCheckBox("Override Model's Domain Sizes (i.e. sort sizes)");
		chckbxNewCheckBox.setPreferredSize(new Dimension(18, 25));
		optionsPanel.add(chckbxNewCheckBox);
		
		JPanel knownSizePanel = new JPanel();
		knownSizePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		FlowLayout flowLayout = (FlowLayout) knownSizePanel.getLayout();
		flowLayout.setVgap(0);
		flowLayout.setHgap(0);
		flowLayout.setAlignment(FlowLayout.LEFT);
		optionsPanel.add(knownSizePanel);
		
		JLabel lblNewLabel = new JLabel("      ");
		knownSizePanel.add(lblNewLabel);
		
		JCheckBox chckbxKnownDomainSize = new JCheckBox("Known Domain Size");
		knownSizePanel.add(chckbxKnownDomainSize);
		
		domainSizeTextField = new JFormattedTextField();
		domainSizeTextField.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				Integer size = new Integer(10);
				try {
					size = new Integer(domainSizeTextField.getText());
					if (size < 1) {
						size = 1;
						domainSizeTextField.setValue(size);
					}
				} catch (NumberFormatException nfe) {
					domainSizeTextField.setValue(size);
				}
// TODO				
				//getOptions().setDomainSize(size);
			}
		});
		domainSizeTextField.setPreferredSize(new Dimension(80, 25));
		domainSizeTextField.setValue(new Integer(10));
		knownSizePanel.add(domainSizeTextField);
		
		JCheckBox chckbxAssumeDomainsAlwaysLarge = new JCheckBox("Assume Domains Always Large");
		chckbxAssumeDomainsAlwaysLarge.setPreferredSize(new Dimension(198, 25));
		optionsPanel.add(chckbxAssumeDomainsAlwaysLarge);
		
		JCheckBox chckbxJustificationEnabled = new JCheckBox("Justification Output Enabled");
		chckbxJustificationEnabled.setPreferredSize(new Dimension(175, 25));
		optionsPanel.add(chckbxJustificationEnabled);
		
		JCheckBox chckbxTraceEnabled = new JCheckBox("Trace Output Enabled");
		chckbxTraceEnabled.setPreferredSize(new Dimension(141, 25));
		optionsPanel.add(chckbxTraceEnabled);
		
		JPanel resultPanel = new JPanel();
		resultPanel.setBorder(new TitledBorder(null, "Result", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		add(resultPanel, BorderLayout.CENTER);
		resultPanel.setLayout(new BorderLayout(0, 0));
		
		resultEditor = new RuleEditor();
		resultEditor.setEditable(false);
		resultPanel.add(resultEditor, BorderLayout.CENTER);
	}
}
