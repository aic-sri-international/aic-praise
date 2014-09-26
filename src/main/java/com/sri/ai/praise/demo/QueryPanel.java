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
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.BoxLayout;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.MutableComboBoxModel;
import javax.swing.border.EmptyBorder;

import com.google.common.annotations.Beta;

/**
 * 
 * @author oreilly
 *
 */
@Beta
public class QueryPanel extends JPanel {
	private static final long serialVersionUID = 1L;
	
	JButton btnExecuteQuery;
	JProgressBar progressBar;
	//
	private MutableComboBoxModel<String> queryModel = new DefaultComboBoxModel<String>();
	//
	private JComboBox<String> queryComboBox;

	/**
	 * Create the panel.
	 */
	public QueryPanel() {
		setBorder(new EmptyBorder(0, 5, 0, 0));
		initialize();
		postGUIInitialization();
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
	
	public void setState(QueryPanel otherQueryPanel) {
		MutableComboBoxModel otherQueryModel = otherQueryPanel.queryModel;
		for (int i = otherQueryModel.getSize()-1; i >= 0; i--) {
			addQuery(otherQueryModel.getElementAt(i).toString());
		}
	}
	
	public String getCurrentQuery() {
		return queryComboBox.getEditor().getItem().toString();
	}
	
	//
	// PRIVATE
	//
	private void initialize() {
		setLayout(new BorderLayout(0, 0));
		
		JPanel queryPanel = new JPanel();
		add(queryPanel, BorderLayout.NORTH);
		queryPanel.setBorder(null);
		queryPanel.setLayout(new BoxLayout(queryPanel, BoxLayout.X_AXIS));
		
		JLabel lblQuery = new JLabel("Query: ");
		lblQuery.setFont(new Font("SansSerif", Font.PLAIN, 12));
		queryPanel.add(lblQuery);
		
		JPanel panel = new JPanel();
		queryPanel.add(panel);
		panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
		
		queryComboBox = new JComboBox<>(queryModel);
		queryComboBox.setFont(new Font("Dialog", Font.PLAIN, 14));
		panel.add(queryComboBox);
		queryComboBox.setPreferredSize(new Dimension(250, 25));
		queryComboBox.setEditable(true);
		
		btnExecuteQuery = new JButton("");
		panel.add(btnExecuteQuery);
		btnExecuteQuery.setPreferredSize(new Dimension(40, 40));
		btnExecuteQuery.setHideActionText(true);
		btnExecuteQuery.setIcon(ImageLookup.EXECUTE_QUERY_LARGE);
		btnExecuteQuery.setToolTipText("Execute Query");
		
		progressBar = new JProgressBar();
		progressBar.setPreferredSize(new Dimension(146, 32));
		progressBar.setEnabled(false);
		queryPanel.add(progressBar);
		queryComboBox.getEditor().addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				if (btnExecuteQuery.isEnabled()) {
					addQuery(getCurrentQuery());
					btnExecuteQuery.doClick();
				}
			}
		});
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
	}
	
	private void postGUIInitialization() {
	}
}
