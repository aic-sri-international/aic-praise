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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.DefaultComboBoxModel;
import javax.swing.Icon;
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
		setBorder(new EmptyBorder(0, 0, 0, 0));
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
		queryPanel.setLayout(new BorderLayout(5, 0));
		
		JLabel lblQuery = new JLabel("Query ");
		queryPanel.add(lblQuery, BorderLayout.WEST);
		
		JPanel panel = new JPanel();
		queryPanel.add(panel, BorderLayout.CENTER);
		panel.setLayout(new BorderLayout(5, 0));
		
		queryComboBox = new JComboBox<>(queryModel);
		panel.add(queryComboBox, BorderLayout.CENTER);
		queryComboBox.setEditable(true);
		queryComboBox.setToolTipText("You can press enter to execute the current query");
		
		btnExecuteQuery = new JButton(ImageLookup.EXECUTE_QUERY_SMALL) {
			private static final long serialVersionUID = 1L;
			// NOTE: Overriding this so that we use the small icons by default
			// as these match in size better with the query combo box.
			@Override
			public void setIcon(Icon defaultIcon) {				
				if (defaultIcon == ImageLookup.EXECUTE_QUERY_LARGE) {
					defaultIcon = ImageLookup.EXECUTE_QUERY_SMALL;
				}
				else if (defaultIcon == ImageLookup.STOP_QUERY_LARGE) {
					defaultIcon = ImageLookup.STOP_QUERY_SMALL;
				}
				super.setIcon(defaultIcon);
		    }
		};
		panel.add(btnExecuteQuery, BorderLayout.EAST);
		btnExecuteQuery.setPreferredSize(new Dimension(28, 28));
		btnExecuteQuery.setHideActionText(true);
		btnExecuteQuery.setIcon(ImageLookup.EXECUTE_QUERY_LARGE);
		btnExecuteQuery.setToolTipText("Execute Query");
		
		progressBar = new JProgressBar();
		progressBar.setEnabled(false);
		queryPanel.add(progressBar, BorderLayout.EAST);
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
