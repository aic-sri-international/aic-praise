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
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;

import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.praise.lbp.LBPConfiguration;
import com.sri.ai.util.Util;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;

/**
 * 
 * @author oreilly
 *
 */
@Beta
public class OptionsPanel extends JPanel {
	private static final long serialVersionUID = 1L;
	//
	private static final String SYNCHRONOUS      = "SYNCHRONOUS";
	private static final String ASYNC_INDIVIDUAL = "ASYNC INDIVIDUAL";
	private static final String ASYNC_GROUP      = "ASYNC GROUP";
	// Note: These are global options, that apply to all open windows at the same time.
	private static SpinnerNumberModel _precisionModel        = new SpinnerNumberModel(PRAiSEDemoApp.DISPLAY_PRECISION, 1, 80, 1);
	private static SpinnerNumberModel _scientificOutputModel = new SpinnerNumberModel(PRAiSEDemoApp.DISPLAY_SCIENTIFIC_AFTER, 2, 80, 1);
	{
		_precisionModel.addChangeListener(new ChangeListener() {
			
			@Override
			public void stateChanged(ChangeEvent e) {
				DefaultSymbol.setNumericDisplayPrecision((Integer)_precisionModel.getValue());				
			}
		});
		_scientificOutputModel.addChangeListener(new ChangeListener() {
			
			@Override
			public void stateChanged(ChangeEvent e) {
				DefaultSymbol.setDisplayScientificAfterNDecimalPlaces((Integer)_scientificOutputModel.getValue());	
			}
		});
	}
	//
	JFormattedTextField domainSizeTextField = null;
	JComboBox scheduleComboBox;
	JCheckBox chckbxJustificationToConsole;
	JCheckBox chckbxJustificationToJustTab;
	JCheckBox chckbxTraceToConsole;
	JCheckBox chckbxTraceToTrace;
	JCheckBox chckbxJustificationEnabled;
	JCheckBox chckbxTraceEnabled;
	JCheckBox chckbxOverrideModel;
	JCheckBox chckbxKnownDomainSize;
	JCheckBox chckbxAssumeDomainsAlwaysLarge;
	private JSpinner precisionSpinner;
	private JSpinner scientificOutputSpinner;

	/**
	 * Create the panel.
	 */
	public OptionsPanel() {
		setBorder(new EmptyBorder(0, 5, 0, 0));
		initialize();
		postGUIInitialization();
	}
	
	public LBPConfiguration.BeliefPropagationUpdateSchedule getSelectedSchedule() {
		LBPConfiguration.BeliefPropagationUpdateSchedule result = null;
		
		String scheduleName = scheduleComboBox.getItemAt(scheduleComboBox.getSelectedIndex()).toString();
		
		if (scheduleName.equals(SYNCHRONOUS)) {
			result = LBPConfiguration.BeliefPropagationUpdateSchedule.SYNCHRONOUS;
		}
		else if (scheduleName.equals(ASYNC_INDIVIDUAL)) {
			result = LBPConfiguration.BeliefPropagationUpdateSchedule.ASYNCHRONOUS_INDIVIDUAL_BASED_CYCLE_DETECTION;
		}
		else if (scheduleName.equals(ASYNC_GROUP)) {
			result = LBPConfiguration.BeliefPropagationUpdateSchedule.ASYNCHRONOUS_GROUP_BASED_CYCLE_DETECTION;
		}
		else {
			Util.fatalError("Missing Schedule Mapping.");
		}

		return result;
	}
	
	//
	// PRIVATE
	//
	private void initialize() {
		setLayout(new BorderLayout(0, 0));
		
		JPanel optionsPanel = new JPanel();
		add(optionsPanel, BorderLayout.NORTH);
		optionsPanel.setBorder(null);
		optionsPanel.setLayout(new BoxLayout(optionsPanel, BoxLayout.Y_AXIS));
		
		JLabel lblOptions = new JLabel("Options");
		lblOptions.setFont(new Font("SansSerif", Font.BOLD, 12));
		optionsPanel.add(lblOptions);
		
		JSeparator separator_5 = new JSeparator();
		optionsPanel.add(separator_5);
		
		JSeparator separator_4 = new JSeparator();
		optionsPanel.add(separator_4);
		
		JPanel schedulePanel = new JPanel();
		schedulePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		optionsPanel.add(schedulePanel);
		schedulePanel.setLayout(new BoxLayout(schedulePanel, BoxLayout.X_AXIS));
		
		JLabel lblSchedule = new JLabel("Schedule:");
		schedulePanel.add(lblSchedule);
		
		scheduleComboBox = new JComboBox();
		scheduleComboBox.setPreferredSize(new Dimension(100, 25));
		schedulePanel.add(scheduleComboBox);
		
		chckbxOverrideModel = new JCheckBox("Override Model's Domain Sizes (i.e. sort sizes)");
		chckbxOverrideModel.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (chckbxOverrideModel.isSelected()) {
					chckbxKnownDomainSize.setEnabled(true);
					domainSizeTextField.setEnabled(true);
					chckbxAssumeDomainsAlwaysLarge.setEnabled(true);
				}
				else {
					chckbxKnownDomainSize.setEnabled(false);
					domainSizeTextField.setEnabled(false);
					chckbxAssumeDomainsAlwaysLarge.setEnabled(false);
				}
			}
		});
		chckbxOverrideModel.setPreferredSize(new Dimension(18, 25));
		optionsPanel.add(chckbxOverrideModel);
		
		JPanel knownSizePanel = new JPanel();
		knownSizePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		FlowLayout flowLayout = (FlowLayout) knownSizePanel.getLayout();
		flowLayout.setVgap(0);
		flowLayout.setHgap(0);
		flowLayout.setAlignment(FlowLayout.LEFT);
		optionsPanel.add(knownSizePanel);
		
		JLabel lblNewLabel = new JLabel("      ");
		knownSizePanel.add(lblNewLabel);
		
		chckbxKnownDomainSize = new JCheckBox("Known Domain Size");
		chckbxKnownDomainSize.setSelected(true);
		chckbxKnownDomainSize.setEnabled(false);
		knownSizePanel.add(chckbxKnownDomainSize);
		
		domainSizeTextField = new JFormattedTextField();
		domainSizeTextField.setEnabled(false);
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
			}
		});
		domainSizeTextField.setPreferredSize(new Dimension(80, 25));
		domainSizeTextField.setValue(new Integer(10));
		knownSizePanel.add(domainSizeTextField);
		
		JPanel assumePanel = new JPanel();
		assumePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		FlowLayout flowLayout_1 = (FlowLayout) assumePanel.getLayout();
		flowLayout_1.setVgap(0);
		flowLayout_1.setHgap(0);
		flowLayout_1.setAlignment(FlowLayout.LEFT);
		optionsPanel.add(assumePanel);
		
		JLabel label_4 = new JLabel("      ");
		assumePanel.add(label_4);
		
		chckbxAssumeDomainsAlwaysLarge = new JCheckBox("Assume Domains Always Large");
		assumePanel.add(chckbxAssumeDomainsAlwaysLarge);
		chckbxAssumeDomainsAlwaysLarge.setPreferredSize(new Dimension(240, 25));
		
		JSeparator separator_1 = new JSeparator();
		optionsPanel.add(separator_1);
		
		chckbxJustificationEnabled = new JCheckBox("Justification Enabled");
		chckbxJustificationEnabled.setSelected(true);
		chckbxJustificationEnabled.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (chckbxJustificationEnabled.isSelected()) {
					chckbxJustificationToConsole.setEnabled(true);
					chckbxJustificationToJustTab.setEnabled(true);
				}
				else {
					chckbxJustificationToConsole.setEnabled(false);
					chckbxJustificationToJustTab.setEnabled(false);
				}
			}
		});
		chckbxJustificationEnabled.setPreferredSize(new Dimension(175, 25));
		optionsPanel.add(chckbxJustificationEnabled);
		
		JPanel justOutToConsolePanel = new JPanel();
		FlowLayout flowLayout_2 = (FlowLayout) justOutToConsolePanel.getLayout();
		flowLayout_2.setVgap(0);
		flowLayout_2.setHgap(0);
		flowLayout_2.setAlignment(FlowLayout.LEFT);
		justOutToConsolePanel.setAlignmentX(0.0f);
		optionsPanel.add(justOutToConsolePanel);
		
		JLabel label = new JLabel("      ");
		justOutToConsolePanel.add(label);
		
		chckbxJustificationToConsole = new JCheckBox("Output to Console Tab");
		chckbxJustificationToConsole.setPreferredSize(new Dimension(200, 25));
		justOutToConsolePanel.add(chckbxJustificationToConsole);
		
		JPanel justOutToJustPanel = new JPanel();
		FlowLayout flowLayout_3 = (FlowLayout) justOutToJustPanel.getLayout();
		flowLayout_3.setVgap(0);
		flowLayout_3.setHgap(0);
		flowLayout_3.setAlignment(FlowLayout.LEFT);
		justOutToJustPanel.setAlignmentX(0.0f);
		optionsPanel.add(justOutToJustPanel);
		
		JLabel label_1 = new JLabel("      ");
		justOutToJustPanel.add(label_1);
		
		chckbxJustificationToJustTab = new JCheckBox("Output to Justification Tab");
		chckbxJustificationToJustTab.setSelected(true);
		chckbxJustificationToJustTab.setPreferredSize(new Dimension(200, 25));
		justOutToJustPanel.add(chckbxJustificationToJustTab);
		
		JSeparator separator_2 = new JSeparator();
		optionsPanel.add(separator_2);
		
		chckbxTraceEnabled = new JCheckBox("Trace Enabled");
		chckbxTraceEnabled.setSelected(true);
		chckbxTraceEnabled.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (chckbxTraceEnabled.isSelected()) {
					chckbxTraceToConsole.setEnabled(true);
					chckbxTraceToTrace.setEnabled(true);
				}
				else {
					chckbxTraceToConsole.setEnabled(false);
					chckbxTraceToTrace.setEnabled(false);
				}
			}
		});
		chckbxTraceEnabled.setPreferredSize(new Dimension(141, 25));
		optionsPanel.add(chckbxTraceEnabled);
		
		JPanel traceOutToConsolePanel = new JPanel();
		FlowLayout flowLayout_4 = (FlowLayout) traceOutToConsolePanel.getLayout();
		flowLayout_4.setVgap(0);
		flowLayout_4.setHgap(0);
		flowLayout_4.setAlignment(FlowLayout.LEFT);
		traceOutToConsolePanel.setAlignmentX(0.0f);
		optionsPanel.add(traceOutToConsolePanel);
		
		JLabel label_2 = new JLabel("      ");
		traceOutToConsolePanel.add(label_2);
		
		chckbxTraceToConsole = new JCheckBox("Output to Console Tab");
		chckbxTraceToConsole.setPreferredSize(new Dimension(200, 25));
		traceOutToConsolePanel.add(chckbxTraceToConsole);
		
		JPanel traceOutputToTracePanel = new JPanel();
		FlowLayout flowLayout_5 = (FlowLayout) traceOutputToTracePanel.getLayout();
		flowLayout_5.setVgap(0);
		flowLayout_5.setHgap(0);
		flowLayout_5.setAlignment(FlowLayout.LEFT);
		traceOutputToTracePanel.setAlignmentX(0.0f);
		optionsPanel.add(traceOutputToTracePanel);
		
		JLabel label_3 = new JLabel("      ");
		traceOutputToTracePanel.add(label_3);
		
		chckbxTraceToTrace = new JCheckBox("Output to Trace Tab");
		chckbxTraceToTrace.setSelected(true);
		chckbxTraceToTrace.setPreferredSize(new Dimension(200, 25));
		traceOutputToTracePanel.add(chckbxTraceToTrace);
		
		JSeparator separator = new JSeparator();
		optionsPanel.add(separator);
		
		JPanel precisionPanel = new JPanel();
		precisionPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		optionsPanel.add(precisionPanel);
		precisionPanel.setLayout(new BorderLayout(0, 0));
		
		precisionSpinner = new JSpinner();
		precisionSpinner.setModel(_precisionModel);
		precisionPanel.add(precisionSpinner, BorderLayout.WEST);
		
		JLabel precisionLabel = new JLabel("Numeric Precision");
		precisionPanel.add(precisionLabel, BorderLayout.CENTER);
		
		JPanel scientificOutputPanel = new JPanel();
		scientificOutputPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		optionsPanel.add(scientificOutputPanel);
		scientificOutputPanel.setLayout(new BorderLayout(0, 0));
		
		scientificOutputSpinner = new JSpinner();
		scientificOutputSpinner.setModel(_scientificOutputModel);
		scientificOutputPanel.add(scientificOutputSpinner, BorderLayout.WEST);
		
		JLabel scientificLabel = new JLabel("Display Scientific After N Decimal Places");
		scientificOutputPanel.add(scientificLabel, BorderLayout.CENTER);
	}

	private void postGUIInitialization() {
		scheduleComboBox.addItem(SYNCHRONOUS);
		scheduleComboBox.addItem(ASYNC_INDIVIDUAL);
		scheduleComboBox.addItem(ASYNC_GROUP);
		
		// Don't select by default.
		chckbxJustificationEnabled.doClick();
		chckbxTraceEnabled.doClick();
	}
}
