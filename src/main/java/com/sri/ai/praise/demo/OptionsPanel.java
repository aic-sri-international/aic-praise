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
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.praise.lbp.LBPConfiguration;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.util.Configuration;
import com.sri.ai.util.Util;

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
	private static SpinnerNumberModel _precisionModel               = new SpinnerNumberModel(PRAiSEDemoApp.DISPLAY_PRECISION, 1, 80, 1);
	private static SpinnerNumberModel _scientificGreaterOutputModel = new SpinnerNumberModel(PRAiSEDemoApp.DISPLAY_SCIENTIFIC_GREATER, 2, 80, 1);
	private static SpinnerNumberModel _scientificAfterOutputModel   = new SpinnerNumberModel(PRAiSEDemoApp.DISPLAY_SCIENTIFIC_AFTER, 2, 80, 1);
	private static SpinnerNumberModel _deadEndsCacheSizeModel       = new SpinnerNumberModel(PRAiSEDemoApp.DEAD_ENDS_CACHE_SIZE, -1, 10000, 1);
	private static SpinnerNumberModel _rewriterCacheSizeModel       = new SpinnerNumberModel(PRAiSEDemoApp.REWRITER_CACHE_SIZE, -1, 10000, 1);

	{
		_precisionModel.addChangeListener(new ChangeListener() {
			
			@Override
			public void stateChanged(ChangeEvent e) {
				SyntaxTrees.setNumericDisplayPrecision((Integer)_precisionModel.getValue());
				Configuration.setProperty(ExpressoConfiguration.KEY_DISPLAY_NUMERIC_PRECISION_FOR_SYMBOLS, ""+_precisionModel.getValue());
			}

		});
		_scientificGreaterOutputModel.addChangeListener(new ChangeListener() {
			
			@Override
			public void stateChanged(ChangeEvent e) {
				SyntaxTrees.setDisplayScientificGreaterNIntegerPlaces((Integer)_scientificGreaterOutputModel.getValue());	
			}
		});
		_scientificAfterOutputModel.addChangeListener(new ChangeListener() {
			
			@Override
			public void stateChanged(ChangeEvent e) {
				SyntaxTrees.setDisplayScientificAfterNDecimalPlaces((Integer)_scientificAfterOutputModel.getValue());	
			}
		});	
		_deadEndsCacheSizeModel.addChangeListener(new ChangeListener() {
			
			@Override
			public void stateChanged(ChangeEvent e) {
				Configuration.setProperty(GrinderConfiguration.KEY_REWRITE_DEAD_ENDS_CACHE_MAXIMUM_SIZE, ""+_deadEndsCacheSizeModel.getValue());
				
			}
		});
		_rewriterCacheSizeModel.addChangeListener(new ChangeListener() {
			
			@Override
			public void stateChanged(ChangeEvent e) {		
				Configuration.setProperty(GrinderConfiguration.KEY_REWRITING_PROCESS_CACHE_MAXIMUM_SIZE, ""+_rewriterCacheSizeModel.getValue());
			}
		});
		
	}
	//
	JTextField typeSizeTextField = null;
	JComboBox<String> scheduleComboBox;
	JCheckBox chckbxJustificationToConsole;
	JCheckBox chckbxJustificationToJustTab;
	JCheckBox chckbxTraceToConsole;
	JCheckBox chckbxTraceToTrace;
	JCheckBox chckbxJustificationEnabled;
	JCheckBox chckbxTraceEnabled;
	JCheckBox chckbxOverrideModel;
	JCheckBox chckbxKnownTypeSize;
	JCheckBox chckbxAssumeDomainsAlwaysLarge;
	JCheckBox chckbxUseBeliefCache;
	JSpinner lbpPrecisionSpinner;
	JSpinner lbpMaxNumIterationsSpinner;
	private JSpinner displayPrecisionSpinner;
	private JSpinner scientificAfterSpinner;
	private JSpinner scientificGreaterSpinner;
	private JSpinner rewriterCacheSizeSpinner;
	private JSpinner deadEndCacheSizeSpinner;

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
		
		scheduleComboBox = new JComboBox<>();
		scheduleComboBox.setPreferredSize(new Dimension(100, 25));
		schedulePanel.add(scheduleComboBox);
		
		chckbxOverrideModel = new JCheckBox("Override Model's Domain Sizes (i.e. sort sizes)");
		chckbxOverrideModel.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (chckbxOverrideModel.isSelected()) {
					chckbxKnownTypeSize.setEnabled(true);
					typeSizeTextField.setEnabled(true);
					chckbxAssumeDomainsAlwaysLarge.setEnabled(true);
				}
				else {
					chckbxKnownTypeSize.setEnabled(false);
					typeSizeTextField.setEnabled(false);
					chckbxAssumeDomainsAlwaysLarge.setEnabled(false);
				}
			}
		});
		
		chckbxUseBeliefCache = new JCheckBox("Use Belief Cache");
		chckbxUseBeliefCache.setSelected(true);
		optionsPanel.add(chckbxUseBeliefCache);
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
		
		chckbxKnownTypeSize = new JCheckBox("Known Domain Size");
		chckbxKnownTypeSize.setSelected(true);
		chckbxKnownTypeSize.setEnabled(false);
		knownSizePanel.add(chckbxKnownTypeSize);
		
		typeSizeTextField = new JTextField();
		typeSizeTextField.setEnabled(false);
		typeSizeTextField.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				Integer size = new Integer(10);
				try {
					size = new Integer(typeSizeTextField.getText());
					if (size < 1) {
						size = 1;
						typeSizeTextField.setText(size.toString());
					}
				} catch (NumberFormatException nfe) {
					typeSizeTextField.setText(size.toString());
				}
			}
		});
		typeSizeTextField.setPreferredSize(new Dimension(80, 25));
		typeSizeTextField.setText(new Integer(10).toString());
		knownSizePanel.add(typeSizeTextField);
		
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
		
		JPanel lbpPrecisionPanel = new JPanel();
		lbpPrecisionPanel.setAlignmentX(0.0f);
		optionsPanel.add(lbpPrecisionPanel);
		lbpPrecisionPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));
		
		JLabel lblPrecision = new JLabel("Limit Precision To: ");
		lbpPrecisionPanel.add(lblPrecision);
		
		lbpPrecisionSpinner = new JSpinner();
		lbpPrecisionSpinner.setModel(new SpinnerNumberModel(20, 1, 80, 1));
		lbpPrecisionPanel.add(lbpPrecisionSpinner);
		
		JPanel lbpIterationsPanel = new JPanel();
		lbpIterationsPanel.setAlignmentX(0.0f);
		optionsPanel.add(lbpIterationsPanel);
		lbpIterationsPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));
		
		JLabel lblMaxIterations = new JLabel("Max # Iterations for Convergence: ");
		lbpIterationsPanel.add(lblMaxIterations);
		
		lbpMaxNumIterationsSpinner = new JSpinner();
		lbpMaxNumIterationsSpinner.setModel(new SpinnerNumberModel(10, 1, 10000, 1));
		lbpIterationsPanel.add(lbpMaxNumIterationsSpinner);
		
		JSeparator separator_1 = new JSeparator();
		optionsPanel.add(separator_1);
		
		chckbxJustificationEnabled = new JCheckBox("Justification Enabled");
		chckbxJustificationEnabled.setSelected(true);
		chckbxJustificationEnabled.addActionListener(new ActionListener() {
			@Override
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
			@Override
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
		precisionPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));
		
		JLabel precisionLabel = new JLabel("Display Numeric Precision:");
		precisionPanel.add(precisionLabel);
		
		displayPrecisionSpinner = new JSpinner();
		displayPrecisionSpinner.setModel(_precisionModel);
		precisionPanel.add(displayPrecisionSpinner);
		
		JPanel scientificOutputPanel = new JPanel();
		scientificOutputPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		optionsPanel.add(scientificOutputPanel);
		scientificOutputPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));
		
		JPanel scientificGreaterPanel = new JPanel();
		scientificOutputPanel.add(scientificGreaterPanel);
		scientificGreaterPanel.setLayout(new BoxLayout(scientificGreaterPanel, BoxLayout.X_AXIS));
		
		JLabel scientificLabel = new JLabel("Use Scientific When Outside Range: ");
		scientificGreaterPanel.add(scientificLabel);
		
		scientificGreaterSpinner = new JSpinner();
		scientificGreaterPanel.add(scientificGreaterSpinner);
		scientificGreaterSpinner.setModel(_scientificGreaterOutputModel);
		
		JLabel dotLabel = new JLabel(".");
		scientificGreaterPanel.add(dotLabel);
		
		scientificAfterSpinner = new JSpinner();
		scientificGreaterPanel.add(scientificAfterSpinner);
		scientificAfterSpinner.setModel(_scientificAfterOutputModel);
		
		JSeparator separator_3 = new JSeparator();
		optionsPanel.add(separator_3);
		
		JPanel deadEndsCacheSizePanel = new JPanel();
		deadEndsCacheSizePanel.setAlignmentX(0.0f);
		optionsPanel.add(deadEndsCacheSizePanel);
		deadEndsCacheSizePanel.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));
		
		JLabel lblDeadEndsCache = new JLabel("Cache Size ");
		deadEndsCacheSizePanel.add(lblDeadEndsCache);
		
		deadEndCacheSizeSpinner = new JSpinner();
		deadEndCacheSizeSpinner.setModel(_deadEndsCacheSizeModel);
		deadEndsCacheSizePanel.add(deadEndCacheSizeSpinner);
		
		JLabel lblNewLabel_1 = new JLabel(" for Dead Ends.");
		deadEndsCacheSizePanel.add(lblNewLabel_1);
		
		JPanel rewriterCacheSizePanel = new JPanel();
		rewriterCacheSizePanel.setAlignmentX(0.0f);
		optionsPanel.add(rewriterCacheSizePanel);
		rewriterCacheSizePanel.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));
		
		JLabel lblRewriterCacheSize = new JLabel("Cache Size ");
		rewriterCacheSizePanel.add(lblRewriterCacheSize);
		
		rewriterCacheSizeSpinner = new JSpinner();
		rewriterCacheSizeSpinner.setModel(_rewriterCacheSizeModel);
		rewriterCacheSizePanel.add(rewriterCacheSizeSpinner);
		
		JLabel lblNewLabel_2 = new JLabel(" for Rewriters.");
		rewriterCacheSizePanel.add(lblNewLabel_2);
	}

	private void postGUIInitialization() {
		scheduleComboBox.addItem(SYNCHRONOUS);
		scheduleComboBox.addItem(ASYNC_INDIVIDUAL);
		scheduleComboBox.addItem(ASYNC_GROUP);
		
		LBPConfiguration defaultConfig = LBPFactory.newLBPConfiguration();
		chckbxUseBeliefCache.setSelected(defaultConfig.isBeliefUseCache());
		lbpMaxNumIterationsSpinner.setValue(defaultConfig.getMaxNumberOfIterationsForConvergence());
		lbpPrecisionSpinner.setValue(defaultConfig.getLimitPrecisionToNumberOfSignificantDecimals());
		deadEndCacheSizeSpinner.setValue(PRAiSEDemoApp.DEAD_ENDS_CACHE_SIZE);
		rewriterCacheSizeSpinner.setValue(PRAiSEDemoApp.REWRITER_CACHE_SIZE);
		
		// Don't select by default.
		chckbxJustificationEnabled.doClick();
		chckbxTraceEnabled.doClick();
	}
}
