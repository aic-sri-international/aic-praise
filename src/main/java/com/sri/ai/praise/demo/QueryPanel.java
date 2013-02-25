package com.sri.ai.praise.demo;

import javax.swing.ImageIcon;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.Dimension;

import javax.swing.border.TitledBorder;
import javax.swing.JComboBox;
import javax.swing.JButton;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import java.awt.FlowLayout;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;

import javax.swing.JFormattedTextField;
import java.awt.Component;

import com.sri.ai.grinder.demo.ExpressionEditor;

public class QueryPanel extends JPanel {
	private static final long serialVersionUID = 1L;
	//
	private ImageIcon imageExecuteQuery = ToolBarPanel.getSmallExecuteQueryIcon(); 
	
	//
	private JFormattedTextField domainSizeTextField = null;
	private ExpressionEditor resultExpressionEditor = null;
	private JComboBox queryComboBox;

	/**
	 * Create the panel.
	 */
	public QueryPanel() {
		initialize();
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
		queryPanel.setLayout(new BorderLayout(0, 0));
		
		queryComboBox = new JComboBox();
		queryComboBox.setEditable(true);
		queryPanel.add(queryComboBox, BorderLayout.CENTER);
		
		JButton btnExecuteQuery = new JButton("");
		btnExecuteQuery.setIcon(imageExecuteQuery);
		btnExecuteQuery.setToolTipText("Execute Query");
		queryPanel.add(btnExecuteQuery, BorderLayout.EAST);
		
		JPanel optionsPanel = new JPanel();
		optionsPanel.setBorder(new TitledBorder(null, "Options", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		queryAndOptionsPanel.add(optionsPanel, BorderLayout.CENTER);
		optionsPanel.setLayout(new BoxLayout(optionsPanel, BoxLayout.Y_AXIS));
		
		JPanel knownSizePanel = new JPanel();
		knownSizePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		FlowLayout flowLayout = (FlowLayout) knownSizePanel.getLayout();
		flowLayout.setVgap(0);
		flowLayout.setHgap(0);
		flowLayout.setAlignment(FlowLayout.LEFT);
		optionsPanel.add(knownSizePanel);
		
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
		optionsPanel.add(chckbxAssumeDomainsAlwaysLarge);
		
		JCheckBox chckbxJustificationEnabled = new JCheckBox("Justification Output Enabled");
		optionsPanel.add(chckbxJustificationEnabled);
		
		JCheckBox chckbxTraceEnabled = new JCheckBox("Trace Output Enabled");
		optionsPanel.add(chckbxTraceEnabled);
		
		JPanel resultPanel = new JPanel();
		resultPanel.setBorder(new TitledBorder(null, "Result", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		add(resultPanel, BorderLayout.CENTER);
		resultPanel.setLayout(new BorderLayout(0, 0));
		
		resultExpressionEditor = new ExpressionEditor();
		resultExpressionEditor.setEnabled(false);
		resultPanel.add(resultExpressionEditor, BorderLayout.CENTER);
	}
}
