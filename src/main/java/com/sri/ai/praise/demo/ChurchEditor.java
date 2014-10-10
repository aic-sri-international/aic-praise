package com.sri.ai.praise.demo;

import java.awt.BorderLayout;

import javax.swing.JPanel;
import javax.swing.ScrollPaneConstants;

import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;
import org.fife.ui.rtextarea.RTextScrollPane;

public class ChurchEditor extends JPanel {
	private static final long serialVersionUID = 1L;
	//
	private RSyntaxTextArea textArea;
	private RTextScrollPane editorScrollPane;
	
	public ChurchEditor() {
		setLayout(new BorderLayout(0, 0));

		textArea = new RSyntaxTextArea(1, 1);
		textArea.setTabsEmulated(false);
		textArea.setTabSize(4);
		textArea.setCodeFoldingEnabled(false);
		textArea.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_LISP);
		
		editorScrollPane = new RTextScrollPane(textArea);
		editorScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
		editorScrollPane.setFoldIndicatorEnabled(false);
		add(editorScrollPane, BorderLayout.CENTER);
	}
	
	public String getText() {
		String result = textArea.getText();
		
		return result;
	}
	
	public void setText(String text) {
		textArea.setText(text);
		textArea.setCaretPosition(0);
	}
	
	public boolean canUndo() {
		return textArea.canUndo();
	}
	
	public void undo() {
		textArea.undoLastAction();
	}
	
	public boolean canRedo() {
		return textArea.canRedo();
	}
	
	public void redo() {
		textArea.redoLastAction();
	}
	
	public void discardAllEdits() {
		textArea.discardAllEdits();
	}
}
