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
import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.AbstractAction;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;
import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CompoundEdit;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.Token;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.praise.rules.antlr.RuleLexer;

@Beta
public class RuleEditor extends JPanel {
	private static final long serialVersionUID = 1L;
	
	private static final String STYLE_REGULAR  = "regular";
	private static final String STYLE_COMMENT  = "comment";
	private static final String STYLE_TERMINAL = "terminal";
	private static final String STYLE_KEYWORD  = "keyword";
	private static final String STYLE_NUMBER   = "number";
	private static final String STYLE_VARIABLE = "variable";
	private static final String STYLE_SYMBOL   = "symbol";
	private static final String STYLE_STRING   = "string";
	
	private static final Color COLOR_REGULAR  = Color.BLACK;
	private static final Color COLOR_COMMENT  = new Color(63,  127,  95);
	private static final Color COLOR_TERMINAL = COLOR_REGULAR;
	private static final Color COLOR_KEYWORD  = new Color(127,   0,  85);
	private static final Color COLOR_NUMBER   = new Color(125, 125, 125);
	private static final Color COLOR_VARIABLE = COLOR_REGULAR;
	private static final Color COLOR_SYMBOL   = COLOR_REGULAR;
	private static final Color COLOR_STRING   = new Color(42,    0, 255);
	//
	private static final Set<Integer> _terminals = new HashSet<Integer>();
	{
		_terminals.add(RuleLexer.ARROW);
		_terminals.add(RuleLexer.DOUBLE_ARROW);
		_terminals.add(RuleLexer.SINGLE_ARROW);
		_terminals.add(RuleLexer.EQUAL);
		_terminals.add(RuleLexer.NOT_EQUAL);
		_terminals.add(RuleLexer.PLUS);
		_terminals.add(RuleLexer.DASH);
		_terminals.add(RuleLexer.TIMES);
		_terminals.add(RuleLexer.DIVIDE);
		_terminals.add(RuleLexer.CARAT);
		_terminals.add(RuleLexer.OPEN_PAREN);
		_terminals.add(RuleLexer.CLOSE_PAREN);
		_terminals.add(RuleLexer.COLON_DASH);
		_terminals.add(RuleLexer.COLON);
		_terminals.add(RuleLexer.SEMICOLON);
		_terminals.add(RuleLexer.PERIOD);
		_terminals.add(RuleLexer.COMMA);
	}
	private static final Set<Integer> _keywords = new HashSet<Integer>();
	{
		_keywords.add(RuleLexer.IF);
		_keywords.add(RuleLexer.THEN);
		_keywords.add(RuleLexer.ELSE);
		_keywords.add(RuleLexer.SORT);
		_keywords.add(RuleLexer.RANDOM);
		_keywords.add(RuleLexer.X);
		_keywords.add(RuleLexer.THERE);
		_keywords.add(RuleLexer.EXISTS);
		_keywords.add(RuleLexer.FOR);
		_keywords.add(RuleLexer.ALL);
		_keywords.add(RuleLexer.AND);
		_keywords.add(RuleLexer.OR);
		_keywords.add(RuleLexer.NOT);
		_keywords.add(RuleLexer.MAY);
		_keywords.add(RuleLexer.BE);
		_keywords.add(RuleLexer.SAME);
		_keywords.add(RuleLexer.AS);
		_keywords.add(RuleLexer.MINUS);
	}
	
	//
	private CompoundUndoableEditListener compoundListener = new CompoundUndoableEditListener();
	//
	private JScrollPane editorScrollPane;
	private JTextPane textPane;
	
	
	public RuleEditor() {
		setLayout(new BorderLayout(0, 0));
		
		editorScrollPane = new JScrollPane();
		add(editorScrollPane, BorderLayout.CENTER);
		
		textPane = new JTextPane();
		editorScrollPane.setViewportView(textPane);
		
		postGUISetup();
	}
	
	public String getText() {
		String result = "";
		StyledDocument styledDoc = textPane.getStyledDocument();
		
		try {
			result = styledDoc.getText(0, styledDoc.getLength());
		} catch (BadLocationException ble) {
			ble.printStackTrace();
		}
		
		return result;
	}
	
	public void setText(String text) {
		try {
			StyledDocument styledDoc = textPane.getStyledDocument();		
			
			if (styledDoc.getLength() > 0) {
				styledDoc.remove(0, styledDoc.getLength());
			}
			// Ensure carriage return linefeeds are replaced with just
			// a single linefeed. This will ensure that the logic in:
		    // ExpressionFormatFilter.format()
			// can always assume +1 for the line end, when trying
			// to map between token positions and the underlying text.
			text = text.replaceAll("\r\n", "\n");
			styledDoc.insertString(0, text, null);
			textPane.setCaretPosition(0);
		} catch (BadLocationException ble) {
				
			ble.printStackTrace();
		}
	}
	
	public boolean isEditable() {
		return textPane.isEditable();
	}
	
	public void setEditable(boolean editable) {
		textPane.setEditable(editable);
	}
	
	public void addUndoableEditListener(UndoableEditListener listener) {
		compoundListener.undoableListeners.add(listener);
	}
	
	//
	// PROTECTED
	//
	protected boolean isTerminal(Token t) {
		return _terminals.contains(t.getType());
	}
	
	
	protected boolean isKeyword(Token t) {
		return _keywords.contains(t.getType());
	}
	
	protected boolean isNumber(Token t) {
		boolean result = false;
		if (isSymbol(t)) {
			Symbol s = DefaultSymbol.createSymbol(t.getText());
			if (s.getValue() instanceof Number) {
				result = true;
			}
		}
		return result;
	}
	
	protected boolean isVariable(Token t) {
		boolean result = false;
		if (isSymbol(t)) {
			String strValue = t.getText();
			if (Character.isUpperCase(strValue.charAt(0))) {
				result = true;
			}
		}
		return result;
	}	
	
	protected boolean isSymbol(Token t) {
		return t.getType() == RuleLexer.ID;
	}
	
	protected boolean isString(Token t) {
		return t.getType() == RuleLexer.STRING;
	}
	
	//
	// PRIVATE
	//
	private void postGUISetup() {
		StyledDocument styledDoc = textPane.getStyledDocument();
		if (styledDoc instanceof AbstractDocument) {
			AbstractDocument doc = (AbstractDocument)styledDoc;
		    doc.setDocumentFilter(new ExpressionFormatFilter());
		} 
		addStylesToDocument(styledDoc);
		styledDoc.addUndoableEditListener(compoundListener);
		
		textPane.getInputMap().put(KeyStroke.getKeyStroke("TAB"),
                "doTab");
		
		textPane.getActionMap().put("doTab", new AbstractAction() {
			private static final long serialVersionUID = 1L;

			@Override
			public void actionPerformed(ActionEvent e) {
				Component comp = textPane.getFocusTraversalPolicy().getComponentAfter(textPane.getFocusCycleRootAncestor(), textPane);
				comp.requestFocus();			
			}
		});
		
		textPane.getInputMap().put(KeyStroke.getKeyStroke("shift TAB"),
                "doShiftTab");
		textPane.getActionMap().put("doShiftTab", new AbstractAction() {
			private static final long serialVersionUID = 1L;

			@Override
			public void actionPerformed(ActionEvent e) {
				Component comp = textPane.getFocusTraversalPolicy().getComponentBefore(textPane.getFocusCycleRootAncestor(), textPane);
				comp.requestFocus();			
			}
		});
	}
	
	private void addStylesToDocument(StyledDocument doc) {
        //Initialize some styles.
        Style def = StyleContext.getDefaultStyleContext().
                        getStyle(StyleContext.DEFAULT_STYLE);
 
        Style regular = doc.addStyle(STYLE_REGULAR, def);
        StyleConstants.setFontSize(regular, 12);
        StyleConstants.setFontFamily(regular, Font.MONOSPACED);
        StyleConstants.setBackground(regular, Color.WHITE);
        StyleConstants.setForeground(regular, COLOR_REGULAR);
 
        Style s = doc.addStyle(STYLE_COMMENT, regular);
        StyleConstants.setForeground(s, COLOR_COMMENT);
 
        s = doc.addStyle(STYLE_TERMINAL, regular);
        StyleConstants.setForeground(s, COLOR_TERMINAL);
        
        s = doc.addStyle(STYLE_KEYWORD, regular);
        StyleConstants.setForeground(s, COLOR_KEYWORD);
        
        s = doc.addStyle(STYLE_NUMBER, regular);
        StyleConstants.setForeground(s, COLOR_NUMBER);
        
        s = doc.addStyle(STYLE_VARIABLE, regular);
        StyleConstants.setBold(s, true);
        StyleConstants.setItalic(s, true);
        StyleConstants.setFontSize(s, 14);
        StyleConstants.setForeground(s, COLOR_VARIABLE);
        
        s = doc.addStyle(STYLE_SYMBOL, regular);
        StyleConstants.setForeground(s, COLOR_SYMBOL);
 
        s = doc.addStyle(STYLE_STRING, regular);
        StyleConstants.setForeground(s, COLOR_STRING); 
	}
	
	private class ExpressionFormatFilter extends DocumentFilter {
		public ExpressionFormatFilter() {
			
		}
		
		@Override
		public void remove(DocumentFilter.FilterBypass fb, int offset, int length) throws BadLocationException {
			
			StyledDocument styledDocument = (StyledDocument)fb.getDocument();
			
			super.remove(fb, offset, length);			
			format(styledDocument);
			
			compoundListener.triggerUndoableEditEvent();		
		}
		
		@Override
		public void insertString(DocumentFilter.FilterBypass fb, int offset, String string, AttributeSet attr) throws BadLocationException {
			
			StyledDocument styledDocument = (StyledDocument)fb.getDocument();
			
			super.insertString(fb, offset, string, attr);
			format(styledDocument);
			
			compoundListener.triggerUndoableEditEvent();
		}
		
		@Override
		public void replace(DocumentFilter.FilterBypass fb, int offset, int length, String text, AttributeSet attrs) throws BadLocationException {
			
			StyledDocument styledDocument = (StyledDocument)fb.getDocument();
			
			super.replace(fb, offset, length, text, attrs);
			format(styledDocument);
			
			compoundListener.triggerUndoableEditEvent();
		}
		
		private void format(StyledDocument styledDocument) throws BadLocationException {
			if (styledDocument.getLength() > 0) {
				String expressionText = styledDocument.getText(0, styledDocument.getLength());
				
				// Everything is considered a comment up front (i.e. these won't be tokenized)
				styledDocument.setCharacterAttributes(0, expressionText.length(), styledDocument.getStyle(STYLE_COMMENT), true);
				
				List<String> lines        = new ArrayList<String>();
				List<Integer> lineOffsets = new ArrayList<Integer>();
				BufferedReader reader = new BufferedReader(new StringReader(expressionText));
				String line;
				int offset = 0;
				try {				
					while ((line = reader.readLine()) != null) {
						lines.add(line);
						lineOffsets.add(offset);
						
						offset += line.length()+1; // i.e. include the newline.
					}
					reader.close();
				} catch (IOException ioe) {
					ioe.printStackTrace();
				}
	    		CharStream cs = new ANTLRStringStream(expressionText);
	    		RuleLexer lexer = new RuleLexer(cs);
	    		CommonTokenStream tokens = new CommonTokenStream(lexer);
	    		
	    		Token token = null;
	    		int lastMarkedUpPos = 0;
	    		boolean lexerFailed = false;
	    		try {
	    			token = tokens.LT(1);
	    		} catch (RuntimeException ex) {
	    			lexerFailed = true;
	    		}
	    		while (!lexerFailed && token.getType() != RuleLexer.EOF) {   			
	    			offset = lineOffsets.get(token.getLine()-1) + token.getCharPositionInLine();
	    			int length = token.getText().length();
	    			Style style  = styledDocument.getStyle(STYLE_REGULAR);
	    			if (isTerminal(token)) {     				
	    				style = styledDocument.getStyle(STYLE_TERMINAL);
	    			}
	    			else if (isKeyword(token)) {
	    				style = styledDocument.getStyle(STYLE_KEYWORD);
	    			}
	    			else if (isNumber(token)) {
	    				style = styledDocument.getStyle(STYLE_NUMBER);
	    			}
	    			else if (isVariable(token)) {
	    				style = styledDocument.getStyle(STYLE_VARIABLE);
	    			}
	    			// Note: Test isSymbol after isNumber and isVariable as both of them are 
	    			// types of symbols.
	    			else if (isSymbol(token)) {
	    				style = styledDocument.getStyle(STYLE_SYMBOL);
	    			}
	    			else if (isString(token)) {
	    				style = styledDocument.getStyle(STYLE_STRING);
	    				length += 2;
	    			}
	    			    			
	    			styledDocument.setCharacterAttributes(offset, length, style, true);
	    			lastMarkedUpPos = offset + length;
	    			try {
	        			tokens.consume();
	    				token = tokens.LT(1);
	    			} catch (RuntimeException ex) {
	    				// ignore and exit.
	    				lexerFailed = true;
	    			}
	    		}
	    		
	    		if (lexerFailed) {
	    			styledDocument.setCharacterAttributes(lastMarkedUpPos, expressionText.length(), styledDocument.getStyle(STYLE_REGULAR), true);
	    		}
	    		
	    		// Note: The following ensure the caret position is reset correctly after formatting occurs when a redo is applied 
	    		// (otherwise the caret ends up going to the end of the document).
	    		compoundListener.undoableEditHappened(new UndoableEditEvent(textPane, new AbstractUndoableEdit() {
					private static final long serialVersionUID = 1L;
					private int caretPosition = textPane.getCaretPosition();
	    			@Override
	    			public void redo() throws CannotRedoException {
	    				super.redo();
	    				textPane.setCaretPosition(caretPosition);
	    			}
	    		}));
			}
		}
	}
	
	private class CompoundUndoableEditListener implements UndoableEditListener {
		public List<UndoableEditListener> undoableListeners = new ArrayList<UndoableEditListener>();
		public Object source = null;
		public CompoundEdit compoundEdit = new CompoundEdit();
		
		public void triggerUndoableEditEvent() {
			if (compoundEdit != null) {
				compoundEdit.end();
				UndoableEditEvent event = new UndoableEditEvent(source, compoundEdit);
				
				compoundEdit = null;
				
				// Notify listeners of event
				for (UndoableEditListener l : undoableListeners) {
					l.undoableEditHappened(event);
				}
			}
		}
		
		@Override
		public void undoableEditHappened(UndoableEditEvent e) {
			this.source = e.getSource();
			if (compoundEdit == null) {
				compoundEdit = new CompoundEdit();
			}
			compoundEdit.addEdit(e.getEdit());
		}
	}
}
