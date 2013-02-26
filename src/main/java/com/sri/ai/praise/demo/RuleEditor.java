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
import java.awt.event.FocusListener;
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
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.Token;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderLexer;

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
		_terminals.add(AntlrGrinderLexer.COLON);
	    _terminals.add(AntlrGrinderLexer.DOUBLE_ARROW);
	    _terminals.add(AntlrGrinderLexer.ARROW);
	    _terminals.add(AntlrGrinderLexer.SINGLE_ARROW);
	    _terminals.add(AntlrGrinderLexer.NOT_EQUAL);
	    _terminals.add(AntlrGrinderLexer.GREATER_THAN_EQUAL);
	    _terminals.add(AntlrGrinderLexer.GREATER_THAN);
	    _terminals.add(AntlrGrinderLexer.LESS_THAN);
	    _terminals.add(AntlrGrinderLexer.LESS_THAN_EQUAL);
	    _terminals.add(AntlrGrinderLexer.EQUAL);
	    _terminals.add(AntlrGrinderLexer.PLUS);
	    _terminals.add(AntlrGrinderLexer.DASH);
	    _terminals.add(AntlrGrinderLexer.TIMES);
	    _terminals.add(AntlrGrinderLexer.DIVIDE);
	    _terminals.add(AntlrGrinderLexer.CARAT);
	    _terminals.add(AntlrGrinderLexer.CLOSE_SQUARE);
	    _terminals.add(AntlrGrinderLexer.OPEN_SQUARE);
	    _terminals.add(AntlrGrinderLexer.CLOSE_DOUBLE_CURLY);
	    _terminals.add(AntlrGrinderLexer.OPEN_DOUBLE_CURLY);
	    _terminals.add(AntlrGrinderLexer.UNDERSCORE_OPEN_CURLY);
	    _terminals.add(AntlrGrinderLexer.CLOSE_CURLY);
	    _terminals.add(AntlrGrinderLexer.OPEN_CURLY);
	    _terminals.add(AntlrGrinderLexer.VERT_BAR);
	    _terminals.add(AntlrGrinderLexer.OPEN_PAREN);
	    _terminals.add(AntlrGrinderLexer.CLOSE_PAREN);
	    _terminals.add(AntlrGrinderLexer.COMMA);
	    _terminals.add(AntlrGrinderLexer.PERIOD);
	}
	private static final Set<Integer> _keywords = new HashSet<Integer>();
	{
		_keywords.add(AntlrGrinderLexer.PREVIOUS);
		_keywords.add(AntlrGrinderLexer.MESSAGE);
		_keywords.add(AntlrGrinderLexer.LAMBDA);
		_keywords.add(AntlrGrinderLexer.IF);
		_keywords.add(AntlrGrinderLexer.THEN);
		_keywords.add(AntlrGrinderLexer.ELSE);
		_keywords.add(AntlrGrinderLexer.THERE);
		_keywords.add(AntlrGrinderLexer.FOR);
		_keywords.add(AntlrGrinderLexer.ALL);
		_keywords.add(AntlrGrinderLexer.EXISTS);
		_keywords.add(AntlrGrinderLexer.MINUS);
		_keywords.add(AntlrGrinderLexer.NOT);
		_keywords.add(AntlrGrinderLexer.IS);
		_keywords.add(AntlrGrinderLexer.CASE);
		_keywords.add(AntlrGrinderLexer.INDEX);
		_keywords.add(AntlrGrinderLexer.OCCURS);
		_keywords.add(AntlrGrinderLexer.FROM);
		_keywords.add(AntlrGrinderLexer.VARIABLE);
		_keywords.add(AntlrGrinderLexer.FACTOR);
		_keywords.add(AntlrGrinderLexer.NEIGHBORS);
		_keywords.add(AntlrGrinderLexer.VALUE);
		_keywords.add(AntlrGrinderLexer.INTERSECTION);
		_keywords.add(AntlrGrinderLexer.UNION);
		_keywords.add(AntlrGrinderLexer.OR);
		_keywords.add(AntlrGrinderLexer.AND);
		_keywords.add(AntlrGrinderLexer.IN);
		_keywords.add(AntlrGrinderLexer.OF);
		_keywords.add(AntlrGrinderLexer.ON);
		_keywords.add(AntlrGrinderLexer.TO);		
	}
	
	//
	private JTextPane textPane;
	
	
	public RuleEditor() {
		setLayout(new BorderLayout(0, 0));
		
		JScrollPane editorScrollPane = new JScrollPane();
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
	
	public void addFocusListener(FocusListener l) {
		textPane.addFocusListener(l);
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
			if (Character.isUpperCase(strValue.charAt(0)) || strValue.equals("_")) {
				result = true;
			}
		}
		return result;
	}	
	
	protected boolean isSymbol(Token t) {
		// Note: Underscore is a prolog variable.
		return t.getType() == AntlrGrinderLexer.ID || t.getType() == AntlrGrinderLexer.UNDERSCORE;
	}
	
	protected boolean isString(Token t) {
		return t.getType() == AntlrGrinderLexer.STRING;
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
		}
		
		@Override
		public void insertString(DocumentFilter.FilterBypass fb, int offset, String string, AttributeSet attr) throws BadLocationException {
			
			StyledDocument styledDocument = (StyledDocument)fb.getDocument();
			
			super.insertString(fb, offset, string, attr);
			format(styledDocument);
		}
		
		@Override
		public void replace(DocumentFilter.FilterBypass fb, int offset, int length, String text, AttributeSet attrs) throws BadLocationException {
			
			StyledDocument styledDocument = (StyledDocument)fb.getDocument();
			
			super.replace(fb, offset, length, text, attrs);
			format(styledDocument);
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
	    		AntlrGrinderLexer lexer = new AntlrGrinderLexer(cs);
	    		CommonTokenStream tokens = new CommonTokenStream(lexer);
	    		
	    		Token token = null;
	    		int lastMarkedUpPos = 0;
	    		boolean lexerFailed = false;
	    		try {
	    			token = tokens.LT(1);
	    		} catch (RuntimeException ex) {
	    			lexerFailed = true;
	    		}
	    		while (!lexerFailed && token.getType() != AntlrGrinderLexer.EOF) {   			
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
			}
		}
	}
}
