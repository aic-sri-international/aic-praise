/*
 * Copyright (c) 2015, SRI International
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
package com.sri.ai.praise.sgsolver.demo.editor;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Token;
import org.fxmisc.richtext.CodeArea;
import org.fxmisc.richtext.LineNumberFactory;
import org.fxmisc.richtext.StyleSpans;
import org.fxmisc.richtext.StyleSpansBuilder;
import org.fxmisc.undo.UndoManager;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.sgsolver.demo.FXUtil;
import com.sri.ai.praise.sgsolver.hogm.antlr.HOGMLexer;
import com.sri.ai.praise.sgsolver.hogm.antlr.HOGMTerminalSymbols;

import javafx.scene.layout.AnchorPane;

import java.util.Collection;
import java.util.Collections;

@Beta
public class HOGMCodeArea extends AnchorPane {
	private CodeArea codeArea;
	
	public HOGMCodeArea() {
		super();
		initialize();
	}
	
	public void setText(String text) {
		codeArea.replaceText(text);
		codeArea.getUndoManager().forgetHistory();
	}
	
	public String getText() {
		return codeArea.getText();
	}
	
	public UndoManager getUndoManager() {
		return codeArea.getUndoManager();
	}
	
	public void undo() {
		codeArea.undo();
	}
	
	public void redo() {
		codeArea.redo();
	}
	
	public void setEditable(boolean value) {
		codeArea.setEditable(value);
	}
	
	public void highlight(int startIdx, int endIdx) {
		int s = startIdx > 0 ? startIdx -1 : startIdx;
		int e = endIdx < (getText().length()-1) ? endIdx+1 : endIdx;
		codeArea.selectRange(s, e);
	}
	
	public void setFocus() {		
		codeArea.requestFocus();
	}
	
	//
	// PRIVATE
	//
	private void initialize() {		
		codeArea = new CodeArea();
		codeArea.setParagraphGraphicFactory(LineNumberFactory.get(codeArea, digits -> "%"+ (digits < 2 ? 2 : digits) + "d"));
		
		codeArea.textProperty().addListener((obs, oldText, newText) -> {
			codeArea.setStyleSpans(0, computeHighlighting(newText));
		});	
		
		FXUtil.anchor(codeArea);		
		getChildren().add(codeArea);
	}
		 
	private static StyleSpans<Collection<String>> computeHighlighting(String text) {
		StyleSpansBuilder<Collection<String>> spansBuilder = new StyleSpansBuilder<>();
		int lastTokenEnd = 0;
		ANTLRInputStream input = new ANTLRInputStream(text);
		HOGMLexer lexer = new HOGMLexer(input);
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		tokens.fill();	
		for (int i = 0; i < tokens.size(); i++) {
			Token t = tokens.get(i);
			if (t.getType() == Token.EOF) {
				break;
			}			
			String styleClass;
			if (t.getType() == HOGMLexer.COMMENT || t.getType() == HOGMLexer.LINE_COMMENT) {
				styleClass = "codeComment";
			}
			else if (HOGMTerminalSymbols.isTerminalSymbol(t.getText())) {
				styleClass = "codeKeyword";
			}
			else {
				styleClass = "codeOther";
			}
			int spacing = t.getStartIndex() - lastTokenEnd;
			if (spacing > 0) {			
				spansBuilder.add(Collections.emptyList(), spacing);
			}
			int stylesize = (t.getStopIndex() - t.getStartIndex()) +1;			
			spansBuilder.add(Collections.singleton(styleClass), stylesize);
			lastTokenEnd = t.getStopIndex()+1;
		}
		
		return spansBuilder.create();
	}
}
