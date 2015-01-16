/*
 * Copyright (c) 2014, SRI International
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
package com.sri.ai.praise.demo;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Action;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JPanel;

import org.fife.ui.rtextarea.RTextArea;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.demo.model.Example;

@Beta
public abstract class AbstractEditorPanel extends JPanel {
	
	public interface ActiveEditorListener {
		void activated();
	}
	
	private static final long serialVersionUID = 1L;
	//
	protected List<ActiveEditorListener> activeEditorListeners = new ArrayList<>();
	//
	protected JFileChooser fileChooser       = null;
	protected JComponent   fileChooserParent = null;
	protected QueryPanel   queryPanel        = null;
	
	public void addActiveEditorListener(ActiveEditorListener l) {
		if (!activeEditorListeners.contains(l)) {
			activeEditorListeners.add(l);
		}
	}
	
	public void setFileChooser(JFileChooser fileChooser, JComponent fileChooserParent) {
		this.fileChooser       = fileChooser;
		this.fileChooserParent = fileChooserParent;
	}
	
	public QueryPanel getQueryPanel() {
		return queryPanel;
	}
	
	public void setQueryPanel(QueryPanel queryPanel) {
		this.queryPanel = queryPanel;
	}
		
	public abstract List<Example> getExamples();
	public abstract void setExample(Example example);
	public abstract String getContextTitle();
	public abstract String getModel();
	public abstract void setContents(String contents, File fromFile) throws IOException;
	public abstract boolean isASaveRequired();
	public abstract void saveIfRequired() throws IOException;
	public abstract void saveAll() throws IOException;
	public abstract void saveAs() throws IOException;
	public abstract boolean canUndo();
	public abstract void undo();
	public abstract void redo();
	public abstract void discardAllEdits();
	public abstract void copyState(AbstractEditorPanel otherEditorPanel);
	public abstract List<String> validateContents();
	
	public Action getCutAction() {
		return RTextArea.getAction(RTextArea.CUT_ACTION);
	}
	
	public Action getCopyAction() {
		return RTextArea.getAction(RTextArea.COPY_ACTION);
	}
	
	public Action getPasteAction() {
		return RTextArea.getAction(RTextArea.PASTE_ACTION);
	}
	
	public Action getDeleteAction() {
		return RTextArea.getAction(RTextArea.DELETE_ACTION);
	}
	
	public Action getSelectAllAction() {
		return RTextArea.getAction(RTextArea.SELECT_ALL_ACTION);
	}
	
	public Action getUndoAction() {
		return RTextArea.getAction(RTextArea.UNDO_ACTION);
	}
	
	public Action getRedoAction() {
		return RTextArea.getAction(RTextArea.REDO_ACTION);
	}
	
	//
	// PROTECTED
	//
	protected void notifyActiveEditorListeners() {
		activeEditorListeners.forEach(l -> l.activated());
	}
}