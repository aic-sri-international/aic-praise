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
package com.sri.ai.praise.sgsolver.demo;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

import org.fxmisc.undo.UndoManager;
import org.fxmisc.undo.UndoManagerFactory;
import org.reactfx.EventSource;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.sgsolver.demo.model.ExamplePage;
import com.sri.ai.praise.sgsolver.demo.model.ExamplePages;

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.MapProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.ReadOnlyBooleanProperty;
import javafx.beans.property.ReadOnlyMapProperty;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleMapProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableMap;

@Beta
public abstract class AbstractPerspective implements Perspective {
	
	private IntegerProperty currentModelPageIndexProperty;
	//
	private BooleanProperty canUndoModelPageEdit = new SimpleBooleanProperty(false);
	private BooleanProperty canRedoModelPageEdit = new SimpleBooleanProperty(false);
	private MapProperty<Integer, Supplier<ModelPageEditor>> modelEditorPages = new SimpleMapProperty<>(FXCollections.observableHashMap());
	private ObjectProperty<File> modelFile = new SimpleObjectProperty<>();
	//
	private EventSource<PageChange> pageChanges = new EventSource<>();
	private UndoManager pageChangeUndoManager   = UndoManagerFactory.unlimitedHistoryUndoManager(
														pageChanges, // stream of changes to observe
														c -> c.redo(), // function to redo a change
														c -> c.undo(), // function to undo a change
														(c1, c2) -> c1.mergeWith(c2));
	
	//
	// START-Perspective default implementations
	@Override
	public void setCurrentModelPageIndexProperty(IntegerProperty currentModelPageIndexProperty) {
		this.currentModelPageIndexProperty = currentModelPageIndexProperty;
		this.currentModelPageIndexProperty.addListener((observer, oldValue, newValue) -> {
			Integer currentPageIdx = newValue.intValue();
			if (modelEditorPages.containsKey(currentPageIdx)) {
				ModelPageEditor mpe = modelEditorPages.get().get(currentPageIdx).get();
				canUndoModelPageEdit.unbind();
				canRedoModelPageEdit.unbind();
				canUndoModelPageEdit.bind(mpe.getUndoManager().undoAvailableProperty());
				canRedoModelPageEdit.bind(mpe.getUndoManager().redoAvailableProperty());
			}
		});
	}
	
	@Override
	public boolean isCanUndoModelPageEdit() {
		return canUndoModelPageEdit.get();
	}
	
	@Override
	public ReadOnlyBooleanProperty canUndoModelPageEditProperty() {
		return canUndoModelPageEdit;
	}
	
	@Override
	public boolean isCanRedoModelPageEdit() {
		return canRedoModelPageEdit.get();
	}
	
	@Override
	public ReadOnlyBooleanProperty canRedoModelPageEditProperty() {
		return canRedoModelPageEdit;
	}
	
	@Override
	public UndoManager getPageChangeUndoManager() {
		return pageChangeUndoManager;
	}
	
	@Override
	public ObservableMap<Integer, Supplier<ModelPageEditor>> getModelEditorPages() {
		return modelEditorPagesProperty().get();
	}
	
	@Override
	public ReadOnlyMapProperty<Integer, Supplier<ModelPageEditor>> modelEditorPagesProperty() {
		return modelEditorPages;
	}
	
	@Override
	public void newModel() {
 		modelEditorPages.set(FXCollections.observableMap(Collections.singletonMap(0, new ModelPageEditorSupplier("", Collections.emptyList()))));
	}
	
	@Override
	public void newModel(File modelFile) {
// TODO	
	}
	
	@Override
	public void newModel(ExamplePages examples) {
		List<ExamplePage> pages = examples.getPages();
		Map<Integer, Supplier<ModelPageEditor>> newModelPageIdxs = new HashMap<>();
		for (int i = 0; i < pages.size(); i++) {
			ExamplePage page = pages.get(i);
			newModelPageIdxs.put(i, new ModelPageEditorSupplier(page.getModel(), page.getDefaultQueriesToRun()));
		}
		modelEditorPages.set(FXCollections.observableMap(newModelPageIdxs));
	}
	
	@Override
	public void addPage(Integer atPageIndex) {
		Supplier<ModelPageEditor> addSupplier = new ModelPageEditorSupplier("// MODEL PAGE "+(atPageIndex+2), Collections.emptyList());
		addPage(atPageIndex, addSupplier);
		pageChanges.push(new AddPageChange(atPageIndex, addSupplier));
	}
	
	@Override 
	public void removePage(Integer pageIndex) {
 		Supplier<ModelPageEditor> removeSupplier = removePageAt(pageIndex);	
 		pageChanges.push(new RemovePageChange(pageIndex, removeSupplier));
	}
	
	@Override
	public File getModelFile() {
		return modelFile.get();
	}
	
	@Override
	public ReadOnlyObjectProperty<File> modelFileProperty() {
		return modelFile;
	}
	
	@Override 
	public boolean isSaveRequired() {
		return false; // TODO
	}
	
	@Override
	public void save() {
// TODO		
	}
	
	@Override
	public void saveAs(File file) {
// TODO		
	}
	// END-Perspective default implementations
	//
	
	protected abstract ModelPageEditor create(String modelPage, List<String> defaultQueries);
	
	protected void addPage(Integer atPageIndex, Supplier<ModelPageEditor> modelPageEditorSupplier) {
		Map<Integer, Supplier<ModelPageEditor>> newModelPageIdxs = new HashMap<>();
 		modelEditorPages.get().entrySet().forEach(e -> {
 			if (e.getKey() > atPageIndex) {
 				newModelPageIdxs.put(e.getKey()+1, e.getValue());
 			}
 			else {
 				newModelPageIdxs.put(e.getKey(), e.getValue());
 			}
 		});
 		newModelPageIdxs.put(atPageIndex+1, modelPageEditorSupplier);  		
 		modelEditorPages.set(FXCollections.observableMap(newModelPageIdxs));
 		currentModelPageIndexProperty.set(atPageIndex+1);
	}
	
	protected Supplier<ModelPageEditor> removePageAt(Integer pageIndex) {
		Supplier<ModelPageEditor> result = modelEditorPages.get(pageIndex);
 		Map<Integer, Supplier<ModelPageEditor>> newModelPageIdxs = new HashMap<>();
 		modelEditorPages.get().entrySet().forEach(e -> {
 			// Skip the page to be removed
 			if (!e.getKey().equals(pageIndex)) {
	 			if (e.getKey() > pageIndex) {
	 				newModelPageIdxs.put(e.getKey()-1, e.getValue());
	 			}
	 			else {
	 				newModelPageIdxs.put(e.getKey(), e.getValue());
	 			}
 			}
 		});
 		modelEditorPages.set(FXCollections.observableMap(newModelPageIdxs));
 		
 		if (pageIndex >= modelEditorPages.size()) {
 			currentModelPageIndexProperty.set(modelEditorPages.size()-1);
 		}
 		else {
 			currentModelPageIndexProperty.set(pageIndex);
 		}
 		return result;
	}
	
	protected class ModelPageEditorSupplier implements Supplier<ModelPageEditor> {
		private ModelPageEditor modelPageEditor = null;
		private String          modelPage;
		private List<String>    defaultQueries  = new ArrayList<>();
		
		public ModelPageEditorSupplier(String modelPage, List<String> defaultQueries) {
			this.modelPage = modelPage;
			this.defaultQueries.addAll(defaultQueries);
		}
		
		@Override
		public ModelPageEditor get() {
			if (modelPageEditor == null) {
				modelPageEditor = create(modelPage, defaultQueries);
			}
			return modelPageEditor;
		}
	}
	
	protected abstract class PageChange {
		protected int pageIdx;
		protected Supplier<ModelPageEditor> pageEditorSupplier;
		
		protected PageChange(int  pageIdx, Supplier<ModelPageEditor> pageEditorSupplier) {
			this.pageIdx            = pageIdx;
			this.pageEditorSupplier = pageEditorSupplier;
		}
		
		abstract void redo();

		abstract void undo();

		Optional<PageChange> mergeWith(PageChange other) {
			// don't merge changes by default
			return Optional.empty();
		}
	}
	
	protected class AddPageChange extends PageChange {
		public AddPageChange(int pageIdx, Supplier<ModelPageEditor> pageEditorSupplier) {
			super(pageIdx, pageEditorSupplier);
		}
		
		void redo() {
			addPage(pageIdx, pageEditorSupplier);
		}

		void undo() {
			if (pageEditorSupplier != removePageAt(pageIdx+1)) {
				throw new IllegalStateException("Page change add undo history appears corrupted.");
			}
		}
	}
	
	protected class RemovePageChange extends PageChange {
		public RemovePageChange(int pageIdx, Supplier<ModelPageEditor> pageEditorSupplier) {
			super(pageIdx, pageEditorSupplier);
		}
		
		void redo() {
			if (pageEditorSupplier != removePageAt(pageIdx)) {
				throw new IllegalStateException("Page change remove redo history appears corrupted.");
			}
		}

		void undo() {
			addPage(pageIdx-1, pageEditorSupplier);
		}
	}
}
