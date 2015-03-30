package com.sri.ai.praise.sgsolver.demo;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

import com.sri.ai.praise.sgsolver.demo.model.ExamplePage;
import com.sri.ai.praise.sgsolver.demo.model.ExamplePages;

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.MapProperty;
import javafx.beans.property.ReadOnlyBooleanProperty;
import javafx.beans.property.ReadOnlyMapProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleMapProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableMap;

public abstract class AbstractPerspective implements Perspective {
	
	private BooleanProperty canUndo = new SimpleBooleanProperty(false);
	private BooleanProperty canRedo = new SimpleBooleanProperty(false);
	private MapProperty<Integer, Supplier<ModelPageEditor>> modelEditorPages = new SimpleMapProperty<>(FXCollections.observableHashMap());
	//
	// START-Perspective default implementations
	@Override
	public boolean isCanUndo() {
		return canUndo.get();
	}
	
	@Override
	public ReadOnlyBooleanProperty canUndoProperty() {
		return canUndo;
	}
	
	@Override
	public boolean isCanRedo() {
		return canRedo.get();
	}
	
	@Override
	public ReadOnlyBooleanProperty canRedoProperty() {
		return canRedo;
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
		Map<Integer, Supplier<ModelPageEditor>> newModelPageIdxs = new HashMap<>();
 		modelEditorPages.get().entrySet().forEach(e -> {
 			if (e.getKey() > atPageIndex) {
 				newModelPageIdxs.put(e.getKey()+1, e.getValue());
 			}
 			else {
 				newModelPageIdxs.put(e.getKey(), e.getValue());
 			}
 		});
 		newModelPageIdxs.put(atPageIndex+1, new ModelPageEditorSupplier("// MODEL PAGE "+(atPageIndex+2), Collections.emptyList()));  		
 		modelEditorPages.set(FXCollections.observableMap(newModelPageIdxs));	
	}
	
	@Override 
	public void removePage(Integer pageIndex) {
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
	}
	// END-Perspective default implementations
	//
	
	protected abstract ModelPageEditor create(String modelPage, List<String> defaultQueries);
	
	class ModelPageEditorSupplier implements Supplier<ModelPageEditor> {
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
}
