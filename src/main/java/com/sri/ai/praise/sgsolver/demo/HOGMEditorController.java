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

import java.util.HashMap;
import java.util.Map;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.Pagination;
import javafx.scene.layout.AnchorPane;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.sgsolver.demo.editor.HOGMCodeArea;

@Beta
public class HOGMEditorController {
	
	@FXML private AnchorPane modelEditorPane;
	@FXML private Pagination evidencePagination;
	@FXML private Menu evidenceMenu;
	@FXML private MenuItem addEvidencePageMenuItem;
	@FXML private MenuItem removeEvidencePageMenuItem;
	//
	private HOGMCodeArea modelCodeArea = new HOGMCodeArea();
	private Map<Integer, HOGMCodeArea> evidenceCodeAreas = new HashMap<>();
	
	@FXML
	private void initialize() {
		evidenceMenu.setText("");
		evidenceMenu.setGraphic(FXUtil.configMenuIcon());
		
		FXUtil.anchor(modelCodeArea);
		modelEditorPane.getChildren().add(modelCodeArea);	
		evidencePagination.pageCountProperty().addListener(new ChangeListener<Number>() {
			public void changed(ObservableValue<? extends Number> observable, Number oldValue, Number newValue) {				
				if (newValue.intValue() <= 1) {
					removeEvidencePageMenuItem.setDisable(true);
				}
				else {
					removeEvidencePageMenuItem.setDisable(false);
				}
			}
		});
		
		evidencePagination.setPageCount(1);
		evidencePagination.setPageFactory(this::createEvidencePage);
	}
	
	@FXML
	private void addEvidencePage(ActionEvent ae) {
		evidencePagination.setPageCount(evidencePagination.getPageCount()+1);
	
		evidencePagination.setCurrentPageIndex(evidencePagination.getPageCount()-1);
	}
	
	@FXML
	private void removeEvidencePage(ActionEvent ae) {
		Integer currentPageIdx = evidencePagination.getCurrentPageIndex();
		evidenceCodeAreas.remove(currentPageIdx);
		Map<Integer, HOGMCodeArea> newEvidenceCodeAreaPageIdxs = new HashMap<>();
		evidenceCodeAreas.entrySet().forEach(e -> {
			if (e.getKey() > currentPageIdx) {
				newEvidenceCodeAreaPageIdxs.put(e.getKey()-1, e.getValue());
			}
			else {
				newEvidenceCodeAreaPageIdxs.put(e.getKey(), e.getValue());
			}
		});
		evidenceCodeAreas.clear();
		evidenceCodeAreas.putAll(newEvidenceCodeAreaPageIdxs);	
		// Reduce the # of pages
		evidencePagination.setPageCount(evidencePagination.getPageCount()-1);
		
		if (currentPageIdx < evidencePagination.getPageCount()) {
			evidencePagination.setCurrentPageIndex(currentPageIdx);
		}
		else {
			evidencePagination.setCurrentPageIndex(evidencePagination.getPageCount()-1);
		}
	}
	
	private Node createEvidencePage(Integer pgIndex) {	
		HOGMCodeArea evidencePage = evidenceCodeAreas.get(pgIndex);
		if (evidencePage == null) {
			evidencePage = new HOGMCodeArea();
			evidenceCodeAreas.put(pgIndex, evidencePage);
		}
		
		return evidencePage;
	}
}
