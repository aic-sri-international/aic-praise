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

import java.util.List;

import com.google.common.annotations.Beta;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcons;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.TabPane;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.AnchorPane;

@Beta
public class QueryController {
	//
	@FXML private ComboBox<String> queryComboBox;
	//
	@FXML private Button executeButton;
	@FXML private Button clearOutputButton;
	//
	@FXML private ProgressBar queryProgressBar;
	//
	//
	@FXML private TabPane outputTabPane;
	@FXML private AnchorPane resultPane;
	@FXML private AnchorPane problemPane;
	@FXML private AnchorPane consolePane;
	//
	@FXML private Tooltip executeTooltip;
	
	public void addDefaultQueries(List<String> queries) {
		if (queries.size() > 0) {
			queries.forEach(query -> {
				if (!queryComboBox.getItems().contains(query)) {
					queryComboBox.getItems().add(query);
				}
			});
			queryComboBox.getSelectionModel().select(queryComboBox.getItems().indexOf(queries.get(0)));
		}
	}
	
	@FXML
	private void initialize() {
		//
    	FXUtil.setDefaultButtonIcon(executeButton, FontAwesomeIcons.PLAY);
    	FXUtil.setDefaultButtonIcon(clearOutputButton, FontAwesomeIcons.ERASER);
	}
}
