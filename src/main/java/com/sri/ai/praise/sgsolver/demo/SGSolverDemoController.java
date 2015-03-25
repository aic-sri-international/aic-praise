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

import java.io.IOException;

import com.google.common.annotations.Beta;

import de.jensd.fx.glyphs.GlyphsBuilder;
import de.jensd.fx.glyphs.GlyphsDude;
import de.jensd.fx.glyphs.GlyphsStack;
import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcons;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.TabPane;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Region;

@Beta
public class SGSolverDemoController {
	//
	private static final String _iconSmallSize  = "17px";
	private static final String _iconMediumSize = "18px";
	private static final String _iconSize       = "24px";
	//
	@FXML private Button newButton;
	@FXML private Button openFileButton;
	@FXML private Button saveButton;
	@FXML private Button saveAsButton;
	//
	@FXML private ComboBox<String> examplesComboBox;
	//
	@FXML private Button undoButton;
	@FXML private Button redoButton;
	//
	@FXML private Button checkInputButton;
	@FXML private Button topExecuteButton;
	@FXML private Button clearOutputButton;
	//
	@FXML private Button newWindowButton;
	@FXML private Button openMenuButton;;
	//
	//
	@FXML private AnchorPane editorPane;
	//
	//
	@FXML private ComboBox queryComboBox;
	@FXML private Button queryExecuteButton;
	@FXML private ProgressBar queryProgressBar;
	//
	//
	@FXML private TabPane outputTabPane;
	@FXML private AnchorPane resultPane;
	@FXML private AnchorPane problemPane;
	@FXML private AnchorPane consolePane;
	//
	//
	@FXML private Tooltip undoTooltip;
	@FXML private Tooltip redoTooltip;
	@FXML private Tooltip topExecuteTooltip;
	@FXML private Tooltip queryExecuteTooltip;
	

    @FXML
    private void initialize() throws IOException {
    	GlyphsDude.setIcon(newButton, FontAwesomeIcons.FILE_ALT, _iconSize, ContentDisplay.GRAPHIC_ONLY);
    	GlyphsDude.setIcon(openFileButton, FontAwesomeIcons.FOLDER_OPEN, _iconSize, ContentDisplay.GRAPHIC_ONLY);
    	GlyphsDude.setIcon(saveButton, FontAwesomeIcons.SAVE, _iconSize, ContentDisplay.GRAPHIC_ONLY);
    	Region saveAsImg = GlyphsStack.create()
    			.add(GlyphsBuilder.create(FontAwesomeIcon.class).glyph(FontAwesomeIcons.SAVE).size(_iconSize).build())
    			.add(GlyphsBuilder.create(FontAwesomeIcon.class).glyph(FontAwesomeIcons.PENCIL).size(_iconSmallSize).build());
    			
    	saveAsButton.setGraphic(saveAsImg);
    	saveAsButton.setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
    	//
    	GlyphsDude.setIcon(undoButton, FontAwesomeIcons.ROTATE_LEFT, _iconSize, ContentDisplay.GRAPHIC_ONLY);
    	GlyphsDude.setIcon(redoButton, FontAwesomeIcons.ROTATE_RIGHT, _iconSize, ContentDisplay.GRAPHIC_ONLY);
    	//
    	GlyphsDude.setIcon(checkInputButton, FontAwesomeIcons.CHECK, _iconSize, ContentDisplay.GRAPHIC_ONLY);
    	GlyphsDude.setIcon(topExecuteButton, FontAwesomeIcons.PLAY, _iconSize, ContentDisplay.GRAPHIC_ONLY);
    	GlyphsDude.setIcon(clearOutputButton, FontAwesomeIcons.ERASER, _iconSize, ContentDisplay.GRAPHIC_ONLY);
    	//
    	GlyphsDude.setIcon(newWindowButton, FontAwesomeIcons.EXTERNAL_LINK, _iconSize, ContentDisplay.GRAPHIC_ONLY);
    	GlyphsDude.setIcon(openMenuButton, FontAwesomeIcons.BARS, _iconSize, ContentDisplay.GRAPHIC_ONLY);
    	
    	GlyphsDude.setIcon(queryExecuteButton, FontAwesomeIcons.PLAY, _iconMediumSize, ContentDisplay.GRAPHIC_ONLY);
    	
    	FXMLLoader editorLoader = new FXMLLoader(HOGMEditorController.class.getResource("hogmeditor.fxml"));
    	Pane editorPane = editorLoader.load();
    	FXUtil.anchor(editorPane);
    	this.editorPane.getChildren().add(editorPane);
    }
}
