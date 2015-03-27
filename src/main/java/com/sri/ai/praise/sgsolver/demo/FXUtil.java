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

import com.google.common.annotations.Beta;

import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ContentDisplay;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Region;
import de.jensd.fx.glyphs.GlyphIcons;
import de.jensd.fx.glyphs.GlyphsBuilder;
import de.jensd.fx.glyphs.GlyphsDude;
import de.jensd.fx.glyphs.GlyphsStack;
import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcons;

@Beta
public class FXUtil {
	//
	public static final String _iconSmallSize            = "17px";
	public static final String _iconMediumSize           = "18px";
	public static final String _buttonDefaultIconSize    = "24px";
	public static final String _buttonPaginationIconSize = "8px";
	
    public static void anchor(Node node) {
        AnchorPane.setTopAnchor(node, 0.0);
        AnchorPane.setLeftAnchor(node, 0.0);
        AnchorPane.setRightAnchor(node, 0.0);
        AnchorPane.setBottomAnchor(node, 0.0);
    }
    
    public static void setDefaultButtonIcon(Button button, GlyphIcons icon) {
    	GlyphsDude.setIcon(button, icon, _buttonDefaultIconSize, ContentDisplay.GRAPHIC_ONLY);
    }
    
    public static void setPaginationButtonIcon(Button button, GlyphIcons icon) {
    	GlyphsDude.setIcon(button, icon, _buttonPaginationIconSize, ContentDisplay.GRAPHIC_ONLY);
    }
    
    public static void setMediumButtonIcon(Button button, GlyphIcons icon) {
    	GlyphsDude.setIcon(button, icon, _buttonDefaultIconSize, ContentDisplay.GRAPHIC_ONLY);
    }
    
    public static void setButtonStackedIcons(Button button, GlyphIcons icon1, GlyphIcons icon2) {
    	Region saveAsImg = GlyphsStack.create()
    			.add(GlyphsBuilder.create(FontAwesomeIcon.class).glyph(icon1).size(_buttonDefaultIconSize).build())
    			.add(GlyphsBuilder.create(FontAwesomeIcon.class).glyph(icon2).size(_iconSmallSize).build());
    			
    	button.setGraphic(saveAsImg);
    	button.setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
    }
    
	public static Node configMenuIcon() {
		HBox node = new HBox();
		
		node.getChildren().add(GlyphsBuilder.create(FontAwesomeIcon.class).glyph(FontAwesomeIcons.COG).size("12px").build());
		node.getChildren().add(GlyphsBuilder.create(FontAwesomeIcon.class).glyph(FontAwesomeIcons.CARET_DOWN).size("10px").build());
		
		return node;
	}
}
