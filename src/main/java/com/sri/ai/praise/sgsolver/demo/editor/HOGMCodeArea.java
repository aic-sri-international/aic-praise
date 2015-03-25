package com.sri.ai.praise.sgsolver.demo.editor;

import java.util.concurrent.ExecutorService;

import org.fxmisc.richtext.CodeArea;

import com.sri.ai.praise.sgsolver.demo.FXUtil;

import javafx.scene.layout.AnchorPane;

public class HOGMCodeArea extends AnchorPane {
	private CodeArea        codeArea;
	private ExecutorService executor;
	
	public HOGMCodeArea() {
		super();
		initialize();
	}
	
	//
	// PRIVATE
	//
	private void initialize() {
		codeArea = new CodeArea();
		
		FXUtil.anchor(codeArea);
		
		getChildren().add(codeArea);
	}
}
