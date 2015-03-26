package com.sri.ai.praise.sgsolver.demo;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcons;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.TabPane;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.AnchorPane;

public class QueryController {
	//
	@FXML private ComboBox<String> queryComboBox;
	//
	@FXML private Button checkInputButton;
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
	
	@FXML
	private void initialize() {
		FXUtil.setDefaultButtonIcon(checkInputButton, FontAwesomeIcons.CHECK);
    	FXUtil.setDefaultButtonIcon(executeButton, FontAwesomeIcons.PLAY);
    	FXUtil.setDefaultButtonIcon(clearOutputButton, FontAwesomeIcons.ERASER);
	}
}
