package com.sri.ai.praise.sgsolver.demo;

import java.util.List;

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
	@FXML private Button undoButton;
	@FXML private Button redoButton;
	//
	@FXML private Button checkInputButton;
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
			queryComboBox.setValue(queries.get(0));
		}
	}
	
	@FXML
	private void initialize() {
		//
    	FXUtil.setDefaultButtonIcon(undoButton, FontAwesomeIcons.ROTATE_LEFT);
    	FXUtil.setDefaultButtonIcon(redoButton, FontAwesomeIcons.ROTATE_RIGHT);
    	//
		FXUtil.setDefaultButtonIcon(checkInputButton, FontAwesomeIcons.CHECK);
		//
    	FXUtil.setDefaultButtonIcon(executeButton, FontAwesomeIcons.PLAY);
    	FXUtil.setDefaultButtonIcon(clearOutputButton, FontAwesomeIcons.ERASER);
	}
}
