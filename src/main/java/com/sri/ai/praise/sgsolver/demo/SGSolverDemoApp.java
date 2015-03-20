package com.sri.ai.praise.sgsolver.demo;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

public class SGSolverDemoApp extends Application {

    @Override
    public void start(Stage primaryStage) throws Exception{
        Parent root = FXMLLoader.load(getClass().getResource("sgsolverdemo.fxml"));
        primaryStage.setTitle("SG Solver");
        Scene scene = new Scene(root, 1024, 768);
        primaryStage.setScene(scene);
        scene.getStylesheets().add("com/sri/ai/praise/sgsolver/demo/sgsolverdemo.css");
        primaryStage.show();
    }


    public static void main(String[] args) {
        launch(args);
    }
}
