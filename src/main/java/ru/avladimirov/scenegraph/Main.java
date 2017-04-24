/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ru.avladimirov.scenegraph;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import org.w3c.dom.Document;

/**
 *
 * @author Vladimirov.A.A
 */
public class Main extends Application {

	public static void main (String[] args) {
		launch (args);
	}

	@Override
	public void start (Stage primaryStage) throws Exception {

		VBox root = new VBox (10);
		root.minWidthProperty ().set (1000);
		root.minHeightProperty ().set (1000);

		Canvas canvas = new Canvas ();
		GraphicsContext context = canvas.getGraphicsContext2D ();
		context.setLineWidth (2);
		context.setFill (Color.BLUEVIOLET);
		context.strokeRoundRect (200, 200, 160, 160, 30, 30);
		canvas.setWidth (500);
		canvas.setHeight (500);
		root.getChildren ().addAll (canvas);

		HBox hBox = new HBox (new Label ("Inside"), new Label ("The World"), new Label ("we swim"));
		root.getChildren ().addAll (hBox);

		Scene rootScene = new Scene (root);
//		GraphXmlBuilder.setNodeFiltering (new Class[]{Canvas.class});
//		GraphXmlBuilder.setLeafNodes (new Class[]{HBox.class});
		Document doc = GraphXmlBuilder.snapshotNode (root, "result.xml");
		primaryStage.setScene (rootScene);
		root.getChildren ().add (GraphXmlToImage.output (doc, "", canvas));
//		root.layoutXProperty ().set (-1000);
//		root.layoutYProperty ().set (-1000);

//		WritableImage image = root.snapshot (null, null);
//		ImageIO.write (SwingFXUtils.fromFXImage (image, null), "png", new File ("image"));
		primaryStage.show ();
	}

}
