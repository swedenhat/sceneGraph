package ru.avladimirov.toimage;

import java.io.File;
import javafx.application.Application;
import javafx.embed.swing.SwingFXUtils;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.Label;
import javafx.scene.image.WritableImage;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import javax.imageio.ImageIO;
import org.w3c.dom.Document;
import ru.avladimirov.scenegraph.SceneGraph;

/**
 * A test class that shows how this all works.
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

		HBox hBoxOne = new HBox (new Label ("Inside"), new Label ("The World"), new Label ("we swim"));
		HBox hBoxTwo = new HBox (new Label ("The World"), new Label ("we swim"));
		hBoxOne.getChildren ().add (hBoxTwo);
		root.getChildren ().addAll (hBoxOne);
		root.getChildren ().add (new Label ("Merci"));

		Scene rootScene = new Scene (root);
		String styleSheet = this.getClass ().getResource ("style.css").toExternalForm ();
		rootScene.getStylesheets ().add (styleSheet);
//		SceneGraph.setNodeFiltering (new Class[]{Canvas.class});
//		SceneGraph.setLeafNodes (new Class[]{HBox.class});
		Document doc = SceneGraph.convertToXml (root, "result.xml");
		primaryStage.setScene (rootScene);
		root.getChildren ().add (new XmlToImageProcessor ().output ("result.xml"));
		WritableImage image = root.snapshot (null, null);
		ImageIO.write (SwingFXUtils.fromFXImage (image, null), "png", new File ("result.png"));

		primaryStage.show ();
	}
}
