package ru.avladimirov.scenegraph;

import javafx.scene.control.Label;

/**
 *
 * @author Vladimirov.A.A
 */
public class Point extends GraphicElement {

	public static final int INSET = 5;

	private Label label;

	public Point (String text) {
		label = new Label (text);
		init ();
		setBindings ();
	}

	private void init () {
		label.setWrapText (true);
		this.setStyle ("	-fx-border-width: 1;\n"
				+ "	-fx-border-color: #000000;");
		this.getChildren ().add (label);
		setBindings ();

	}

	private void setBindings () {
		label.maxWidthProperty ().set (GraphXmlToImage.LEVEL_WIDTH - INSET * 4);
		label.maxHeightProperty ().set (GraphXmlToImage.LEVEL_HEIGHT * 0.6);
		this.maxWidthProperty ().bind (label.maxWidthProperty ()
				.add (INSET * 2));
		this.maxHeightProperty ().bind (label.maxHeightProperty ()
				.add (INSET * 2));
		this.minWidthProperty ().bind (this.maxWidthProperty ());
		this.minHeightProperty ().bind (this.maxHeightProperty ());

		label.layoutXProperty ().bind (this.widthProperty ()
				.subtract (label.widthProperty ())
				.divide (2));
		label.layoutYProperty ().bind (this.heightProperty ()
				.subtract (label.heightProperty ())
				.divide (2));
	}

}
