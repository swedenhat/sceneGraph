package ru.avladimirov.toimage;

import javafx.scene.control.Label;

/**
 * A javafx element, a visual representation of a leaf. It is called 'Berry' to
 * distinguish from xml leaves and any other 'nodes'. Berries with one common
 * parent form a {@link Bunch}. And each of them is suprisingly a
 * {@link GraphicElement}.
 *
 * @author Vladimirov.A.A
 */
public class Berry extends GraphicElement {

	private Label label;

	/**
	 *
	 * @param text leaf's name
	 */
	public Berry (String text) {
		label = new Label (text);
		init ();
		setBindings ();
	}

	/**
	 * Makes all necessary initializations and configurations for the children
	 * UI elements in this class.
	 */
	private void init () {
		label.setWrapText (true);
		this.getChildren ().add (label);
		setBindings ();
	}

	/**
	 * Sets all necessary layout bindings between ui elements in this class and
	 * for the class itself (width, height, etc).
	 */
	private void setBindings () {
		label.maxWidthProperty ().set (LEVEL_MAX_WIDTH);
		label.minWidthProperty ().set (LEVEL_MIN_WIDTH);
		label.maxHeightProperty ().set (LEVEL_MAX_HEIGHT);
		label.minHeightProperty ().set (LEVEL_MIN_HEIGHT);
		this.maxWidthProperty ().bind (label.maxWidthProperty ()
				.add (INSET_INSIDE_BERRY * 2));
		this.maxHeightProperty ().bind (label.maxHeightProperty ()
				.add (INSET_INSIDE_BERRY * 2));
		this.minWidthProperty ().bind (label.minWidthProperty ()
				.add (INSET_INSIDE_BERRY * 2));
		this.minHeightProperty ().bind (label.minHeightProperty ()
				.add (INSET_INSIDE_BERRY * 2));

		label.layoutXProperty ().bind (this.widthProperty ()
				.subtract (label.widthProperty ())
				.divide (2));
		label.layoutYProperty ().bind (this.heightProperty ()
				.subtract (label.heightProperty ())
				.divide (2));
	}
}
