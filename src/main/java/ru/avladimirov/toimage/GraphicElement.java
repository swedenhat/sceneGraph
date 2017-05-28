package ru.avladimirov.toimage;

import javafx.scene.layout.Pane;

/**
 * A class that is a common ancestor for two others: {@link Berry} and
 * {@link Bunch}. That's its one and only function.
 *
 * @author Vladimirov.A.A
 */
public abstract class GraphicElement extends Pane {

	public static final int LEVEL_MIN_HEIGHT = 40;
	public static final int LEVEL_MAX_HEIGHT = 50;
	public static final int LEVEL_MIN_WIDTH = 40;
	public static final int LEVEL_MAX_WIDTH = 150;
	public static final int INSET_INSIDE_BERRY = 10;

}
