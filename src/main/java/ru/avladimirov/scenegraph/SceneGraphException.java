package ru.avladimirov.scenegraph;

/**
 * A common exception for any errors that may occur in this framework during its
 * work.
 *
 * @author Vladimirov.A.A
 */
public class SceneGraphException extends Exception {

	public SceneGraphException () {
	}

	public SceneGraphException (String message) {
		super (message);
	}

	public SceneGraphException (String message, Throwable cause) {
		super (message, cause);
	}

	public SceneGraphException (Throwable cause) {
		super (cause);
	}

}
