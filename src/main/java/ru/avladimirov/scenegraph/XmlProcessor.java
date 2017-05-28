package ru.avladimirov.scenegraph;

import javafx.scene.layout.Pane;
import org.w3c.dom.Document;

/**
 * All processors that work with scene graph xmls should implement this
 * interface. The idea is that there are different ways to transform the xml in
 * something visually comprehensive, so you may 'plug in' different processors.
 *
 * @author Vladimirov.A.A
 */
public interface XmlProcessor {

	/**
	 * Processes the xml and saves the result to a file.
	 *
	 * @param doc a {@link org.w3c.dom.Document} that is a build from the scene
	 * graph xml.
	 * @param fileName of the file where the ouput will be saved.
	 * @return a {@link javafx.scene.layout.Pane} with the ouput that can be
	 * included into your application.
	 * @throws SceneGraphException in case of any exception you need to throw
	 */
	public Pane output (Document doc) throws SceneGraphException;

	/**
	 * Reads the xml from the file, processes it and saves the result to a file.
	 *
	 * @param xmlFileName of an xml file with a scene graph.
	 * @param fileName of the file where the ouput will be saved.
	 * @return a {@link javafx.scene.layout.Pane} with the ouput that can be
	 * included into your application.
	 * @throws SceneGraphException in case of any exception you need to throw
	 */
	public Pane output (String xmlFileName) throws SceneGraphException;

}
