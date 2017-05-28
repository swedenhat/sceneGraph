package ru.avladimirov.scenegraph;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import javafx.scene.Node;
import javafx.scene.Parent;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Walks the scene graph in a javafx application and outputs it to an xml. The
 * class is made static, so that you can call its methods from any point in your
 * program.
 *
 * @author Vladimirov.A.A
 */
public class SceneGraph {

	public static final String CHILD_TAG = "child";
	public static final String ATTRIBUTE_CLASS_NAME = "class";
	public static final String ATTRIBUTE_LEVEL = "level";
	public static final String ATTRIBUTE_LEAF = "leaf";
	public static final String ATTRIBUTE_ID = "id";

	private static Class[] filteredNodes = null;
	private static Class[] leafNodes = null;
	private static int id = 0;

	private SceneGraph () {
	}

	/**
	 * Sets javafx classes that should be treated as leaves. That's useful when
	 * you don't need to go too deep into your scene graph. For example, a
	 * {@link javafx.scene.control.Label} contains a child {@code LabeledText}.
	 * If you don't need it to be registered in your xml, you add the Label
	 * class as a leaf here.
	 *
	 * @param leafs an array of classes that you want to be treated as leaves.
	 */
	public static void setLeafNodes (Class[] leafs) {
		leafNodes = leafs;
	}

	public static Class[] getLeafNodes () {
		return leafNodes;
	}

	/**
	 * Sets javafx classes that should be filtered out from your graph. Add here
	 * classes that you don't want to be registered in the xml. For example, if
	 * you add a {@link javafx.scene.control.Label} here, there will be no
	 * labels in the output document, as if there were no labels in the graph
	 * itself.
	 *
	 * @param filtered an array of classes that you want to be filtered out.
	 */
	public static void setFilteredNodes (Class[] filtered) {
		filteredNodes = filtered;
	}

	public static Class[] getFilteredNodes () {
		return filteredNodes;
	}

	/**
	 * Makes an xml document, containing the scene graph beginning from the
	 * specified node. Each branch is a gui class that has children (like some
	 * {@link javafx.scene.layout.HBox}, containing some
	 * {@link javafx.scene.control.Label}s), and each leaf is a gui class that
	 * either don't have children or can't have them (like a
	 * {@code LabeledText}). The document is then saved to a file.
	 *
	 * @param root a javafx node from which the graph should be builded. It will
	 * be the father node: only its children will be added to the xml.
	 * @param fileName the name of the file where the resulting xml is saved. If
	 * it is null or empty, the xml won't be saved.
	 * @return a {@link org.w3c.dom.Document}, representing the structure of the
	 * scene graph.
	 * @throws ru.avladimirov.scenegraph.SceneGraphException rethrowes possible
	 * inner exceptions:
	 * {@link ParserConfigurationException}, {@link TransformerException}, {@link IOException}
	 */
	public static Document convertToXml (Parent root, String fileName) throws SceneGraphException {
		try {
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance ();
			DocumentBuilder builder = dbf.newDocumentBuilder ();
			DOMImplementation impl = builder.getDOMImplementation ();
			Document doc = impl.createDocument (null, null, null);

			//recursively build a document of all children of all the nodes
			makeElement (doc, doc, root, 0);
			id = 0;
			//if the file name is incorrect, don't save anywhere
			if (fileName == null || fileName.isEmpty ()) {
				return doc;
			}

			TransformerFactory tf = TransformerFactory.newInstance ();
			Transformer transformer;
			transformer = tf.newTransformer ();
			transformer.setOutputProperty (OutputKeys.ENCODING, "UTF-8");

			DOMSource source = new DOMSource (doc);
			ByteArrayOutputStream baos = new ByteArrayOutputStream ();
			StreamResult streamResult = new StreamResult (baos);

			transformer.transform (source, streamResult);

			byte[] bytes = baos.toByteArray ();
			File file = new File (fileName);
			Files.write (file.toPath (), bytes, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
			return doc;
		} catch (ParserConfigurationException | TransformerException | IOException ex) {
			throw new SceneGraphException (ex);
		}
	}

	/**
	 * A recursive function that walks the scene graph and builds a
	 * {@link org.w3c.dom.Document} based on it.
	 *
	 * @param doc a document, to which new elements are added.
	 * @param parentElement a parent element, to which this current element,
	 * created by this function, will be appended.
	 * @param sceneNode a javafx class, for which this function makes the
	 * current element.
	 * @param level the depth level of the current element from the point of the
	 * root element.
	 */
	private static void makeElement (Document doc, org.w3c.dom.Node parentElement, Node sceneNode, int level) {
		//if the node is marked to be filtered out, we don't add it to the doc
		if (Utils.checkContainment (sceneNode.getClass (), filteredNodes)) {
			return;
		}
		//make an element for the node
		Element currentElement = doc.createElement (CHILD_TAG);
		currentElement.setAttribute (ATTRIBUTE_CLASS_NAME, sceneNode.getClass ().getSimpleName ());
		currentElement.setAttribute (ATTRIBUTE_ID, ++id + "");
		currentElement.setAttribute (ATTRIBUTE_LEVEL, level + "");
		//only Parents may have children. But some not-parent elements are also Nodes.
		if (sceneNode instanceof Parent) {
			Parent parent = (Parent) sceneNode;
			//Will it be a leaf?
			if (parent.getChildrenUnmodifiable ().isEmpty ()
					//or if the node is set to be a leaf node by user
					|| Utils.checkContainment (parent.getClass (), leafNodes)) {
				currentElement.setAttribute (ATTRIBUTE_LEAF, Boolean.TRUE.toString ());
				parentElement.appendChild (currentElement);
			} //It's not a leaf. Add it to doc and call this func recursively again.
			else {
				currentElement.setAttribute (ATTRIBUTE_LEAF, Boolean.FALSE.toString ());
				parentElement.appendChild (currentElement);
				level++;
				for (Node child : parent.getChildrenUnmodifiable ()) {
					makeElement (doc, currentElement, child, level);
				}
			}
		} //if node is not a Parent, it will be a leaf node
		else {
			currentElement.setAttribute (ATTRIBUTE_LEAF, Boolean.TRUE.toString ());
			parentElement.appendChild (currentElement);
		}
	}
}
