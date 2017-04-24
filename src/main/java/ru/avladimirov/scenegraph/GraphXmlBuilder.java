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
 *
 * @author Vladimirov.A.A
 */
public class GraphXmlBuilder {

	static final String ROOT_TAG = "root";
	static final String CHILD_TAG = "child";
	static final String ATTRIBUTE_CLASS_NAME = "class";
	static final String ATTRIBUTE_LEVEL = "level";
	static final String ATTRIBUTE_LEAF = "leaf";
	static final String ATTRIBUTE_ID = "id";

	private static Class[] filterNodes = null;
	private static Class[] leafNodes = null;

	private static int id = 0;

	private GraphXmlBuilder () {
	}

	public static void setLeafNodes (Class[] leafs) {
		leafNodes = leafs;
	}

	public static void setNodeFiltering (Class[] filtered) {
		filterNodes = filtered;
	}

	/**
	 * Makes an xml document, containing the scene graph beginning from the
	 * specified node as an a xml view. Each branch is a gui class that has
	 * children (like some HBox, containing some Labels), and each leaf is a gui
	 * class that either don't have children or can't have them (like a Label).
	 * It saves it to file.
	 *
	 * @param root
	 * @throws ParserConfigurationException
	 * @throws TransformerException
	 * @throws IOException
	 */
	public static Document snapshotNode (Parent root, String whereToSave) throws ParserConfigurationException, TransformerException, IOException {
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance ();
		DocumentBuilder builder = dbf.newDocumentBuilder ();
		DOMImplementation impl = builder.getDOMImplementation ();
		Document doc = impl.createDocument (null, null, null);

		//recursively build a document of all children of all the nodes
		makeElement (doc, doc, root, 0);
		id = 0;
		//if the file nams is incorrect, don't save anywhere
		if (whereToSave == null || whereToSave.isEmpty ()) {
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
		File file = new File (whereToSave);
		Files.write (file.toPath (), bytes, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
		return doc;
	}

	private static void makeElement (Document doc, org.w3c.dom.Node parentElement, Node node, int level) {
		String tag = "";
		if (level == 0) {
			tag = ROOT_TAG;
		} else {
			tag = CHILD_TAG;
		}

		//if the node is marked to be filtered out, we don't add it to the doc
		if (Utils.checkContainment (node.getClass (), filterNodes)) {
			return;
		}
		Element currentElement = doc.createElement (tag);
		currentElement.setAttribute (ATTRIBUTE_CLASS_NAME, node.getClass ().getSimpleName ());
		currentElement.setAttribute (ATTRIBUTE_ID, ++id + "");
		currentElement.setAttribute (ATTRIBUTE_LEVEL, level + "");
		if (node instanceof Parent) {
			Parent parent = (Parent) node;
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
