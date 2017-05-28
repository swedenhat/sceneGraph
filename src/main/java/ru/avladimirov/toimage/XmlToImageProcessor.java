package ru.avladimirov.toimage;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import ru.avladimirov.scenegraph.SceneGraph;
import ru.avladimirov.scenegraph.SceneGraphException;
import ru.avladimirov.scenegraph.XmlProcessor;

/**
 * An implementation of the {@link XmlProcessor} that outputs the scene graph
 * xml to an image.
 *
 * @author Vladimirov.A.A
 */
public class XmlToImageProcessor implements XmlProcessor {

	public XmlToImageProcessor () {

	}

	/**
	 * {@inheritDoc}.
	 *
	 * @param doc
	 * @param fileName
	 * @return
	 * @throws SceneGraphException
	 */
	@Override
	public GraphicElement output (Document doc) throws SceneGraphException {
		ArrayList<Node> leafs = new ArrayList<> ();
		getAllNodes (doc, leafs);
		Collections.sort (leafs, new ComparatorById ());
		return drawGraph (leafs);
	}

	/**
	 * {@inheritDoc}.
	 *
	 * @param xmlFileName
	 * @param fileName
	 * @return
	 * @throws SceneGraphException
	 */
	@Override
	public GraphicElement output (String xmlFileName) throws SceneGraphException {
		try {
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance ();
			DocumentBuilder builder = factory.newDocumentBuilder ();
			Document doc = builder.parse (new File (xmlFileName));
			return output (doc);
		} catch (ParserConfigurationException | SAXException | IOException | SceneGraphException ex) {
			throw new SceneGraphException (ex);
		}
	}

	/**
	 * Walks the graph and collects all leafs in it. A recursive function.
	 *
	 * @param node that is checked for being a leaf.
	 * @param leafs a global list of leafs for the graph, to which leafs are
	 * added.
	 */
	private void getAllNodes (Node node, ArrayList<Node> leafs) {
		if (node.hasAttributes ()) {
			NamedNodeMap attributes = node.getAttributes ();
			Node leafAttribute = attributes.getNamedItem (SceneGraph.ATTRIBUTE_LEAF);
			String isLeafString = leafAttribute != null ? leafAttribute.getNodeValue () : "";
			boolean isLeaf = Boolean.valueOf (isLeafString);
			if (isLeaf) {
				leafs.add (node);
			}
		}

		if (node.hasChildNodes ()) {
			NodeList list = node.getChildNodes ();
			for (int i = 0; i < list.getLength (); i++) {
				Node child = list.item (i);
				getAllNodes (child, leafs);
			}
		}
	}

	/**
	 * A wrapper for a recursive algorithm that draws the whole graph. Makes
	 * some pre- and post- actions and calls the algorithm.
	 *
	 * @param inputLeaves
	 * @return
	 * @throws SceneGraphException
	 */
	private GraphicElement drawGraph (ArrayList<Node> inputLeaves) throws SceneGraphException {
		//at first we form input conditions: make gui elements for each existent leaf.
		Map<Node, GraphicElement> nodeToGraphic = new LinkedHashMap<> ();
		for (Node leaf : inputLeaves) {
			NamedNodeMap attributes = leaf.getAttributes ();
			Node classAttribute = attributes.getNamedItem (SceneGraph.ATTRIBUTE_CLASS_NAME);
			String className = classAttribute != null ? classAttribute.getNodeValue () : "";
			Berry point = new Berry (className);
			Node levelAttribute = attributes.getNamedItem (SceneGraph.ATTRIBUTE_LEVEL);
			String level = levelAttribute != null ? levelAttribute.getNodeValue () : "";
			//add a special css id, depending on the deepness level.
			point.setId ("berry" + level);
			nodeToGraphic.put (leaf, point);
		}

		//then we start a recursive algo.
		while (nodeToGraphic.size () > 1) {
			renderOneLevel (nodeToGraphic);
		}

		//at the end we should have the only one graphic element, that contains the shole tree.
		if (nodeToGraphic.size () > 1) {
			throw new SceneGraphException ("An error occured. The image wasn't fully processed to the top element.");
		}
		for (Map.Entry<Node, GraphicElement> entry : nodeToGraphic.entrySet ()) {
			return entry.getValue ();
		}
		throw new SceneGraphException ("An error occured. Something went wrong and the xml wasn't processed.");
	}

	/**
	 * Processes the one level of the graph. The algorithm is as follows: <br>
	 * 1. We take the existent map of leaves with their graphic elements. <br>
	 * 2. For each leaf, all siblings of which are also a leaves, we form a
	 * {@link Bunch} of them and replace them in the map with their bunch. Thus
	 * we complete one level of deepness. <br>
	 * 3. Now the bunches in the map are treated as leaves for the next level.
	 * We can process the algo from the beginning.
	 *
	 * @param nodeToGraphic the map containing leaves to their graphic elements.
	 */
	private void renderOneLevel (Map<Node, GraphicElement> nodeToGraphic) {
		Map<Node, GraphicElement> readyComposites = new LinkedHashMap<> ();

		List<Node> finishedNodes = new ArrayList<> ();

		Iterator<Node> nodeIter = nodeToGraphic.keySet ().iterator ();
		while (nodeIter.hasNext ()) {
			//get next leaf
			Node leaf = nodeIter.next ();
			//check that it was not processed by any previous node
			if (finishedNodes.contains (leaf)) {
				continue;
			}
			//get its parent to process all siblings and make a branch
			Node parent = leaf.getParentNode ();

			//check that all siblings are also leafs
			boolean allLeafsAreReady = true;
			for (int i = 0; i < parent.getChildNodes ().getLength (); i++) {
				Node node = parent.getChildNodes ().item (i);
				if (!nodeToGraphic.containsKey (node)) {
					allLeafsAreReady = false;
					break;
				}
			}

			if (!allLeafsAreReady) {
				continue;
			}

			//now process all siblings
			List<GraphicElement> graphics = new ArrayList<GraphicElement> ();
			for (int i = 0; i < parent.getChildNodes ().getLength (); i++) {
				Node node = parent.getChildNodes ().item (i);
				graphics.add (nodeToGraphic.get (node));
				finishedNodes.add (node);
			}

			if (!graphics.isEmpty ()) {
				NamedNodeMap attributes = parent.getAttributes ();
				Node classAttribute = attributes.getNamedItem (SceneGraph.ATTRIBUTE_CLASS_NAME);
				String className = classAttribute != null ? classAttribute.getNodeValue () : "";
				Bunch bunch = new Bunch (new Berry (className), graphics);
				Node levelAttribute = attributes.getNamedItem (SceneGraph.ATTRIBUTE_LEVEL);
				String level = levelAttribute != null ? levelAttribute.getNodeValue () : "";
				//add a special css id, depending on the deepness level.
				bunch.setId ("bunch" + level);
				readyComposites.put (parent, bunch);
			}
		}

		for (Node finished : finishedNodes) {
			nodeToGraphic.remove (finished);
		}
		for (Map.Entry<Node, GraphicElement> entry : readyComposites.entrySet ()) {
			nodeToGraphic.put (entry.getKey (), entry.getValue ());
		}
	}

	/**
	 * A comparator for the nodes. Though there is no need to sort leaves before
	 * drawing, I prefer sorting. This makes it all more ordered.
	 */
	private static class ComparatorById implements Comparator<Node> {

		@Override
		public int compare (Node o1, Node o2) {
			String id1 = o1.getAttributes ().getNamedItem (SceneGraph.ATTRIBUTE_ID).getNodeValue ();
			String id2 = o2.getAttributes ().getNamedItem (SceneGraph.ATTRIBUTE_ID).getNodeValue ();
			int result = (new Integer (id1)).compareTo (new Integer (id2));
			return result;
		}
	}
}
