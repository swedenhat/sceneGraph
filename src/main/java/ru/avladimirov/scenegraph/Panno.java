/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ru.avladimirov.scenegraph;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javafx.scene.layout.Pane;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

/**
 *
 * @author Vladimirov.A.A
 */
public class Panno extends Pane {

	public Panno () {
		super ();

	}

	public void drawGraph (ArrayList<Node> inputLeaves) {
		this.setWidth (GraphXmlToImage.LEVEL_WIDTH * inputLeaves.size ());
		this.setMinHeight (GraphXmlToImage.LEVEL_HEIGHT * 2);
		renderOneLevel (inputLeaves);
	}

	private void renderOneLevel (ArrayList<Node> inputLeaves) {
		Map<Node, GraphicElement> readyComposites = new LinkedHashMap<> ();
		List<Node> readyNodes = new ArrayList<> ();

		Iterator<Node> nodeIter = inputLeaves.iterator ();
		while (nodeIter.hasNext ()) {
			//get next leaf
			Node leaf = nodeIter.next ();
			//check that it was not processed by any previous node
			if (readyNodes.contains (leaf)) {
				continue;
			}
			//get its parent to process all siblings and make a branch
			Node parent = leaf.getParentNode ();

			//check that all siblings are also leafs
			boolean allLeafs = true;
			for (int i = 0; i < parent.getChildNodes ().getLength (); i++) {
				Node node = parent.getChildNodes ().item (i);
				if (node.hasChildNodes ()) {
					allLeafs = false;
					break;
				}
			}

			if (!allLeafs) {
				continue;
			}

			//now process all siblings
			List<GraphicElement> points = new ArrayList<GraphicElement> ();
			for (int i = 0; i < parent.getChildNodes ().getLength (); i++) {
				Node node = parent.getChildNodes ().item (i);
//				if (!readyNodes.contains (node)) {
				NamedNodeMap attributes = node.getAttributes ();
				Node classAttribute = attributes.getNamedItem (GraphXmlBuilder.ATTRIBUTE_CLASS_NAME);
				String className = classAttribute != null ? classAttribute.getNodeValue () : "";
				Point point = new Point (className);
				points.add (point);
				readyNodes.add (node);
//				}
			}

			if (!points.isEmpty ()) {
				NamedNodeMap attributes = parent.getAttributes ();
				Node classAttribute = attributes.getNamedItem (GraphXmlBuilder.ATTRIBUTE_CLASS_NAME);
				String className = classAttribute != null ? classAttribute.getNodeValue () : "";
				Composite branch = new Composite (new Point (className), points);
				this.getChildren ().add (branch);
				readyComposites.put (parent, branch);
			}
		}
	}
}
