/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ru.avladimirov.scenegraph;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import javafx.scene.canvas.Canvas;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 *
 * @author Vladimirov.A.A
 */
public class GraphXmlToImage {

	public static final int LEVEL_HEIGHT = 120;
	public static final int LEVEL_WIDTH = 100;
	private static int maxLevel = 0;

	private GraphXmlToImage () {
	}

	public static Panno output (Document doc, String fileName, Canvas canvas) {
		ArrayList<Node> leafs = new ArrayList<> ();
		LinkedHashMap<Node, Boolean> allNodes = new LinkedHashMap<> ();
		getAllNodes (doc, leafs, allNodes);
		Collections.sort (leafs, new ComparatorById ());
		System.out.println ("level=" + maxLevel);
		Panno panno = new Panno ();
		panno.drawGraph (leafs);
		return panno;
//		drawNameInRect (canvas, "GetYourPrize and be such a cool guy");
	}

	private static void getAllNodes (Node node, ArrayList<Node> leafs, LinkedHashMap<Node, Boolean> allNodes) {
		if (node.hasAttributes ()) {
			NamedNodeMap attributes = node.getAttributes ();
			Node idAttribute = attributes.getNamedItem (GraphXmlBuilder.ATTRIBUTE_ID);
			String id = idAttribute != null ? idAttribute.getNodeValue () : "";
			Node leafAttribute = attributes.getNamedItem (GraphXmlBuilder.ATTRIBUTE_LEAF);
			String isLeafString = leafAttribute != null ? leafAttribute.getNodeValue () : "";
			Node levelAttribute = attributes.getNamedItem (GraphXmlBuilder.ATTRIBUTE_LEVEL);
			String levelString = levelAttribute != null ? levelAttribute.getNodeValue () : "";
			boolean isLeaf = Boolean.valueOf (isLeafString);
			if (isLeaf) {
				leafs.add (node);
			}
			allNodes.put (node, false);
			int level = new Integer (levelString);
			if (maxLevel < level) {
				maxLevel = level;
			}
		}

		if (node.hasChildNodes ()) {
			NodeList list = node.getChildNodes ();
			for (int i = 0; i < list.getLength (); i++) {
				Node child = list.item (i);
				getAllNodes (child, leafs, allNodes);
			}
		}
	}

	private static class ComparatorById implements Comparator<Node> {

		@Override
		public int compare (Node o1, Node o2) {
			String id1 = o1.getAttributes ().getNamedItem (GraphXmlBuilder.ATTRIBUTE_ID).getNodeValue ();
			String id2 = o2.getAttributes ().getNamedItem (GraphXmlBuilder.ATTRIBUTE_ID).getNodeValue ();
			int result = (new Integer (id1)).compareTo (new Integer (id2));
			return result;
		}
	}
//
//	private static void drawNameInRect (Canvas canvas, String text) {
//		GraphicsContext context = canvas.getGraphicsContext2D ();
//		context.setLineWidth (0.5);
//		context.setFill (Color.BLUEVIOLET);
//		context.fillText (text, 50, 50, 200);
//		context.strokeRoundRect (50, 35, 200, 50, 10, 10);
//	}
}
