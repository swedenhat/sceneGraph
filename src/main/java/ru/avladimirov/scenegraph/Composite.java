/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ru.avladimirov.scenegraph;

import java.util.List;
import javafx.geometry.Pos;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

/**
 *
 * @author Vladimirov.A.A
 */
public class Composite extends GraphicElement {

	private VBox holder = new VBox (30);
	private HBox childrenBox = new HBox (10);
	private GraphicElement head;

	public Composite (GraphicElement head, List<GraphicElement> children) {
		this.head = head;
		childrenBox.getChildren ().addAll (children);
		holder.getChildren ().addAll (head, childrenBox);
		holder.setAlignment (Pos.CENTER);
		this.getChildren ().add (holder);
	}
}
