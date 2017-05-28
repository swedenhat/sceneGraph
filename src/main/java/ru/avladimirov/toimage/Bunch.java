package ru.avladimirov.toimage;

import java.util.List;
import javafx.beans.property.DoubleProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.value.ObservableValue;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

/**
 * A gui element, a bunch of {@link Berry}-ies. Represents an xml element (node)
 * with children.
 *
 * @author Vladimirov.A.A
 */
public class Bunch extends GraphicElement {

	private VBox holder = new VBox (30); //holds the head and its children
	private HBox childrenBox = new HBox (10); //holds all children
	private GraphicElement head;
	private Canvas canvas; //for drawing links between graphs. Is placed directly to this pane.

	/**
	 *
	 * @param head a parent node that has children. The head of this
	 * one-generation mini-graph.
	 * @param children of the 'head' parent.
	 */
	public Bunch (GraphicElement head, List<GraphicElement> children) {
		this.head = head;
		childrenBox.getChildren ().addAll (children);
		childrenBox.setAlignment (Pos.TOP_CENTER);
		childrenBox.setFillHeight (false);
		holder.getChildren ().addAll (head, childrenBox);
		holder.setAlignment (Pos.CENTER);
		holder.setFillWidth (false);

		this.getChildren ().add (holder);

		//position it centrally
		holder.layoutXProperty ().bind (this.widthProperty ().subtract (holder.widthProperty ()).divide (2));
		holder.layoutYProperty ().bind (this.heightProperty ().subtract (holder.heightProperty ()).divide (2));

		//Trigger listens for all geometric (x, y, width, height) changes of all elements in the pane.
		//When trigger changes you should redraw the links between elements.
		SimpleBooleanProperty trigger = new SimpleBooleanProperty (false);
		addListeners (trigger);
		trigger.addListener ((ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) -> {
			//Redraw only if it's meaningful: when the pane has real size.
			if (Bunch.this.widthProperty ().get () > 0 && Bunch.this.heightProperty ().get () > 0) {
				//remove the previous version of the canvas
				if (Bunch.this.getChildren ().contains (canvas)) {
					Bunch.this.getChildren ().remove (canvas);
				}
				//and draw new canvas with new links
				canvas = new Canvas (Bunch.this.widthProperty ().get (), Bunch.this.heightProperty ().get ());
				Bunch.this.getChildren ().add (canvas);
				//position it centrally
				canvas.layoutXProperty ().bind (this.widthProperty ().subtract (holder.widthProperty ()).divide (2));
				canvas.layoutYProperty ().bind (this.heightProperty ().subtract (holder.heightProperty ()).divide (2));
				canvas.toBack ();
				for (Node node : children) {
					GraphicElement ele = (GraphicElement) node;
					if (canvas != null) {
						double toX = getAbsoluteX (ele).doubleValue ();
						double toY = getAbsoluteY (ele).doubleValue ();
						double fromX = head.layoutXProperty ().get () + head.widthProperty ().get () / 2;
						double fromY = head.layoutYProperty ().get () + head.heightProperty ().get ();
						GraphicsContext gc = canvas.getGraphicsContext2D ();
						gc.strokeLine (fromX, fromY, toX, toY);
					}
				}
			}
		});
	}

	/**
	 * Gets an absolute layout x position of the element in terms of the holding
	 * pane. So, the class that holds the element is {@link Bunch}, and this
	 * function gets the absolute X position of the element, that is added to
	 * the Bunch through some mediate panes (like HBox, VBox).
	 *
	 * @param element which x is to be tracked.
	 * @return a property that can be listened.
	 */
	private SimpleDoubleProperty getAbsoluteX (GraphicElement element) {
		DoubleProperty eleX = element.layoutXProperty ();
		DoubleProperty eleXbox = element.getParent ().layoutXProperty ();
		SimpleDoubleProperty absoluteX = new SimpleDoubleProperty ();
		absoluteX.bind (eleX
				.add (eleXbox)
				.add (element.widthProperty ()
						.divide (2)));
		return absoluteX;
	}

	/**
	 * Gets an absolute layout y position of the element in terms of the holding
	 * pane. So, the class that holds the element is {@link Bunch}, and this
	 * function gets the absolute Y position of the element, that is added to
	 * the Bunch through some mediate panes (like HBox, VBox).
	 *
	 * @param element which y is to be tracked.
	 * @return a property that can be listened.
	 */
	private SimpleDoubleProperty getAbsoluteY (GraphicElement element) {
		DoubleProperty eleY = element.layoutYProperty ();
		DoubleProperty eleYbox = element.getParent ().layoutYProperty ();
		SimpleDoubleProperty absoluteY = new SimpleDoubleProperty ();
		absoluteY.bind (eleY
				.add (eleYbox));
		return absoluteY;
	}

	/**
	 * Adds listeners to any geometric properties that can be changed during
	 * this pane's creation. All changes in listeners are collected by one
	 * trigger, that rules them all.
	 *
	 * @param trigger a simple boolean property that tracks changes in all other
	 * geometric properties.
	 */
	private void addListeners (SimpleBooleanProperty trigger) {
		this.visibleProperty ().addListener ((ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) -> {
			trigger.setValue (!trigger.get ());
		});
		this.widthProperty ().addListener ((ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> {
			trigger.setValue (!trigger.get ());
		});
		this.heightProperty ().addListener ((ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> {
			trigger.setValue (!trigger.get ());
		});
		head.layoutXProperty ().addListener ((ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> {
			trigger.setValue (!trigger.get ());
		});
		head.layoutYProperty ().addListener ((ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> {
			trigger.setValue (!trigger.get ());
		});
		holder.layoutXProperty ().addListener ((ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> {
			trigger.setValue (!trigger.get ());
		});
		holder.layoutYProperty ().addListener ((ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> {
			trigger.setValue (!trigger.get ());
		});
		for (Node node : childrenBox.getChildren ()) {
			GraphicElement ele = (GraphicElement) node;
			getAbsoluteX (ele).addListener ((ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> {
				trigger.setValue (!trigger.get ());
			});
			getAbsoluteY (ele).addListener ((ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> {
				trigger.setValue (!trigger.get ());
			});
			ele.layoutXProperty ().addListener ((ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> {
				trigger.setValue (!trigger.get ());
			});
			ele.layoutYProperty ().addListener ((ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> {
				trigger.setValue (!trigger.get ());
			});
			ele.widthProperty ().addListener ((ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> {
				trigger.setValue (!trigger.get ());
			});
			ele.heightProperty ().addListener ((ObservableValue<? extends Number> observable, Number oldValue, Number newValue) -> {
				trigger.setValue (!trigger.get ());
			});
		}
	}
}
