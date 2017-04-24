/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ru.avladimirov.scenegraph;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;

/**
 *
 * @author Vladimirov.A.A
 */
public class ResizableCanvas extends Canvas {

	ResizableCanvas (int i, int i0) {
		super (i, i0);
		widthProperty ().addListener (evt -> draw ());
		heightProperty ().addListener (evt -> draw ());
	}

	ResizableCanvas () {
		super ();
		widthProperty ().addListener (evt -> draw ());
		heightProperty ().addListener (evt -> draw ());
	}

	private void draw () {
		double width = getWidth ();
		double height = getHeight ();
		GraphicsContext gc = getGraphicsContext2D ();
		gc.clearRect (0, 0, width, height);
	}

	@Override
	public boolean isResizable () {
		return true;
	}

}
