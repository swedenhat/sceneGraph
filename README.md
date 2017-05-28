sceneGraph
==========

An utility for drawing a JavaFX scene graph of your JavaFX application.

## Structure

Has two parts:

* Graph-to-xml converter. Outputs the scene graph into an xml document.
* Xml-to-image processor. Reads the xml and draws an image of the scene graph, then saves it to a file.

## How to use

You can use the first part anywhere and not once inside your application:

1. (Optional) Specify classes, you want to exclude from your graph:
```java
SceneGraph.setFilteredNodes (new Class[]{Canvas.class, Separator.class, VBox.class});
```

2. (Optional) Specify classes, deeper than which you don't want to go. For example,
you have a LabeledText class as a child of a Label class. Or, you have some children classes inside ComboBox.
If you don't want to show them, specify Label and ComboBox as leaf nodes:
```java
SceneGraph.setLeafNodes (new Class[]{Label.class, ComboBox.class});
```

3. Convert the graph to an xml document. You will have it as a return value (if you want to make an image immediately)
and also saved to disk as 'result.xml'. Note that you can work not only with the whole graph, but also with its parts,
depending on what node you specify in the function. The node 'root' used here is the root node of the scene. 
```java
Document doc = SceneGraph.convertToXml (root, "result.xml");
```

4. Now make a separate JavaFX application and process your xml to an image.
You need to do this in another app, because the resulting image is made using JavaFX layouts and controls, and has to be snapshoted to be saved.
Thus it has to be added to the scene. You are not willing to do this during your application runtime, so better do it after and separately.
```java
Pane root = new Pane ();
GraphicElement xmlToImage = new XmlToImageProcessor ().output ("result.xml");
root.getChildren ().add (xmlToImage);
Scene rootScene = new Scene (root);
primaryStage.setScene (rootScene);
WritableImage image = root.snapshot (null, null);
ImageIO.write (SwingFXUtils.fromFXImage (image, null), "png", new File ("image"));
```

## CSS styling

The two base classes that can be styled are: Berry and Bunch.

Berries and Bunches of each level can be styled separately, the have ids like 'berry'+[level] and 'bunch'+[level].
Levels are counted from 0 (the topmost node). So, if you want to style separately all berries of the second level, use #berry2 css name. 

Example:
```css
Berry {
	-fx-border-width: 1;
	-fx-border-radius: 5;
	-fx-border-color: #559955;
	-fx-background-color: rgba(255, 255 ,255 , 2);
	-fx-background-radius: 5;
}

Bunch {
	-fx-background-color: rgba(200, 200 ,200 , 1);
}

#berry1 {
	-fx-background-color: rgba(150, 150 ,255 , 1);
}

#berry2 {
	-fx-background-color: rgba(233, 100 ,100 , 1);
}

#bunch1,
#bunch2 {
	-fx-padding: 10; 
	-fx-border-width: 1;
	-fx-border-color: #009900;
	-fx-border-radius: 5;
}
```

## Dependencies

No dependencies.

