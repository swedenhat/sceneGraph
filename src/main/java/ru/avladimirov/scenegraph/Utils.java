/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ru.avladimirov.scenegraph;

/**
 *
 * @author Vladimirov.A.A
 */
public class Utils {

	private Utils () {
	}

	public static boolean checkContainment (String source, String[] stringsToCompare) {
		if (stringsToCompare == null) {
			return false;
		}
		for (String string : stringsToCompare) {
			if (string.equals (source)) {
				return true;
			}
		}
		return false;
	}

	public static boolean checkContainment (Class source, Class[] classesToCompare) {
		if (classesToCompare == null) {
			return false;
		}
		for (Class clazz : classesToCompare) {
			if (clazz.getSimpleName ().equals (source.getSimpleName ())) {
				return true;
			}
		}
		return false;
	}

}
