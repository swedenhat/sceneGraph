package ru.avladimirov.scenegraph;

/**
 * An util class for some inner operations.
 *
 * @author Vladimirov.A.A
 */
public class Utils {

	private Utils () {
	}

	/**
	 * Checks if the passed array of classes contains the source class.
	 *
	 * @param source a class to check
	 * @param classesToCompare an array where to check.
	 * @return true if the class is contained in the array, false otherwise of
	 * if the array is null.
	 */
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

	/**
	 * No necessary now.
	 *
	 * @param rad PI or two PI
	 * @return the same but in signed angles.
	 */
	public static double getAngleFromRad (double rad) {
		return 180 * rad / Math.PI;
	}

}
