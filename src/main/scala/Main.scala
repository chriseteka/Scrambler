package com.chrisworks

import service.Scrambler
import util.FileUtil.openFileChooser

/**
 * Starting this will open a fileChooser for you to select a JSON file.
 * If it is a valid JSON, keys ending in id/name will be scrambled with some consistent data
 */
object Main extends App {

  openFileChooser().foreach(Scrambler.liveScrambler.scramble)

}
