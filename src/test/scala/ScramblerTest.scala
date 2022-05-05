package com.chrisworks

import service.Scrambler

//Since we don't have test libs here, we will use the regular asserts
object ScramblerTest extends App {

  Scrambler.liveScrambler.scramble(TestUtils.sampleFilePath)
}
